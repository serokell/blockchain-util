{-# LANGUAGE DataKinds           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.AVLp.Accum
       ( AVLChgAccum (..)
       , AVLChgAccums
       , RootHashComp (..)
       , RootHashes
       , AllAvlEntries
       , modAccum
       , query
       , iter
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Tree.AVL (MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Recursive (rmap)

import           Snowdrop.Core (AvlRevision (..), CSMappendException (..), ChgAccumModifier (..),
                                Undo (..), ValueOp (..), hChangeSetElToList)

import           Snowdrop.Execution.DbActions.AVLp.Avl (AvlHashable, KVConstraint, RootHash (..),
                                                        avlRootHash, mkAVL)
import           Snowdrop.Execution.DbActions.AVLp.State (AVLCache, AVLCacheT, AllAvlEntries,
                                                          IsAvlEntry, RetrieveImpl,
                                                          RootHashComp (..), RootHashes,
                                                          reThrowAVLEx, runAVLCacheT)
import           Snowdrop.Execution.DbActions.Types (DGetter, DIter, DIter', DModify,
                                                     DbActionsException (..), IterAction (..))
import           Snowdrop.Util


-- | Change accumulator type for AVL tree.
data AVLChgAccum h t = AVLChgAccum
    { acaMap     :: AVL.Map h (HKey t) (HVal t)
    -- ^ AVL map, which contains avl tree with most-recent updates
    , acaStorage :: AVLCache h
    -- ^ AVL tree cache, which stores results of all `save` operations performed on AVL tree
    , acaTouched :: Set h
    -- ^ Set of nodes, which were touched during all of change operations applied on tree
    }

-- | Change accumulator type for AVL tree, wrapped with Maybe.
-- `Nothing` is treated identically to `Just $ AVLChgAccum (Pure rootHash) def mempty`,
-- where `rootHash` is root hash of current state
type AVLChgAccums h xs = Maybe (Rec (AVLChgAccum h) xs)

resolveAvlCA
    :: AvlHashable h
    => HasGetter state (RootHashes h xs)
    => state
    -> AVLChgAccums h xs
    -> Rec (AVLChgAccum h) xs
resolveAvlCA _ (Just cA) = cA
resolveAvlCA st Nothing = rmap crAVLChgAccum (gett st)
  where
    crAVLChgAccum (RootHashComp rh) = AVLChgAccum
      { acaMap = pure $ unRootHash rh
      , acaStorage = mempty
      , acaTouched = mempty
      }

modAccum
    :: forall h xs ctx m .
    ( AvlHashable h, HasGetter ctx (RootHashes h xs)
    , RetrieveImpl (ReaderT ctx m) h
    , MonadIO m, MonadCatch m
    , AllAvlEntries h xs
    )
    => ctx
    -> DModify (AVLChgAccums h xs) xs m
modAccum ctx acc' cs' = first Just <<$>> case acc' of
    Just acc -> modAccumAll acc cs'
    Nothing  -> modAccumAll (resolveAvlCA ctx acc') cs'
  where
    modAccumAll
        :: RecAll' rs (IsAvlEntry h)
        => Rec (AVLChgAccum h) rs
        -> ChgAccumModifier rs
        -> m (Either CSMappendException (Rec (AVLChgAccum h) rs, Undo rs))
    modAccumAll RNil _                  = pure $ Right (RNil, Undo def def)
    modAccumAll accums'@(_ :& _) accMod = modAccum' accums' accMod

    modAccum'
        :: forall r rs rs' . (rs ~ (r ': rs'), RecAll' rs (IsAvlEntry h))
        => Rec (AVLChgAccum h) rs
        -> ChgAccumModifier rs
        -> m (Either CSMappendException (Rec (AVLChgAccum h) rs, Undo rs))
    modAccum' (x :& xs) md = do
        (resOne :: Either CSMappendException (AVLChgAccum h r, Undo '[r]), rest :: ChgAccumModifier rs') <-
            case md of
                CAMChange (y :& ys) ->
                    (, CAMChange ys) <$> modAccumOne ctx x (CAMChange $ rone y)
                CAMRevert (Undo (y1 :& ys1) (y2 :& ys2)) ->
                    (, CAMRevert $ Undo ys1 ys2) <$> modAccumOne ctx x (CAMRevert $ Undo (rone y1) (rone y2))
        case resOne of
            Right (acc, Undo (unone -> cs) (unone -> bs)) ->
                bimap (acc :&) (\(Undo csx bsx) -> Undo (cs :& csx) (bs :& bsx)) <<$>> modAccumAll xs rest
            Left e -> pure $ Left e

modAccumOne
    :: forall h t ctx m .
    ( AvlHashable h, RetrieveImpl (ReaderT ctx m) h
    , MonadIO m, MonadCatch m
    , IsAvlEntry h t
    )
    => ctx
    -> AVLChgAccum h t
    -> ChgAccumModifier '[t]
    -> m (Either CSMappendException (AVLChgAccum h t, Undo '[t]))
modAccumOne pState (AVLChgAccum initAvl initAcc initTouched) cMod = reThrowAVLEx @(HKey t) $
  returnWithUndo =<< case cMod of
    CAMChange (hChangeSetElToList . unone -> cs) ->
        ( Right . doUncurry AVLChgAccum
          <$> runAVLCacheT (foldM modAVL (initAvl, initTouched) cs) initAcc pState )
            `catch` \e@(CSMappendException _) -> pure $ Left e
    CAMRevert (Undo _cs (unAvlRevision . unone -> sn)) -> case (BS.null sn, deserialise sn) of
        (False, Left str) ->
            throwM $ DbApplyException $ "Error parsing AVL snapshot from undo: " <> toText str
        (False, Right rootH) ->
            pure $ Right $ AVLChgAccum (mkAVL rootH) initAcc initTouched
        _ -> pure $ Right $ AVLChgAccum initAvl initAcc initTouched
  where
    returnWithUndo
        :: Either CSMappendException (AVLChgAccum h t)
        -> m (Either CSMappendException (AVLChgAccum h t, Undo '[t]))
    returnWithUndo (Left e)   = pure $ Left e
    returnWithUndo (Right cA) =
        pure $ Right (cA, Undo def $ rone $ AvlRevision $ serialise $ avlRootHash initAvl) -- TODO compute undo

    doUncurry :: (a -> b -> c -> d) -> ((a, c), b) -> d
    doUncurry f ((a, c), b) = f a b c

    modAVL
        :: (KVConstraint k v, Serialisable (MapLayer h k v h), AVL.Hash h k v)
        => (AVL.Map h k v, Set h)
        -> (k, ValueOp v)
        -> AVLCacheT h (ReaderT ctx m) (AVL.Map h k v, Set h)
    modAVL (avl, touched) (k, valueop) = processResp =<< AVL.lookup' k avl
      where
        processResp ((lookupRes, (<> touched) -> touched'), avl') =
          case (valueop, lookupRes) of
            (NotExisted, _     ) -> pure (avl', touched')
            (New v     , _     ) -> (, touched') . snd <$> AVL.insert' k v avl'
            (Rem       , Just _) -> (, touched') . snd <$> AVL.delete' k avl'
            (Upd v     , Just _) -> (, touched') . snd <$> AVL.insert' k v avl'
            _                    -> throwM $ CSMappendException k


query
    :: forall h xs ctx m .
    ( AvlHashable h, HasGetter ctx (RootHashes h xs)
    , RetrieveImpl (ReaderT ctx m) h
    , MonadIO m, MonadCatch m
    , AllAvlEntries h xs
    )
    => ctx -> DGetter (AVLChgAccums h xs) xs m
query ctx (Just ca) hset = queryAll ca hset
  where
    queryAll :: forall rs . RecAll' rs (IsAvlEntry h) => Rec (AVLChgAccum h) rs -> HSet rs -> m (HMap rs)
    queryAll RNil RNil                       = pure RNil
    queryAll accums'@(_ :& _) reqs'@(_ :& _) = query' accums' reqs'

    query'
        :: forall r rs rs' . (rs ~ (r ': rs'), RecAll' rs (IsAvlEntry h))
        => Rec (AVLChgAccum h) rs -> HSet rs -> m (HMap rs)
    query' (AVLChgAccum initAvl initAcc _ :& accums) (HSetEl req :& reqs) = reThrowAVLEx @(HKey r) $ do
        let queryDo = fst <$> foldM queryDoOne (mempty, initAvl) req
            queryDoOne (resp, avl) key = first (processResp resp key) <$> AVL.lookup' key avl

            processResp resp key (Just v, _touched) = M.insert key v resp
            processResp resp _ _                    = resp
        responses <- fst <$> runAVLCacheT queryDo initAcc ctx
        (HMapEl responses :&) <$> queryAll accums reqs
query ctx cA req = query ctx (Just $ resolveAvlCA ctx cA) req

iter
    :: forall h xs ctx m.
    ( AvlHashable h
    , HasGetter ctx (RootHashes h xs)
    , RetrieveImpl (ReaderT ctx m) h
    , MonadIO m, MonadCatch m
    , AllAvlEntries h xs
    )
    => ctx
    -> DIter (AVLChgAccums h xs) xs m
iter ctx (Just ca) = pure $ iterAll ca
  where
    iterAll :: forall rs . RecAll' rs (IsAvlEntry h) => Rec (AVLChgAccum h) rs -> DIter' rs m
    iterAll RNil = RNil
    iterAll (AVLChgAccum initAvl initAcc _ :& accums) =
        IterAction
            (\initB f -> fmap fst $ reThrowAVLEx @(HKey (Head rs)) $
                  runAVLCacheT (AVL.fold (initB, f, id) initAvl) initAcc ctx) :& iterAll accums
iter ctx cA = iter ctx (Just $ resolveAvlCA ctx cA)
