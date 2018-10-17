{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
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
       , modAccumU
       , computeUndo
       , query
       , iter
       ) where

import           Universum

import           Data.Tree.AVL (MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Recursive (rmap)

import           Snowdrop.Core (CSMappendException (..), HChangeSet, HChangeSetEl, ValueOp (..),
                                hChangeSetElToList)
import           Snowdrop.Execution.DbActions.AVLp.Avl (AllAvlEntries, AvlHashable, AvlUndo,
                                                        IsAvlEntry, KVConstraint,
                                                        RootHash (unRootHash), RootHashComp (..),
                                                        RootHashes, avlRootHash, mkAVL)
import           Snowdrop.Execution.DbActions.AVLp.State (AVLCache, AVLCacheT, RetrieveImpl,
                                                          reThrowAVLEx, runAVLCacheT)
import           Snowdrop.Execution.DbActions.Types (DGetter', DIter', DModify', IterAction (..))
import           Snowdrop.Util (HKey, HMap, HMapEl (..), HSet, HSetEl (..), HVal, HasGetter (..),
                                Head, NewestFirst (..), OldestFirst (..), RecAll')

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
    :: (AvlHashable h, HasGetter state (RootHashes h xs))
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
    -> AVLChgAccums h xs
    -> DModify' (AVLChgAccums h xs) xs m
modAccum ctx acc' cs' = fmap Just <<$>> case acc' of
    Just acc -> modAccumAll acc cs'
    Nothing  -> modAccumAll (resolveAvlCA ctx acc') cs'
  where
    modAccumAll
        :: RecAll' rs (IsAvlEntry h)
        => Rec (AVLChgAccum h) rs
        -> OldestFirst [] (HChangeSet rs)
        -> m (Either CSMappendException (OldestFirst [] (Rec (AVLChgAccum h) rs)))
    modAccumAll initAcc css =
        fmap Right impl `catch` \(e :: CSMappendException) -> pure $ Left e
      where
        impl = OldestFirst . reverse . snd <$> foldM foldHandler (initAcc, []) css
        foldHandler (curAcc, resAccs) hcs = (\acc -> (acc, acc : resAccs)) <$> modAccumOne curAcc hcs

    modAccumOne
        :: RecAll' rs (IsAvlEntry h)
        => Rec (AVLChgAccum h) rs
        -> HChangeSet rs
        -> m (Rec (AVLChgAccum h) rs)
    modAccumOne RNil RNil                     = pure RNil
    modAccumOne (ca :& caRest) (cs :& csRest) =
        liftA2 (:&) (modAccumOneDo ca cs) (modAccumOne caRest csRest)

    modAccumOneDo
        :: forall r .
          IsAvlEntry h r
        => AVLChgAccum h r
        -> HChangeSetEl r
        -> m (AVLChgAccum h r)
    modAccumOneDo (AVLChgAccum avl acc touched) (hChangeSetElToList -> cs) =
        reThrowAVLEx @(HKey r) @h $
        doUncurry AVLChgAccum <$> runAVLCacheT (foldM modAVL (avl, touched) cs) acc ctx

    doUncurry :: (a -> b -> c -> d) -> ((a, c), b) -> d
    doUncurry f ((a, c), b) = f a b c

modAVL
    ::
      ( KVConstraint k v
      , Serialisable (MapLayer h k v h)
      , AVL.Hash h k v
      , AvlHashable h
      , MonadCatch m
      , RetrieveImpl (ReaderT ctx m) h
      )
    => (AVL.Map h k v, Set h)
    -> (k, ValueOp v)
    -> AVLCacheT h (ReaderT ctx m) (AVL.Map h k v, Set h)
modAVL (avl, touched) (k, valueop) = processResp =<< AVL.lookup' k avl
  where
    processResp ((lookupRes, (<> touched) -> touched'), avl') =
      case (valueop, lookupRes) of
        (NotExisted, Nothing) -> pure (avl', touched')
        (New v     , Nothing) -> (, touched') . snd <$> AVL.insert' k v avl'
        (Rem       , Just _)  -> (, touched') . snd <$> AVL.delete' k avl'
        (Upd v     , Just _)  -> (, touched') . snd <$> AVL.insert' k v avl'
        _                     -> throwM $ CSMappendException k


modAccumU
    :: forall h xs . AVLChgAccums h xs
    -> NewestFirst [] (AvlUndo h xs)
    -> AVLChgAccums h xs
modAccumU accM (NewestFirst []) = accM
modAccumU (Just accum) (NewestFirst (u:us)) =
    Just $ rmap' accum undoRootH
  where
    rmap' :: forall xs'. Rec (AVLChgAccum h) xs' -> AvlUndo h xs' -> Rec (AVLChgAccum h) xs'
    rmap' RNil RNil     = RNil
    rmap' (AVLChgAccum _ acc touched :& xs) (RootHashComp rootH :& rhs) =
      AVLChgAccum (mkAVL rootH) acc touched :& rmap' xs rhs
    undoRootH = last $ u :| us
modAccumU Nothing (NewestFirst (u:us)) =
    Just $ rmap (\(RootHashComp rootH) -> AVLChgAccum (mkAVL rootH) def def) undoRootH
  where
    undoRootH = last $ u :| us

computeUndo
    :: forall h xs ctx .
    ( HasGetter ctx (RootHashes h xs)
    )
    => AVLChgAccums h xs
    -> ctx
    -> AvlUndo h xs
computeUndo Nothing ctx    = gett ctx
computeUndo (Just accum) _ = rmap (RootHashComp . avlRootHash . acaMap) accum

query
    :: forall h xs ctx m .
    ( AvlHashable h, HasGetter ctx (RootHashes h xs)
    , RetrieveImpl (ReaderT ctx m) h
    , MonadIO m, MonadCatch m
    , AllAvlEntries h xs
    )
   => ctx -> AVLChgAccums h xs -> DGetter' xs m
query ctx (Just ca) hset = queryAll ca hset
  where
    queryAll :: forall rs . RecAll' rs (IsAvlEntry h) => Rec (AVLChgAccum h) rs -> HSet rs -> m (HMap rs)
    queryAll RNil RNil                       = pure RNil
    queryAll accums'@(_ :& _) reqs'@(_ :& _) = query' accums' reqs'

    query'
        :: forall r rs rs' . (rs ~ (r ': rs'), RecAll' rs (IsAvlEntry h))
        => Rec (AVLChgAccum h) rs -> HSet rs -> m (HMap rs)
    query' (AVLChgAccum initAvl initAcc _ :& accums) (HSetEl req :& reqs) = reThrowAVLEx @(HKey r) @h $ do
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
    -> AVLChgAccums h xs
    -> m (DIter' xs m)
iter ctx (Just ca) = pure $ iterAll ca
  where
    iterAll :: forall rs . RecAll' rs (IsAvlEntry h) => Rec (AVLChgAccum h) rs -> DIter' rs m
    iterAll RNil = RNil
    iterAll (AVLChgAccum initAvl initAcc _ :& accums) =
        IterAction
            (\initB f -> fmap fst $ reThrowAVLEx @(HKey (Head rs)) @h $
                runAVLCacheT (AVL.fold (initB, flip f, id) initAvl) initAcc ctx) :& iterAll accums
iter ctx cA = iter ctx (Just $ resolveAvlCA ctx cA)
