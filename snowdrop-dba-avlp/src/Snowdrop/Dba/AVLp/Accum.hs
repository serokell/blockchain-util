{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.AVLp.Accum
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
       , AvlClientConf
       , AvlServerConf
       ) where

import           Universum

import           Data.Tree.AVL (MapLayer (..))
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Recursive (rmap)
import           Data.Vinyl.TypeLevel (AllConstrained)

import           Snowdrop.Core (CSMappendException (..), ChgAccum, HChangeSet, HChangeSetEl, Undo,
                                ValueOp (..), hChangeSetElToList)
import           Snowdrop.Dba.AVLp.Avl (AllAvlEntries, AvlHashable, AvlProofs, AvlUndo, IsAvlEntry,
                                        KVConstraint, RootHash (unRootHash), RootHashComp (..),
                                        RootHashes, avlRootHash, mkAVL)
import           Snowdrop.Dba.AVLp.Constraints (RHashable, rmapWithHash)
import           Snowdrop.Dba.AVLp.State (AVLCache, AVLCacheT, RetrieveImpl, reThrowAVLEx,
                                          runAVLCacheT)
import           Snowdrop.Dba.Base (DGetter', DIter', DModify', DbApplyProof, DbComponents,
                                    IterAction (..))
import           Snowdrop.Hetero (HKey, HMap, HMapEl (..), HSet, HSetEl (..), HVal, Head)
import           Snowdrop.Util (HasGetter (..), NewestFirst (..), OldestFirst (..),
                                Serialisable (..))

data AvlClientConf hash (xs :: [*])

type instance ChgAccum (AvlClientConf hash xs) = AVLChgAccums hash xs
type instance Undo (AvlClientConf hash xs) = AvlUndo hash xs
type instance DbComponents (AvlClientConf hash xs) = xs
type instance DbApplyProof (AvlClientConf hash xs) = ()

data AvlServerConf hash (xs :: [*])

type instance ChgAccum (AvlServerConf hash xs) = AVLChgAccums hash xs
type instance Undo (AvlServerConf hash xs) = AvlUndo hash xs
type instance DbComponents (AvlServerConf hash xs) = xs
type instance DbApplyProof (AvlServerConf hash xs) = AvlProofs hash xs

-- | Change accumulator type for AVL tree.
data AVLChgAccum h t = AVLChgAccum
    { acaMap     :: AVL.Map h (HKey t) (HVal t)
    -- ^ AVL map, which contains avl tree with most-recent updates
    , acaStorage :: AVLCache h
    -- ^ AVL tree cache, which stores results of all `save` operations performed on AVL tree
    , acaTouched :: Set AVL.Revision
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
-- modAccum = undefined
modAccum ctx acc' cs' = fmap Just <<$>> case acc' of
    Just acc -> modAccumAll acc cs'
    Nothing  -> modAccumAll (resolveAvlCA ctx acc') cs'
  where
    modAccumAll
        :: AllConstrained (IsAvlEntry h) rs
        => Rec (AVLChgAccum h) rs
        -> OldestFirst [] (HChangeSet rs)
        -> m (Either CSMappendException (OldestFirst [] (Rec (AVLChgAccum h) rs)))
    modAccumAll initAcc css =
        fmap Right impl `catch` \(e :: CSMappendException) -> pure $ Left e
      where
        impl = OldestFirst . reverse . snd <$> foldM foldHandler (initAcc, []) css
        foldHandler (curAcc, resAccs) hcs = (\acc -> (acc, acc : resAccs)) <$> modAccumOne curAcc hcs

    modAccumOne
        :: AllConstrained (IsAvlEntry h) rs
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
    :: forall k v h m ctx .
      ( KVConstraint k v
      , Serialisable (MapLayer h k v h)
      , AVL.Hash h k v
      , AvlHashable h
      , MonadCatch m
      , RetrieveImpl (ReaderT ctx m) h
      )
    => (AVL.Map h k v, Set AVL.Revision)
    -> (k, ValueOp v)
    -> AVLCacheT h (ReaderT ctx m) (AVL.Map h k v, Set AVL.Revision)
modAVL (avl, touched) (k, valueop) = processResp =<< AVL.lookup k avl
  where
    processResp :: ((Maybe v, Set AVL.Revision), AVL.Map h k v)
                -> AVLCacheT h (ReaderT ctx m) (AVL.Map h k v, Set AVL.Revision)
    processResp ((lookupRes, (<> touched) -> touched'), avl') =
      case (valueop, lookupRes) of
        (NotExisted, Nothing) -> pure (avl', touched')
        (New v     , Nothing) -> (, touched') . snd <$> AVL.insert k v avl'
        (Rem       , Just _)  -> (, touched') . snd <$> AVL.delete k avl'
        (Upd v     , Just _)  -> (, touched') . snd <$> AVL.insert k v avl'
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
    , RHashable h xs
    )
    => AVLChgAccums h xs
    -> ctx
    -> AvlUndo h xs
computeUndo Nothing ctx    = gett ctx
computeUndo (Just accum) _ = rmapWithHash @h (RootHashComp . avlRootHash . acaMap) accum

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
    queryAll :: forall rs . AllConstrained (IsAvlEntry h) rs => Rec (AVLChgAccum h) rs -> HSet rs -> m (HMap rs)
    queryAll RNil RNil                       = pure RNil
    queryAll accums'@(_ :& _) reqs'@(_ :& _) = query' accums' reqs'

    query'
        :: forall r rs rs' . (rs ~ (r ': rs'), AllConstrained (IsAvlEntry h) rs)
        => Rec (AVLChgAccum h) rs -> HSet rs -> m (HMap rs)
    query' (AVLChgAccum initAvl initAcc _ :& accums) (HSetEl req :& reqs) = reThrowAVLEx @(HKey r) @h $ do
        let queryDo = fst <$> foldM queryDoOne (mempty, initAvl) req
            queryDoOne (resp, avl) key = first (processResp resp key) <$> AVL.lookup key avl

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
    iterAll :: forall rs . AllConstrained (IsAvlEntry h) rs => Rec (AVLChgAccum h) rs -> DIter' rs m
    iterAll RNil = RNil
    iterAll (AVLChgAccum initAvl initAcc _ :& accums) =
        IterAction
            (\initB f -> fmap (fst . fst) $ reThrowAVLEx @(HKey (Head rs)) @h $
                runAVLCacheT (AVL.fold (initB, flip f, id) initAvl) initAcc ctx) :& iterAll accums

iter ctx cA = iter ctx (Just $ resolveAvlCA ctx cA)
