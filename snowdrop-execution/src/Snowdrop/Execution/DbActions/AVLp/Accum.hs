{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.AVLp.Accum
       ( AVLChgAccum
       , AVLChgAccum' (..)
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

import           Snowdrop.Core (CSMappendException (..), ChangeSet, IdSumPrefixed (..), Prefix (..),
                                StateP, StateR, ValueOp (..), changeSetToList, idSumPrefix)
import           Snowdrop.Util (HasGetter (..), NewestFirst (..), OldestFirst (..))

import           Snowdrop.Execution.DbActions.AVLp.Avl (AvlHashable, AvlUndo, KVConstraint,
                                                        RootHash (..), avlRootHash, mkAVL)
import           Snowdrop.Execution.DbActions.AVLp.State (AVLCache, AVLCacheT, RetrieveImpl,
                                                          runAVLCacheT)

-- | Change accumulator type for AVL tree.
data AVLChgAccum' h k v = AVLChgAccum
    { acaMap     :: AVL.Map h k v
    -- ^ AVL map, which contains avl tree with most-recent updates
    , acaStorage :: AVLCache h
    -- ^ AVL tree cache, which stores results of all `save` operations performed on AVL tree
    , acaTouched :: Set h
    -- ^ Set of nodes, which were touched during all of change operations applied on tree
    }

-- | Change accumulator type for AVL tree, wrapped with Maybe.
-- `Nothing` is treated identically to `Just $ AVLChgAccum (Pure rootHash) def mempty`,
-- where `rootHash` is root hash of current state
type AVLChgAccum h k v = Maybe (AVLChgAccum' h k v)

resolveAvlCA
    :: (AvlHashable h, HasGetter state (RootHash h))
    => state
    -> AVLChgAccum h k v
    -> AVLChgAccum' h k v
resolveAvlCA _ (Just cA) = cA
resolveAvlCA st  _ = AVLChgAccum
    { acaMap = pure $ unRootHash $ gett st
    , acaStorage = mempty
    , acaTouched = mempty
    }

modAccumU
    :: AVLChgAccum h k v
    -> NewestFirst [] (AvlUndo h)
    -> AVLChgAccum h k v
modAccumU accM (NewestFirst []) = accM
modAccumU (Just (AVLChgAccum _avl initAcc initTouched)) (NewestFirst (u:us)) =
    Just $ AVLChgAccum (mkAVL undoRootH) initAcc initTouched
  where
    undoRootH = last $ u :| us
modAccumU Nothing (NewestFirst (u:us)) =
    Just $ AVLChgAccum (mkAVL undoRootH) def def
  where
    undoRootH = last $ u :| us

computeUndo
    :: forall k v ctx h .
    ( HasGetter ctx (RootHash h)
    )
    => AVLChgAccum h k v
    -> ctx
    -> AvlUndo h
computeUndo Nothing ctx                    = gett ctx
computeUndo (Just (AVLChgAccum avl _ _)) _ = avlRootHash avl

modAccum'
    :: forall k v ctx h m .
    ( AvlHashable h, RetrieveImpl (ReaderT ctx m) h, AVL.Hash h k v, MonadIO m, MonadCatch m
    , KVConstraint k v, Serialisable (MapLayer h k v h)
    )
    => AVLChgAccum' h k v
    -> OldestFirst [] (ChangeSet k v)
    -> ctx
    -> m (Either (CSMappendException k) (OldestFirst [] (AVLChgAccum h k v)))
modAccum' initAcc (OldestFirst css) pState =
    (Right <$> modAccumDo) `catch` \e@(CSMappendException _) -> pure $ Left e
  where
    modAccumDo = fmap Just . OldestFirst . reverse . snd <$> foldM processCS (initAcc, []) css
    processCS (AVLChgAccum avl acc touched, res) (changeSetToList -> cs) = do
        acc' <- doUncurry AVLChgAccum <$> runAVLCacheT (foldM modAVL (avl, touched) cs) acc pState
        pure (acc', acc' : res)

    doUncurry :: (a -> b -> c -> d) -> ((a, c), b) -> d
    doUncurry f ((a, c), b) = f a b c

    modAVL
        :: (AVL.Map h k v, Set h)
        -> (k, ValueOp v)
        -> AVLCacheT h (ReaderT ctx m) (AVL.Map h k v, Set h)
    modAVL (avl, touched) (k, valueop) = processResp =<< AVL.lookup' k avl
      where
        processResp ((lookupRes, (<> touched) -> touched'), avl') =
          case (valueop, lookupRes) of
            (NotExisted ,_     ) -> pure (avl', touched')
            (New v      ,_     ) -> (, touched') . snd <$> AVL.insert' k v avl'
            (Rem        ,Just _) -> (, touched') . snd <$> AVL.delete' k avl'
            (Upd v      ,Just _) -> (, touched') . snd <$> AVL.insert' k v avl'
            _                    -> throwM $ CSMappendException k

modAccum
    :: forall k v ctx h m .
    ( AvlHashable h, HasGetter ctx (RootHash h), RetrieveImpl (ReaderT ctx m) h
    , AVL.Hash h k v, MonadIO m, MonadCatch m
    , KVConstraint k v, Serialisable (MapLayer h k v h))
    => AVLChgAccum h k v
    -> OldestFirst [] (ChangeSet k v)
    -> ctx
    -> m (Either (CSMappendException k) (OldestFirst [] (AVLChgAccum h k v)))
-- modAccum accM (changeSetToList -> []) _ = pure $ Right accM
-- empty changeset won't alter accumulator
modAccum (Just acc) cs sth = modAccum' @k @v @ctx acc cs sth
modAccum cA cs sth         = modAccum' @k @v @ctx (resolveAvlCA sth cA) cs sth

query
    :: forall k v ctx h m .
    ( AvlHashable h, HasGetter ctx (RootHash h)
    , RetrieveImpl (ReaderT ctx m) h, AVL.Hash h k v, MonadIO m, MonadCatch m
    , KVConstraint k v, Serialisable (MapLayer h k v h)
    )
    => AVLChgAccum h k v -> StateR k -> ctx -> m (StateP k v)
query (Just (AVLChgAccum initAvl initAcc _)) req sth = fmap fst $ runAVLCacheT queryDo initAcc sth
  where
    queryDo = fst <$> foldM queryDoOne (mempty, initAvl) req
    queryDoOne (m, avl) key = first processResp <$> AVL.lookup' key avl
      where
        processResp (Just v, _touched) = M.insert key v m
        processResp _                  = m
query cA req sth = query (Just $ resolveAvlCA sth cA) req sth

iter
    :: forall k v ctx b h m.
    ( AvlHashable h, HasGetter ctx (RootHash h)
    , RetrieveImpl (ReaderT ctx m) h, AVL.Hash h k v, MonadIO m, MonadCatch m
    , KVConstraint k v,  Serialisable (MapLayer h k v h)
    )
    => AVLChgAccum h k v -> Prefix -> b -> ((k, v) -> b -> b) -> ctx -> m b
iter (Just (AVLChgAccum initAvl initAcc _)) pr initB f sth =
    fmap fst $ runAVLCacheT (AVL.fold (initB, f', id) initAvl) initAcc sth
  where
    f' kv@(k, _) b =
      if idSumPrefix k == pr
      then f kv b
      else b
iter cA pr b f sth = iter (Just $ resolveAvlCA sth cA) pr b f sth
