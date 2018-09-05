{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.AVLp.Accum
       ( AVLChgAccum
       , AVLChgAccum' (..)
       , modAccum
       , query
       , iter
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.Tree.AVL (MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M

import           Snowdrop.Core (CSMappendException (..), ChgAccumModifier (..), IdSumPrefixed (..),
                                Prefix (..), StateP, StateR, Undo (..), ValueOp (..),
                                changeSetToList, idSumPrefix)
import           Snowdrop.Util (HasGetter (..))

import           Snowdrop.Execution.DbActions.AVLp.Avl (AvlHashable, KVConstraint, RootHash (..),
                                                        avlRootHash, mkAVL)
import           Snowdrop.Execution.DbActions.AVLp.State (AVLCache, AVLCacheT, RetrieveImpl,
                                                          runAVLCacheT)
import           Snowdrop.Execution.DbActions.Types (DbActionsException (..))


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
    :: AvlHashable h
    => HasGetter state (RootHash h)
    => state
    -> AVLChgAccum h k v
    -> AVLChgAccum' h k v
resolveAvlCA _ (Just cA) = cA
resolveAvlCA st  _ = AVLChgAccum
    { acaMap = pure $ unRootHash $ gett st
    , acaStorage = mempty
    , acaTouched = mempty
    }

modAccum'
    :: forall k v ctx h m .
    ( AvlHashable h, RetrieveImpl (ReaderT ctx m) h, AVL.Hash h k v, MonadIO m, MonadCatch m
    , KVConstraint k v, Serialisable (MapLayer h k v h)
    )
    => AVLChgAccum' h k v
    -> ChgAccumModifier k v
    -> ctx
    -> m (Either (CSMappendException k) (AVLChgAccum h k v, Undo k v))
modAccum' (AVLChgAccum initAvl initAcc initTouched) cMod pState = do
    returnWithUndo =<< case cMod of
      CAMChange (changeSetToList -> cs) ->
        ( Right . Just . doUncurry AVLChgAccum
          <$> runAVLCacheT (foldM modAVL (initAvl, initTouched) cs) initAcc pState )
            `catch` \e@(CSMappendException _) -> pure $ Left e
      CAMRevert (Undo _cs sn) ->
        case (BS.null sn, deserialise sn) of
          (False, Left str) ->
            throwM $ DbApplyException $ "Error parsing AVL snapshot from undo: " <> toText str
          (False, Right rootH) ->
            pure $ Right $ Just $ AVLChgAccum (mkAVL rootH) initAcc initTouched
          _ -> pure $ Right $ Just $ AVLChgAccum initAvl initAcc initTouched
  where
    returnWithUndo
        :: Either (CSMappendException k) (AVLChgAccum h k v)
        -> m (Either (CSMappendException k) (AVLChgAccum h k v, Undo k v))
    returnWithUndo (Left e)   = pure $ Left e
    returnWithUndo (Right cA) =
        pure $ Right (cA, Undo def $ serialise $ avlRootHash initAvl) -- TODO compute undo


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
    -> ChgAccumModifier k v
    -> ctx
    -> m (Either (CSMappendException k) (AVLChgAccum h k v, Undo k v))
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
