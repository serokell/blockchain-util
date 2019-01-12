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

import           Data.Tree.AVL (MapLayer)
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Vinyl (RecMapMethod (..), Rec (..))
import           Data.Vinyl.Recursive (rmap)
import           Data.Vinyl.TypeLevel (AllConstrained)

import           Snowdrop.Core (CSMappendException (..), ChgAccum, HChangeSet, HChangeSetEl, Undo,
                                ValueOp (..), hChangeSetElToList)
import           Snowdrop.Dba.AVLp.Avl (AllAvlEntries, AvlHashable, AvlProofs, AvlUndo, IsAvlEntry,
                                        KVConstraint, RootHash (unRootHash), RootHashComp (..),
                                        RootHashes, avlRootHash, mkAVL)
import           Snowdrop.Dba.AVLp.Constraints (AvlHashC)
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
    , acaTouched :: Set h
    -- ^ Set of nodes, which were touched during all of change operations applied on tree
    }

deriving instance (Show h, Show (HVal t), Show (HKey t)) => Show (AVLChgAccum h t)

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
    , MonadCatch m
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
    => (AVL.Map h k v, Set h)
    -> (k, ValueOp v)
    -> AVLCacheT h (ReaderT ctx m) (AVL.Map h k v, Set h)
modAVL (avl, touched0) (k, valueop) = processResp =<< AVL.lookup k avl
  where
    processResp :: ((Maybe v, Set h), AVL.Map h k v)
                -> AVLCacheT h (ReaderT ctx m) (AVL.Map h k v, Set h)
    processResp ((lookupRes, (<> touched0) -> touched), avl') =
      let appendTouched = second (<> touched) . swap in
      case (valueop, lookupRes) of
        (NotExisted, Nothing) -> pure (avl', touched)
        (New v     , Nothing) -> appendTouched <$> AVL.insert k v avl'
        (Rem       , Just _)  -> appendTouched <$> AVL.delete k avl'
        (Upd v     , Just _)  -> appendTouched <$> AVL.insert k v avl'
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
    , RecMapMethod (AvlHashC h) (AVLChgAccum h) xs
    )
    => AVLChgAccums h xs
    -> ctx
    -> AvlUndo h xs
computeUndo Nothing ctx    = gett ctx
computeUndo (Just accum) _ = rmapMethod @(AvlHashC h) (RootHashComp . avlRootHash . acaMap) accum

-- | Constructs a record of getters which return values for requested keys from
-- optional ca argument. If ca is not provided, then tree is built from root
-- hashes from ctx argument. nodeActs is a record of effectful actions called on
-- all "touched" AVL nodes (i.e. all visited nodes).
query
    :: forall h xs ctx m .
    ( AvlHashable h, HasGetter ctx (RootHashes h xs)
    , RetrieveImpl (ReaderT ctx m) h
    , MonadCatch m
    , AllAvlEntries h xs
    )
    => ctx
    -> AVLChgAccums h xs
    -> Rec (Const (Set h -> m ())) xs
    -> DGetter' xs m
query ctx (Just ca) nodeActs hset = queryAll ca nodeActs hset
  where
    queryAll :: forall rs . AllConstrained (IsAvlEntry h) rs => Rec (AVLChgAccum h) rs -> Rec (Const (Set h -> m ())) rs -> HSet rs -> m (HMap rs)
    queryAll RNil RNil RNil                                 = pure RNil
    queryAll accums'@(_ :& _) acts'@(_ :& _) reqs'@(_ :& _) = query' accums' acts' reqs'

    query'
        :: forall r rs rs' . (rs ~ (r ': rs'), AllConstrained (IsAvlEntry h) rs)
        => Rec (AVLChgAccum h) rs
        -> Rec (Const (Set h -> m ())) rs
        -> HSet rs -> m (HMap rs)
    query' (AVLChgAccum initAvl initAcc _ :& accums) ((getConst -> nodeAct) :& acts) (HSetEl req :& reqs) = reThrowAVLEx @(HKey r) @h $ do
        let queryDo = fst <$> foldM queryDoOne ((mempty, mempty), initAvl) req

            queryDoOne (resp, avl) key = first (combineLookupRes resp key) <$> AVL.lookup key avl

            combineLookupRes :: (Map (HKey r) (HVal r), Set h)
                             -> HKey r
                             -> (Maybe (HVal r), Set h)
                             -> (Map (HKey r) (HVal r), Set h)
            combineLookupRes (accumVals, accumTouched) key (maybeVal, touched) =
                let newVals = maybe accumVals (\val -> M.insert key val accumVals) maybeVal
                in (newVals, Set.union accumTouched touched)

        (responses, touchedNodes) <- fst <$> runAVLCacheT queryDo initAcc ctx
        nodeAct touchedNodes
        (HMapEl responses :&) <$> queryAll accums acts reqs
query ctx cA nodeAct req = query ctx (Just $ resolveAvlCA ctx cA) nodeAct req

-- | Constructs a record of iterators which iterate through all nodes from
-- optional ca argument. If ca is not provided, then tree is built from root
-- hashes from ctx argument. nodeActs is a record of effectful actions called on
-- all "touched" AVL nodes (i.e. all nodes).
iter
    :: forall h xs ctx m.
    ( AvlHashable h
    , HasGetter ctx (RootHashes h xs)
    , RetrieveImpl (ReaderT ctx m) h
    , MonadCatch m
    , AllAvlEntries h xs
    )
    => ctx
    -> AVLChgAccums h xs
    -> Rec (Const (Set h -> m ())) xs
    -> m (DIter' xs m)
iter ctx (Just ca) nodeActs = pure $ iterAll ca nodeActs
  where
    iterAll :: forall rs . AllConstrained (IsAvlEntry h) rs => Rec (AVLChgAccum h) rs -> Rec (Const (Set h -> m ())) rs -> DIter' rs m
    iterAll RNil _ = RNil
    iterAll (AVLChgAccum initAvl initAcc _ :& accums) ((getConst -> nodeAct) :& restActs) =
        IterAction
            (\initB f -> do
                -- TODO: this is lie, ctx is not used
                ((res, touchedNodes), _) <- reThrowAVLEx @(HKey (Head rs)) @h $
                    runAVLCacheT (AVL.fold (initB, flip f, id) initAvl) initAcc ctx
                nodeAct touchedNodes
                pure res
            )
            :& iterAll accums restActs
iter ctx cA f = iter ctx (Just $ resolveAvlCA ctx cA) f
