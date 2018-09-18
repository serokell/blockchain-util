{-# LANGUAGE DataKinds           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.AVLp.Actions
       ( AVLChgAccum
       , avlServerDbActions
       , avlClientDbActions
       ) where

import           Universum

import           Data.Tree.AVL (MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Recursive (rmap)

import           Snowdrop.Execution.DbActions.AVLp.Accum (AVLChgAccum (..), AVLChgAccums,
                                                          RootHashes, iter, modAccum, query)
import           Snowdrop.Execution.DbActions.AVLp.Avl (AvlHashable, KVConstraint, RootHash (..),
                                                        avlRootHash, materialize, mkAVL, saveAVL)
import           Snowdrop.Execution.DbActions.AVLp.State (AMSRequested (..), AVLCache (..),
                                                          AVLCacheT, AVLPureStorage (..),
                                                          AVLServerState (..), AllAvlEntries,
                                                          AvlProof (..), AvlProofs, ClientTempState,
                                                          IsAvlEntry, RetrieveF, RetrieveImpl,
                                                          RootHashComp (..), clientModeToTempSt,
                                                          runAVLCacheT)
import           Snowdrop.Execution.DbActions.Types (ClientMode (..), DbAccessActions (..),
                                                     DbModifyActions (..), RememberForProof (..))
import           Snowdrop.Util

avlClientDbActions
    :: forall h xs m n.
    ( MonadIO m, MonadCatch m, MonadIO n, MonadCatch n
    , AvlHashable h
    , RetrieveImpl (ReaderT (ClientTempState h xs n) m) h
    , AllAvlEntries h xs
    )
    => RetrieveF h n
    -> RootHashes h xs
    -> n (ClientMode (AvlProofs h xs) -> DbModifyActions (AVLChgAccums h xs) xs m ())
avlClientDbActions retrieveF = fmap mkActions . newTVarIO
  where
    mkActions
        :: TVar (RootHashes h xs)
        -> ClientMode (AvlProofs h xs)
        -> DbModifyActions (AVLChgAccums h xs) xs m ()
    mkActions var ctMode =
        DbModifyActions (mkAccessActions var ctMode) (apply var)

    mkAccessActions
        :: TVar (RootHashes h xs)
        -> ClientMode (AvlProofs h xs)
        -> DbAccessActions (AVLChgAccums h xs) xs  m
    mkAccessActions var ctMode = do
        DbAccessActions
          -- adding keys to amsRequested
          (\cA req -> createState >>= \ctx -> query ctx cA req)
          (\cA cs -> createState >>= \ctx -> modAccum ctx cA cs)
          -- setting amsRequested to AMSWholeTree as iteration with
          -- current implementation requires whole tree traversal
          (\cA -> createState >>= \ctx -> iter ctx cA)
      where
        createState = clientModeToTempSt retrieveF ctMode =<< atomically (readTVar var)

    apply :: AvlHashable h => TVar (RootHashes h xs) -> AVLChgAccums h xs -> m ()
    apply var (Just accums) =
        liftIO $ atomically $ writeTVar var $ rmap (RootHashComp . avlRootHash . acaMap) accums
    apply _ Nothing = pure ()

avlServerDbActions
    :: forall xs h m n .
    ( MonadIO m, MonadCatch m, AvlHashable h, MonadIO n
    , AllAvlEntries h xs

    , AllWholeTree xs
    , Monoid (Rec AMSRequested xs)
    )
    => AVLServerState h xs
    -> n ( RememberForProof -> DbModifyActions (AVLChgAccums h xs) xs m (AvlProofs h xs)
            -- `DbModifyActions` provided by `RememberForProof` object
            -- (`RememberForProof False` for disabling recording for queries performed)
          , RetrieveF h n
            -- Function to retrieve data from server state internal AVL storage
          )
avlServerDbActions = fmap mkActions . newTVarIO
  where
    retrieveHash var h = atomically $ M.lookup h . unAVLPureStorage . amsState <$> readTVar var
    mkActions var = (\recForProof ->
                        DbModifyActions
                          (mkAccessActions var recForProof)
                          (apply var),
                        retrieveHash var)
    mkAccessActions var recForProof =
        DbAccessActions
          -- adding keys to amsRequested
          (\cA req ->
            liftIO (atomically (retrieveAMS var recForProof $ rmap (AMSKeys . unHSetEl) req))
                >>= \ctx -> query ctx cA req
          )
          (\cA cs ->
             liftIO (atomically (readTVar var)) >>= \ctx -> modAccum ctx cA cs)
          -- setting amsRequested to AMSWholeTree as iteration with
          -- current implementation requires whole tree traversal
          (\cA ->
             liftIO (atomically (retrieveAMS var recForProof allWholeTree))
               >>= \ctx -> iter ctx cA)

    retrieveAMS
        :: Monoid (Rec AMSRequested xs)
        => TVar (AVLServerState h xs)
        -> RememberForProof
        -> Rec AMSRequested xs
        -> STM (AVLServerState h xs)
    retrieveAMS var (RememberForProof True) amsReq = do
        ams <- readTVar var
        writeTVar var (ams { amsRequested = amsRequested ams `mappend` amsReq }) $> ams
    retrieveAMS var _ _ = readTVar var

    apply :: Monoid (Rec AMSRequested xs)
          => TVar (AVLServerState h xs)
          -> AVLChgAccums h xs
          -> m (AvlProofs h xs)
    apply var Nothing =
        rmap (AvlProof . AVL.Proof . mkAVL . unRootHashComp) . amsRootHashes <$> atomically (readTVar var)
    apply var (Just accums) =
        liftIO $ applyDo var accums >>= \oldAms -> fst <$>
            runAVLCacheT
              (computeProofAll (amsRootHashes oldAms) accums (amsRequested oldAms))
              def
              (amsState oldAms)

    applyDo
        :: Monoid (Rec AMSRequested xs)
        => TVar (AVLServerState h xs)
        -> Rec (AVLChgAccum h) xs
        -> IO (AVLServerState h xs)
    applyDo var accums = atomically $ do
        s <- readTVar var
        (roots, accCache) <- saveAVLs (amsState s) accums
        let newState = AMS {
              amsRootHashes = roots
            , amsState = AVLPureStorage $ unAVLCache accCache <> unAVLPureStorage (amsState s)
            , amsRequested = mempty
            }
        writeTVar var newState $> s

    saveAVLs :: AllAvlEntries h rs => AVLPureStorage h -> Rec (AVLChgAccum h) rs -> STM (RootHashes h rs, AVLCache h)
    saveAVLs _ RNil = pure (RNil, def)
    saveAVLs storage (AVLChgAccum accAvl acc _ :& accums) = do
        (h, acc') <- runAVLCacheT (saveAVL accAvl) acc storage
        (restRoots, restAcc) <- saveAVLs storage accums
        pure (RootHashComp h :& restRoots, AVLCache $ unAVLCache acc' <> unAVLCache restAcc)

computeProofAll
    :: (AllAvlEntries h xs, AvlHashable h)
    => RootHashes h xs
    -> Rec (AVLChgAccum h) xs
    -> Rec AMSRequested xs
    -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AvlProofs h xs)
computeProofAll RNil RNil RNil = pure RNil
computeProofAll (RootHashComp rootH :& roots) (AVLChgAccum _ _ accTouched :& accums) (req :& reqs) = do
    proof <- AvlProof <$> computeProof rootH accTouched req
    (proof :&) <$> computeProofAll roots accums reqs

computeProof
    :: forall t h . (IsAvlEntry h t, AvlHashable h)
    => RootHash h
    -> Set h
    -> AMSRequested t
    -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Proof h (HKey t) (HVal t))
computeProof (mkAVL -> oldAvl) accTouched requested =
    case requested of
        AMSWholeTree -> computeProofWhole oldAvl
        AMSKeys ks   -> computeProofKeys oldAvl ks
  where
    computeProofWhole
        :: AVL.Map h (HKey t) (HVal t)
        -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Proof h (HKey t) (HVal t))
    computeProofWhole = fmap AVL.Proof . materialize

    computeProofKeys
        :: AVL.Map h (HKey t) (HVal t)
        -> Set (HKey t)
        -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Proof h (HKey t) (HVal t))
    computeProofKeys tree ks = do
        (avl', allTouched) <- foldM computeTouched (tree, mempty) ks
        AVL.prune (allTouched <> accTouched) =<< materialize avl'

    computeTouched
        :: (KVConstraint k v, AVL.Hash h k v, Serialisable (MapLayer h k v h))
        => (AVL.Map h k v, Set h)
        -> k
        -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Map h k v, Set h)
    computeTouched (avl, touched) key = do
        ((_res, touched'), avl') <- AVL.lookup' key avl
        pure (avl', touched' <> touched)

class AllWholeTree xs where
    allWholeTree :: Rec AMSRequested xs
instance AllWholeTree '[] where
    allWholeTree = RNil
instance AllWholeTree xs' => AllWholeTree (t ': xs') where
    allWholeTree = AMSWholeTree :& allWholeTree
