{-# LANGUAGE DataKinds           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.AVLp.Actions
       ( AVLChgAccum
       , avlServerDbActions
       , avlClientDbActions
       , AllWholeTree
       ) where

import           Universum

import           Data.Tree.AVL (MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Recursive (rmap)

import           Snowdrop.Core (ChgAccum, Undo)
import           Snowdrop.Dba.AVLp.Accum (AVLChgAccum (..), AVLChgAccums, RootHashes, computeUndo,
                                          iter, modAccum, modAccumU, query)
import           Snowdrop.Dba.AVLp.Avl (AllAvlEntries, AvlHashable, AvlProof (..),
                                        AvlProofs, AvlUndo, IsAvlEntry, KVConstraint,
                                        RootHash (..), RootHashComp (..), avlRootHash,
                                        materialize, mkAVL, saveAVL)
import           Snowdrop.Dba.AVLp.State (AMSRequested (..), AVLCache (..),
                                                          AVLCacheT, AVLPureStorage (..),
                                                          AVLServerState (..), ClientTempState,
                                                          RetrieveF, RetrieveImpl,
                                                          clientModeToTempSt, runAVLCacheT)
import           Snowdrop.Dba.Base (ClientMode (..), DbAccessActions (..), DbAccessActionsM (..),
                                    DbAccessActionsU (..), DbApplyProof, DbComponents,
                                    DbModifyActions (..), RememberForProof (..))
import           Snowdrop.Hetero (HKey, HVal, unHSetEl)
import           Snowdrop.Util (NewestFirst)

avlClientDbActions
    :: forall conf h xs .
    ( AvlHashable h
    , RetrieveImpl (ReaderT (ClientTempState h xs STM) STM) h
    , AllAvlEntries h xs

    , Undo conf ~ AvlUndo h xs
    , ChgAccum conf ~ AVLChgAccums h xs
    , DbApplyProof conf ~ ()
    , xs ~ DbComponents conf
    )
    => RetrieveF h STM
    -> RootHashes h xs
    -> STM (ClientMode (AvlProofs h xs) -> DbModifyActions conf STM)
avlClientDbActions retrieveF = fmap mkActions . newTVar
  where
    mkActions
        :: TVar (RootHashes h xs)
        -> ClientMode (AvlProofs h xs)
        -> DbModifyActions conf STM
    mkActions var ctMode =
        DbModifyActions (mkAccessActions var ctMode) (apply var)

    mkAccessActions
        :: TVar (RootHashes h xs)
        -> ClientMode (AvlProofs h xs)
        -> DbAccessActionsU conf STM
    mkAccessActions var ctMode = daaU
      where
        daa =
          DbAccessActions
            -- adding keys to amsRequested
          (\cA req -> createState >>= \ctx -> query ctx cA req)
            -- setting amsRequested to AMSWholeTree as iteration with
            -- current implementation requires whole tree traversal
          (\cA -> createState >>= \ctx -> iter ctx cA)
        daaM = DbAccessActionsM daa (\cA cs -> createState >>= \ctx -> modAccum ctx cA cs)
        daaU = DbAccessActionsU daaM
                  (withProjUndo . modAccumU)
                  (\cA _cs -> pure . Right . computeUndo cA =<< createState)
        createState = clientModeToTempSt retrieveF ctMode =<< ((readTVar var))

    apply :: AvlHashable h => TVar (RootHashes h xs) -> AVLChgAccums h xs -> STM ()
    apply var (Just accums) =
        writeTVar var $ rmap (RootHashComp . avlRootHash . acaMap) accums
    apply _ Nothing = pure ()

withProjUndo :: (MonadThrow m, Applicative f) => (NewestFirst [] undo -> a) -> NewestFirst [] undo -> m (f a)
withProjUndo action = pure . pure . action

avlServerDbActions
    :: forall conf h xs .
    ( AvlHashable h
    , AllAvlEntries h xs

    , AllWholeTree xs
    , Monoid (Rec AMSRequested xs)

    , Undo conf ~ AvlUndo h xs
    , ChgAccum conf ~ AVLChgAccums h xs
    , DbApplyProof conf ~ AvlProofs h xs
    , xs ~ DbComponents  conf
    )
    => AVLServerState h xs
    -> STM ( RememberForProof -> DbModifyActions conf STM
            -- `DbModifyActions` provided by `RememberForProof` object
            -- (`RememberForProof False` for disabling recording for queries performed)
          , RetrieveF h STM
            -- Function to retrieve data from server state internal AVL storage
          )
avlServerDbActions = fmap mkActions . newTVar
  where
    retrieveHash var h = M.lookup h . unAVLPureStorage . amsState <$> readTVar var
    mkActions var = (\recForProof ->
                        DbModifyActions
                          (mkAccessActions var recForProof)
                          (apply var),
                        retrieveHash var)
    mkAccessActions var recForProof = daaU
      where
        daa = DbAccessActions
                -- adding keys to amsRequested
                (\cA req ->
                  (retrieveAMS var recForProof $ rmap (AMSKeys . unHSetEl) req)
                      >>= \ctx -> query ctx cA req
                )
                -- setting amsRequested to AMSWholeTree as iteration with
                -- current implementation requires whole tree traversal
                (\cA ->
                  (retrieveAMS var recForProof allWholeTree)
                    >>= \ctx -> iter ctx cA
                )
        daaM = DbAccessActionsM daa (\cA cs -> (readTVar var) >>= \ctx -> modAccum ctx cA cs)
        daaU = DbAccessActionsU daaM
                  (withProjUndo . modAccumU)
                  (\cA _cs -> pure . Right . computeUndo cA =<< (readTVar var))

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
          -> STM (AvlProofs h xs)
    apply var Nothing =
        rmap (AvlProof . AVL.Proof . mkAVL . unRootHashComp) . amsRootHashes <$> (readTVar var)
    apply var (Just accums) =
        applyDo var accums >>= \oldAms -> fst <$>
            runAVLCacheT
              (computeProofAll (amsRootHashes oldAms) accums (amsRequested oldAms))
              def
              (amsState oldAms)

    applyDo
        :: Monoid (Rec AMSRequested xs)
        => TVar (AVLServerState h xs)
        -> Rec (AVLChgAccum h) xs
        -> STM (AVLServerState h xs)
    applyDo var accums = do
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
    -> AVLCacheT h (ReaderT (AVLPureStorage h) STM) (AvlProofs h xs)
computeProofAll RNil RNil RNil = pure RNil
computeProofAll (RootHashComp rootH :& roots) (AVLChgAccum _ _ accTouched :& accums) (req :& reqs) = do
    proof <- AvlProof <$> computeProof rootH accTouched req
    (proof :&) <$> computeProofAll roots accums reqs

computeProof
    :: forall t h . (IsAvlEntry h t, AvlHashable h)
    => RootHash h
    -> Set h
    -> AMSRequested t
    -> AVLCacheT h (ReaderT (AVLPureStorage h) STM) (AVL.Proof h (HKey t) (HVal t))
computeProof (mkAVL -> oldAvl) accTouched requested =
    case requested of
        AMSWholeTree -> computeProofWhole oldAvl
        AMSKeys ks   -> computeProofKeys oldAvl ks
  where
    computeProofWhole
        :: AVL.Map h (HKey t) (HVal t)
        -> AVLCacheT h (ReaderT (AVLPureStorage h) STM) (AVL.Proof h (HKey t) (HVal t))
    computeProofWhole = fmap AVL.Proof . materialize

    computeProofKeys
        :: AVL.Map h (HKey t) (HVal t)
        -> Set (HKey t)
        -> AVLCacheT h (ReaderT (AVLPureStorage h) STM) (AVL.Proof h (HKey t) (HVal t))
    computeProofKeys tree ks = do
        (avl', allTouched) <- foldM computeTouched (tree, mempty) ks
        AVL.prune (allTouched <> accTouched) =<< materialize avl'

    computeTouched
        :: (KVConstraint k v, AVL.Hash h k v, Serialisable (MapLayer h k v h))
        => (AVL.Map h k v, Set h)
        -> k
        -> AVLCacheT h (ReaderT (AVLPureStorage h) STM) (AVL.Map h k v, Set h)
    computeTouched (avl, touched) key = do
        ((_res, touched'), avl') <- AVL.lookup' key avl
        pure (avl', touched' <> touched)

class AllWholeTree xs where
    allWholeTree :: Rec AMSRequested xs
instance AllWholeTree '[] where
    allWholeTree = RNil
instance AllWholeTree xs' => AllWholeTree (t ': xs') where
    allWholeTree = AMSWholeTree :& allWholeTree
