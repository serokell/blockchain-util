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

import           Snowdrop.Execution.DbActions.AVLp.Accum (AVLChgAccum, AVLChgAccum' (..), iter,
                                                          modAccum, query)
import           Snowdrop.Execution.DbActions.AVLp.Avl (AvlHashable, KVConstraint, RootHash (..),
                                                        avlRootHash, materialize, mkAVL, saveAVL)
import           Snowdrop.Execution.DbActions.AVLp.State (AMSRequested (..), AVLCache (..),
                                                          AVLCacheT, AVLPureStorage (..),
                                                          AVLServerState (..), ClientTempState,
                                                          RetrieveF, RetrieveImpl,
                                                          clientModeToTempSt, reThrowAVLEx,
                                                          runAVLCacheT)
import           Snowdrop.Execution.DbActions.Types (ClientMode (..), DbAccessActions (..),
                                                     DbModifyActions (..), RememberForProof (..))


avlClientDbActions
    :: forall k v m h n.
    ( KVConstraint k v
    , MonadIO m
    , MonadCatch m
    , AvlHashable h
    , AVL.Hash h k v
    , MonadIO n
    , MonadCatch n
    , RetrieveImpl (ReaderT (ClientTempState h k v n) m) h
    , Serialisable (MapLayer h k v h)
    )
    => RetrieveF h n
    -> RootHash h
    -> n (ClientMode (AVL.Proof h k v) -> DbModifyActions (AVLChgAccum h k v) k v m ())
avlClientDbActions retrieveF = fmap mkActions . newTVarIO
  where
    mkActions
        :: TVar (RootHash h)
        -> ClientMode (AVL.Proof h k v)
        -> DbModifyActions (AVLChgAccum h k v) k v m ()
    mkActions var ctMode =
        DbModifyActions (mkAccessActions var ctMode) (reThrowAVLEx @k . apply var)
    mkAccessActions
        :: TVar (RootHash h)
        -> ClientMode (AVL.Proof h k v)
        -> DbAccessActions (AVLChgAccum h k v) k v m
    mkAccessActions var ctMode =
        DbAccessActions
          -- adding keys to amsRequested
          (\cA req -> reThrowAVLEx @k $ query cA req =<< createState)
          (\cA cs -> reThrowAVLEx @k $ modAccum cA cs =<< createState)
          -- setting amsRequested to AMSWholeTree as iteration with
          -- current implementation requires whole tree traversal
          (\cA p b f -> reThrowAVLEx @k $ iter cA p b f =<< createState)
      where
        createState = clientModeToTempSt retrieveF ctMode =<< atomically (readTVar var)
    apply :: AvlHashable h => TVar (RootHash h) -> AVLChgAccum h k v -> m ()
    apply var (Just (AVLChgAccum accAvl _acc _accTouched)) =
        liftIO $ atomically $ writeTVar var (avlRootHash accAvl)
    apply _ Nothing = pure ()

avlServerDbActions
    :: forall k v m h n .
    ( MonadIO m, MonadCatch m, AvlHashable h, AVL.Hash h k v, MonadIO n
    , KVConstraint k v, Serialisable (MapLayer h k v h)
    )
    => AVLServerState h k
    -> n ( RememberForProof -> DbModifyActions (AVLChgAccum h k v) k v m (AVL.Proof h k v)
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
                          (reThrowAVLEx @k . apply var),
                        retrieveHash var)
    mkAccessActions var recForProof =
        DbAccessActions
          -- adding keys to amsRequested
          (\cA req -> liftIO $ reThrowAVLEx @k $ query cA req =<<
            atomically (retrieveAMS var recForProof $ AMSKeys req ))
          (\cA cs -> liftIO $ reThrowAVLEx @k $ modAccum cA cs =<< atomically (readTVar var))
          -- setting amsRequested to AMSWholeTree as iteration with
          -- current implementation requires whole tree traversal
          (\cA p b f -> liftIO $ reThrowAVLEx @k $ iter cA p b f =<<
            atomically (retrieveAMS var recForProof AMSWholeTree ))

    retrieveAMS var (RememberForProof True) amsReq = do
        ams <- readTVar var
        writeTVar var (ams { amsRequested = amsRequested ams <> amsReq }) $> ams
    retrieveAMS var _ _ = readTVar var

    apply :: TVar (AVLServerState h k) -> AVLChgAccum h k v -> m (AVL.Proof h k v)
    apply var (Just (AVLChgAccum accAvl acc accTouched)) =
        liftIO $ applyDo >>= \oldAms -> fst <$>
            runAVLCacheT
              (computeProof (amsRootHash oldAms) (amsRequested oldAms))
              def
              (amsState oldAms)
      where
        applyDo :: IO (AVLServerState h k)
        applyDo = atomically $ do
            ams <- readTVar var
            (h', acc') <- runAVLCacheT (saveAVL accAvl) acc (amsState ams)
            let newState = AMS {
                  amsRootHash = h'
                , amsState = AVLPureStorage $ unAVLCache acc' <> unAVLPureStorage (amsState ams)
                , amsRequested = mempty
                }
            writeTVar var newState $> ams

        computeProof
            :: RootHash h
            -> AMSRequested k
            -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Proof h k v)
        computeProof (mkAVL -> oldAvl) requested =
            case requested of
                AMSWholeTree -> computeProofWhole
                AMSKeys ks   -> computeProofKeys ks
          where
            computeProofWhole :: AVLCacheT h (ReaderT (AVLPureStorage h ) IO) (AVL.Proof h k v)
            computeProofWhole = AVL.Proof <$> materialize oldAvl

            computeProofKeys
                :: Set k
                -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Proof h k v)
            computeProofKeys ks = do
                (avl', allTouched) <- foldM computeTouched (oldAvl, mempty) ks
                AVL.prune (allTouched <> accTouched) =<< materialize avl'

            computeTouched
                :: (AVL.Map h k v, Set h)
                -> k
                -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Map h k v, Set h)
            computeTouched (avl, touched) key = do
                ((_res, touched'), avl') <- AVL.lookup' key avl
                pure (avl', touched' <> touched)

    apply var Nothing = AVL.Proof . mkAVL . amsRootHash <$> atomically (readTVar var)
