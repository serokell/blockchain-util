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

import           Snowdrop.Execution.DbActions.AVLp.Accum (AVLChgAccum, AVLChgAccum' (..),
                                                          computeUndo, iter, modAccum, modAccumU,
                                                          query)
import           Snowdrop.Execution.DbActions.AVLp.Avl (AvlHashable, AvlUndo, KVConstraint,
                                                        RootHash (..), avlRootHash, materialize,
                                                        mkAVL, saveAVL)
import           Snowdrop.Execution.DbActions.AVLp.State (AMSRequested (..), AVLCache (..),
                                                          AVLCacheT, AVLPureStorage (..),
                                                          AVLServerState (..), ClientTempState,
                                                          RetrieveF, RetrieveImpl,
                                                          clientModeToTempSt, reThrowAVLEx,
                                                          runAVLCacheT)
import           Snowdrop.Execution.DbActions.Types (ClientMode (..), DbAccessActions (..),
                                                     DbAccessActionsM (..), DbAccessActionsU (..),
                                                     DbActionsException (..), DbModifyActions (..),
                                                     RememberForProof (..))
import           Snowdrop.Util (HasPrism, NewestFirst, inj, proj)

avlClientDbActions
  :: forall k v m h n undo.
    ( KVConstraint k v
    , MonadIO m
    , MonadCatch m
    , AvlHashable h
    , AVL.Hash h k v
    , MonadIO n
    , MonadCatch n
    , RetrieveImpl (ReaderT (ClientTempState h k v n) m) h
    , Serialisable (MapLayer h k v h)
    , HasPrism undo (AvlUndo h)
    )
    => RetrieveF h n
    -> RootHash h
    -> n (ClientMode (AVL.Proof h k v) -> DbModifyActions (AVLChgAccum h k v) undo k v m ())
avlClientDbActions retrieveF = fmap mkActions . newTVarIO
  where
    mkActions var ctMode =
        DbModifyActions (mkAccessActions var ctMode) (reThrowAVLEx @k . apply var)

    mkAccessActions var ctMode = daaU
      where
        daa =
          DbAccessActions
            -- adding keys to amsRequested
            (\cA req -> reThrowAVLEx @k $ query cA req =<< createState)
            -- setting amsRequested to AMSWholeTree as iteration with
            -- current implementation requires whole tree traversal
            (\cA p b f -> reThrowAVLEx @k $ iter cA p b f =<< createState)
        daaM = DbAccessActionsM daa (\cA cs -> reThrowAVLEx @k $ modAccum cA cs =<< createState)
        daaU = DbAccessActionsU daaM
                  (withProjUndo . modAccumU)
                  (\cA _cs -> pure . Right . inj . computeUndo cA =<< reThrowAVLEx @k createState)
        createState = clientModeToTempSt retrieveF ctMode =<< atomically (readTVar var)

    apply :: AvlHashable h => TVar (RootHash h) -> AVLChgAccum h k v -> m ()
    apply var (Just (AVLChgAccum accAvl _acc _accTouched)) =
        liftIO $ atomically $ writeTVar var (avlRootHash accAvl)
    apply _ Nothing = pure ()

withProjUndo :: (HasPrism undo undo', MonadThrow m, Applicative f) => (NewestFirst [] undo' -> a) -> NewestFirst [] undo -> m (f a)
withProjUndo action = maybe (throwM DbUndoProjectionError) (pure . pure . action) . traverse proj

avlServerDbActions
    :: forall k v m h n undo .
    ( MonadIO m, MonadCatch m, AvlHashable h, AVL.Hash h k v, MonadIO n
    , KVConstraint k v, Serialisable (MapLayer h k v h)
    , HasPrism undo (AvlUndo h)
    )
    => AVLServerState h k
    -> n ( RememberForProof -> DbModifyActions (AVLChgAccum h k v) undo k v m (AVL.Proof h k v)
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
    mkAccessActions var recForProof = daaU
      where
        daa = DbAccessActions
                -- adding keys to amsRequested
                (\cA req -> liftIO $ reThrowAVLEx @k $ query cA req =<<
                              atomically (retrieveAMS var recForProof $ AMSKeys req ))
                -- setting amsRequested to AMSWholeTree as iteration with
                -- current implementation requires whole tree traversal
                (\cA p b f -> liftIO $ reThrowAVLEx @k $ iter cA p b f =<<
                  atomically (retrieveAMS var recForProof AMSWholeTree ))
        daaM = DbAccessActionsM daa (\cA cs -> liftIO $ reThrowAVLEx @k $ modAccum cA cs =<< atomically (readTVar var))
        daaU = DbAccessActionsU daaM
                  (withProjUndo . modAccumU)
                  (\cA _cs -> pure . Right . inj . computeUndo cA =<< atomically (readTVar var))

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
                AMSWholeTree -> computeProofWhole oldAvl
                AMSKeys ks   -> computeProofKeys oldAvl ks

        computeProofWhole :: AVL.Map h k v -> AVLCacheT h (ReaderT (AVLPureStorage h ) IO) (AVL.Proof h k v)
        computeProofWhole = fmap AVL.Proof . materialize

        computeProofKeys
            :: AVL.Map h k v
            -> Set k
            -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Proof h k v)
        computeProofKeys tree ks = do
            (avl', allTouched) <- foldM computeTouched (tree, mempty) ks
            AVL.prune (allTouched <> accTouched) =<< materialize avl'

        computeTouched
            :: (AVL.Map h k v, Set h)
            -> k
            -> AVLCacheT h (ReaderT (AVLPureStorage h) IO) (AVL.Map h k v, Set h)
        computeTouched (avl, touched) key = do
            ((_res, touched'), avl') <- AVL.lookup' key avl
            pure (avl', touched' <> touched)

    apply var Nothing = AVL.Proof . mkAVL . amsRootHash <$> atomically (readTVar var)
