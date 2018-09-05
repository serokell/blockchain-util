{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.AVLp.State
       ( AVLServerState (..)
       , AMSRequested (..)
       , ClientTempState (..)
       , clientModeToTempSt
       , reThrowAVLEx
       , AVLPureStorage (..)
       , initAVLPureStorage
       , AVLCache (..)
       , AVLCacheT (..)
       , runAVLCacheT
       , RetrieveF
       , RetrieveImpl (..)
       ) where

import           Universum

import           Data.Default (Default (..))
import qualified Data.Map.Strict as M
import           Data.Tree.AVL (KVStoreMonad (..), MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL

import           Snowdrop.Execution.DbActions.Types (ClientMode (..), DbActionsException (..))
import           Snowdrop.Util (HasGetter (..))

import           Snowdrop.Execution.DbActions.AVLp.Avl (AvlHashable, KVConstraint, RootHash (..),
                                                        deserialiseM, materialize, mkAVL, saveAVL)

----------------------------------------------------------------------------
-- Server state
----------------------------------------------------------------------------

-- | Data type used as state of DbModifyActions.
data AVLServerState h k = AMS
    { amsRootHash  :: RootHash h       -- ^ Root hash of tree kept in storage
    , amsState     :: AVLPureStorage h -- ^ Storage of whole AVL tree (including old nodes)
    , amsRequested :: AMSRequested k
    -- ^ Set of keys that were requested since the last `apply` operation.
    -- Note, that keys which were requested with `RememberForProof False` passed to
    -- `avlServerDbActions` are not being added to this set.
    }

instance HasGetter (AVLServerState h k) (RootHash h) where
    gett = amsRootHash

instance HasGetter (AVLServerState h k) (AVLPureStorage h) where
    gett = amsState


-- | Data type for tracking keys which were requested from access actions.
data AMSRequested k
    = AMSWholeTree
    -- ^ Constructor, identifying that all keys of tree were requested
    -- (which happens with current implementation of iteration)
    | AMSKeys (Set k)
    -- ^ Constructor, containing set of keys that were requested

instance Ord k => Semigroup (AMSRequested k) where
    AMSWholeTree <> _ = AMSWholeTree
    _ <> AMSWholeTree = AMSWholeTree
    AMSKeys s1 <> AMSKeys s2 = AMSKeys $ s1 <> s2

instance Ord k => Monoid (AMSRequested k) where
    mempty = AMSKeys mempty
    mappend = (<>)

----------------------------------------------------------------------------
-- Client state
----------------------------------------------------------------------------

data ClientTempState h k v n = ClientTempState
    { ctRetrieve :: Either (AVLPureStorage h) (RetrieveF h n)
    , ctRootHash :: RootHash h
    }

data ClientError = BrokenProofError | UnexpectedRootHash
    deriving Show

instance Exception ClientError

reThrowAVLEx :: forall k m a . (MonadCatch m, Show k, Typeable k) => m a -> m a
reThrowAVLEx m =
    m `catch` (\(e :: ClientError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.DeserialisationError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.NotFound k) -> throwM $ DbProtocolError $ show e)


clientModeToTempSt
    :: forall h k v m n .
    ( MonadCatch m, AvlHashable h, AVL.Hash h k v, MonadIO n, MonadCatch n
    , KVConstraint k v, Serialisable (MapLayer h k v h))
    => RetrieveF h n
    -> ClientMode (AVL.Proof h k v)
    -> RootHash h
    -> m (ClientTempState h k v n)
clientModeToTempSt _ (ProofMode p@(AVL.Proof avl)) rootH =
    flip ClientTempState rootH <$> fmap Left convert
  where
    convert = do
        when (not $ AVL.checkProof (unRootHash rootH) p) $ throwM BrokenProofError
        AVLPureStorage . unAVLCache . snd  <$>
            runAVLCacheT (saveAVL avl) def (AVLPureStorage @h def)
clientModeToTempSt retrieveF RemoteMode rootH = pure $ ClientTempState (Right retrieveF) rootH

instance HasGetter (ClientTempState h k v n) (RootHash h) where
    gett = ctRootHash

----------------------------------------------------------------------------
-- AVL-storage datatypes
----------------------------------------------------------------------------

-- | Pure implementation of permanent storage
newtype AVLPureStorage h = AVLPureStorage { unAVLPureStorage :: Map h ByteString }

initAVLPureStorage
    :: forall k v m h .
    ( MonadIO m
    , MonadCatch m
    , MonadThrow m
    , KVConstraint k v
    , AvlHashable h
    , AVL.Hash h k v
    , Serialisable (MapLayer h k v h)
    )
    => Map k v -> m (AVLServerState h k)
initAVLPureStorage (M.toList -> kvs) = reThrowAVLEx @k $ do
    (rootH, AVLPureStorage . unAVLCache -> st) <-
      runAVLCacheT
        (foldM (\b (k, v) -> snd <$> AVL.insert @h k v b) AVL.empty kvs >>= saveAVL)
        def (AVLPureStorage @h def)
    fullAVL <- runAVLCacheT @_ @h (materialize @h @k @v $ mkAVL rootH) def st
    putStrLn $ "Built AVL+ tree:\n" <> (AVL.showMap $ fst fullAVL)
    pure $ AMS { amsRootHash = rootH, amsState = st, amsRequested = mempty }

-- | Accumulator for changes emerging from `save` operations
-- being performed on AVL tree
newtype AVLCache h = AVLCache { unAVLCache :: Map h ByteString }
    deriving (Default, Semigroup, Monoid)

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheT h m a = AVLCacheT (StateT (AVLCache h) m a)
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadCatch, MonadState (AVLCache h), MonadTrans)

instance (MonadThrow m, AvlHashable h, RetrieveImpl m h) => KVStoreMonad h (AVLCacheT h m) where
    retrieve k = checkInAccum >>= deserialiseM
      where
        checkInAccum = M.lookup k . unAVLCache <$> get >>= maybe checkInState pure
        checkInState = lift (retrieveImpl k) >>= maybe (throwM $ AVL.NotFound k) pure
    store k v = modify' $ AVLCache . M.insert k (serialise v) . unAVLCache

runAVLCacheT
    :: MonadThrow m
    => AVLCacheT h (ReaderT ctx m) a
    -> AVLCache h
    -> ctx
    -> m (a, AVLCache h)
runAVLCacheT (AVLCacheT ma) initSt ctx = runReaderT (runStateT ma initSt) ctx

----------------------------------------------------------------------------
-- Retrieve monad
----------------------------------------------------------------------------

type RetrieveF h m = h -> m (Maybe ByteString)

-- TODO replace with AVL.KVRetrieveM after this type is introduced into library
class Monad m => RetrieveImpl m h where
    retrieveImpl :: RetrieveF h m

instance Monad m => RetrieveImpl (ReaderT (RetrieveF h m) m) h where
    retrieveImpl k = ask >>= lift . ($ k)

instance (Monad m, AvlHashable h) => RetrieveImpl (ReaderT (AVLServerState h k) m) h where
    retrieveImpl k = asks ( M.lookup k . unAVLPureStorage . gett )

instance (Monad m, AvlHashable h) => RetrieveImpl (ReaderT (AVLPureStorage h) m) h where
    retrieveImpl k = asks ( M.lookup k . unAVLPureStorage )

instance (AvlHashable h, MonadCatch m) => RetrieveImpl (ReaderT (ClientTempState h k v m) m) h where
    retrieveImpl k = asks ctRetrieve >>=
        lift . either (runReaderT $ retrieveImpl k) (runReaderT $ retrieveImpl k)
