{-# LANGUAGE DataKinds           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.AVLp.State
       ( AVLServerState (..)
       , AMSRequested (..)
       , ClientTempState (..)
       , clientModeToTempSt
       , ClientError (..)
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
import qualified Data.Set as S
import           Data.Tree.AVL (KVStoreMonad (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL
import           Data.Vinyl.Core (Rec (..))
import           Loot.Log (MonadLogging, logDebug)

import           Snowdrop.Execution.DbActions.AVLp.Avl (AllAvlEntries, AvlHashable, AvlProof (..),
                                                        AvlProofs, IsAvlEntry, RootHash (..),
                                                        RootHashComp (..), RootHashes, deserialiseM,
                                                        materialize, mkAVL, saveAVL)
import           Snowdrop.Execution.DbActions.Types (ClientMode (..), DbActionsException (..))
import           Snowdrop.Hetero (HKey, HMap, HVal, unHMapEl)
import           Snowdrop.Util (HasGetter (..))

----------------------------------------------------------------------------
-- Server state
----------------------------------------------------------------------------

-- | Data type used as state of DbModifyActions.
data AVLServerState h xs = AMS
    { amsRootHashes :: RootHashes h xs  -- ^ Root hash of tree kept in storage
    , amsState      :: AVLPureStorage h -- ^ Storage of whole AVL tree (including old nodes)
    , amsRequested  :: Rec AMSRequested xs
    -- ^ Set of keys that were requested since the last `apply` operation.
    -- Note, that keys which were requested with `RememberForProof False` passed to
    -- `avlServerDbActions` are not being added to this set.
    }

instance HasGetter (AVLServerState h xs) (RootHashes h xs) where
    gett = amsRootHashes

instance HasGetter (AVLServerState h xs) (AVLPureStorage h) where
    gett = amsState


-- | Data type for tracking keys which were requested from access actions.
data AMSRequested t
    = AMSWholeTree
    -- ^ Constructor, identifying that all keys of tree were requested
    -- (which happens with current implementation of iteration)
    | AMSKeys (Set (HKey t))
    -- ^ Constructor, containing set of keys that were requested

instance Default (AMSRequested t) where
    def = AMSKeys S.empty

instance Ord (HKey t) => Semigroup (AMSRequested t) where
    AMSWholeTree <> _ = AMSWholeTree
    _ <> AMSWholeTree = AMSWholeTree
    AMSKeys s1 <> AMSKeys s2 = AMSKeys $ s1 <> s2

instance Ord (HKey t) => Monoid (AMSRequested t) where
    mempty = AMSKeys mempty
    mappend = (<>)

----------------------------------------------------------------------------
-- Client state
----------------------------------------------------------------------------

data ClientTempState h xs n = ClientTempState
    { ctRetrieve   :: Either (AVLPureStorage h) (RetrieveF h n)
    , ctRootHashes :: RootHashes h xs
    }

data ClientError = BrokenProofError | UnexpectedRootHash
    deriving Show

instance Exception ClientError

reThrowAVLEx :: forall k h m a . (MonadCatch m, Show h, Typeable h, Show k, Typeable k) => m a -> m a
reThrowAVLEx m =
    m `catch` (\(e :: ClientError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.DeserialisationError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.NotFound k) -> throwM $ DbProtocolError $ "Not found key: " <> show e)
      `catch` (\(e :: AVL.NotFound h) -> throwM $ DbProtocolError $ "Not found hash: " <> show e)

clientModeToTempSt
    :: forall h xs m n .
    ( AvlHashable h
    , MonadCatch m, MonadIO n, MonadCatch n
    , AllAvlEntries h xs
    )
    => RetrieveF h n
    -> ClientMode (AvlProofs h xs)
    -> RootHashes h xs
    -> m (ClientTempState h xs n)
clientModeToTempSt _ (ProofMode proofs') rootHashes =
    flip ClientTempState rootHashes <$> fmap Left (convertAll proofs' rootHashes def)
  where
    convertAll
        :: AllAvlEntries h rs
        => AvlProofs h rs
        -> RootHashes h rs
        -> AVLPureStorage h
        -> m (AVLPureStorage h)
    convertAll RNil RNil res          = pure res
    convertAll (p :& proofs) (RootHashComp rootH :& roots) cache =
        convert p rootH cache >>= convertAll proofs roots

    convert :: forall r . IsAvlEntry h r => AvlProof h r -> RootHash h -> AVLPureStorage h -> m (AVLPureStorage h)
    convert (AvlProof p@(AVL.Proof avl)) rootH cache = reThrowAVLEx @(HKey r) @h $ do
        when (not $ AVL.checkProof (unRootHash rootH) p) $ throwM BrokenProofError
        AVLPureStorage . unAVLCache . snd  <$> runAVLCacheT (saveAVL avl) def cache
clientModeToTempSt retrieveF RemoteMode rootH = pure $ ClientTempState (Right retrieveF) rootH

instance HasGetter (ClientTempState h xs n) (RootHashes h xs) where
    gett = ctRootHashes

----------------------------------------------------------------------------
-- AVL-storage datatypes
----------------------------------------------------------------------------

-- | Pure implementation of permanent storage
newtype AVLPureStorage h = AVLPureStorage { unAVLPureStorage :: Map h ByteString }
instance Default (AVLPureStorage h) where
    def = AVLPureStorage def

initAVLPureStorage
    :: forall xs h m .
    ( MonadIO m, MonadCatch m, MonadLogging m
    , AvlHashable h
    , AllAvlEntries h xs
    )
    => HMap xs
    -> m (AVLServerState h xs)
initAVLPureStorage xs = initAVLPureStorageAll xs def
  where
    initAVLPureStorageAll
        :: forall rs . AllAvlEntries h rs
        => HMap rs
        -> AVLPureStorage h
        -> m (AVLServerState h rs)
    initAVLPureStorageAll RNil cache        = pure $ AMS RNil cache RNil
    initAVLPureStorageAll rs@(_ :& _) cache = initAVLPureStorage' rs cache

    initAVLPureStorage'
        :: forall rs r rs' . (rs ~ (r ': rs'), AllAvlEntries h rs)
        => HMap rs
        -> AVLPureStorage h
        -> m (AVLServerState h rs)
    initAVLPureStorage' ((M.toList . unHMapEl -> kvs) :& accums) cache = reThrowAVLEx @(HKey r) @h $ do
        logDebug "Initializing AVL+ pure storage"
        (rootH, unAVLCache -> cache') <-
            runAVLCacheT
                (foldM (\b (k, v) -> snd <$> AVL.insert @h k v b) AVL.empty kvs >>= saveAVL)
                def
                cache
        let newCache = AVLPureStorage $ unAVLPureStorage cache <> cache'
        logDebug "Materializing AVL+ pure storage"
        fullAVL <-
            runAVLCacheT @_ @h
                (materialize @h @(HKey r) @(HVal r) $ mkAVL rootH)
                def
                newCache
        logDebug . fromString $ "Built AVL+ tree:\n" <> (AVL.showMap $ fst fullAVL)
        AMS{..} <- initAVLPureStorageAll accums newCache
        pure $ AMS (RootHashComp rootH :& amsRootHashes) amsState (def :& amsRequested)

-- | Accumulator for changes emerging from `save` operations
-- being performed on AVL tree
newtype AVLCache h = AVLCache { unAVLCache :: Map h ByteString }
    deriving (Default, Semigroup, Monoid)

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheT h m a = AVLCacheT (StateT (AVLCache h) m a)
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadCatch, MonadState (AVLCache h), MonadTrans)

instance (MonadThrow m, AvlHashable h, RetrieveImpl m h)
         => KVStoreMonad h (AVLCacheT h m) where
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

instance (AvlHashable h, MonadCatch m) => RetrieveImpl (ReaderT (ClientTempState h xs m) m) h where
    retrieveImpl k = asks ctRetrieve >>=
        lift . either (runReaderT $ retrieveImpl k) (runReaderT $ retrieveImpl k)
