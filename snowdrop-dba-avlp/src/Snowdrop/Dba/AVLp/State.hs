{-# LANGUAGE DataKinds           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.AVLp.State
       ( AVLServerState (..)
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
       , RetrieveEl(..)
       , AVLCacheEl(..)
       , AVLCacheElT
       , runAVLCacheElT
       , asAVLCache
       , upcastAVLCache
       , upcastAVLCache'
       , RetrieveImpl (..)
       ) where

import           Data.Vinyl (RecSubset, rcast, rget, rput, rreplace)
import           Data.Vinyl.TypeLevel (RImage)
import           Universum

import           Data.Default (Default (..))
import qualified Data.Map.Strict as M
import           Data.Tree.AVL (MapLayer)
import qualified Data.Tree.AVL as AVL
import           Data.Vinyl.Core (Rec (..))
import           Loot.Log (MonadLogging, logDebug)
import           Snowdrop.Dba.AVLp.Avl (AllAvlEntries, AvlHashable, AvlProof (..), AvlProofs,
                                        IsAvlEntry, RootHash (..), RootHashComp (..), RootHashes,
                                        saveAVL)
import           Snowdrop.Dba.Base (ClientMode (..), DbActionsException (..))
import           Snowdrop.Hetero (HKey, HMap, HVal, RContains, unHMapEl)
import           Snowdrop.Util (HasGetter (..))

----------------------------------------------------------------------------
-- Server state
----------------------------------------------------------------------------

-- | Data type used as state of DbModifyActions.
data AVLServerState h xs = AMS
    { amsRootHashes :: RootHashes h xs  -- ^ Root hash of tree kept in storage
    , amsState      :: AVLPureStorage h xs -- ^ Storage of whole AVL tree (including old nodes)
    , amsVisited    :: Rec (Const (Set h)) xs
    -- ^ Set of nodes visited since the last `apply` operation.
    -- Note, that nodes which were requested with `RememberForProof False` passed to
    -- `avlServerDbActions` are not being added to this set.
    }

instance HasGetter (AVLServerState h xs) (RootHashes h xs) where
    gett = amsRootHashes

instance HasGetter (AVLServerState h xs) (AVLPureStorage h xs) where
    gett = amsState

----------------------------------------------------------------------------
-- Client state
----------------------------------------------------------------------------

data ClientTempState h xs n = ClientTempState
    { ctRetrieve   :: Either (AVLPureStorage h xs) (RetrieveF h n xs)
    , ctRootHashes :: RootHashes h xs
    }

data ClientError = BrokenProofError | UnexpectedRootHash
    deriving Show

instance Exception ClientError

reThrowAVLEx :: forall k h m a . (MonadCatch m, Show h, Typeable h, Show k, Typeable k) => m a -> m a
reThrowAVLEx m =
    m `catch` (\(e :: ClientError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.NotFound k) -> throwM $ DbProtocolError $ "Not found key: " <> show e)
      `catch` (\(e :: AVL.NotFound h) -> throwM $ DbProtocolError $ "Not found hash: " <> show e)

class    Default (AVLCacheEl h x) => DefaultCacheEl h x
instance Default (AVLCacheEl h x) => DefaultCacheEl h x

clientModeToTempSt
    :: forall h xs m n .
    ( AvlHashable h
    , MonadCatch m, MonadCatch n
    , AllAvlEntries h xs

    , Default (Rec (AVLCacheEl h) xs)
    )
    => RetrieveF h n xs
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
        -> AVLPureStorage h rs
        -> m (AVLPureStorage h rs)
    convertAll RNil RNil res                                     = pure res
    convertAll a@(_ :& _) b@(_ :& _) c@(AVLPureStorage (_ :& _)) = convertAll' a b c

    convertAll'
        :: AllAvlEntries h (r ': rs)
        => AvlProofs h (r ': rs)
        -> RootHashes h (r ': rs)
        -> AVLPureStorage h (r ': rs)
        -> m (AVLPureStorage h (r ': rs))
    convertAll' (p :& proofs) (RootHashComp rootH :& roots) (AVLPureStorage (c :& cs)) = do
        r <- convert p rootH c
        rs <- convertAll proofs roots (AVLPureStorage cs)
        pure (AVLPureStorage (r :& unAVLPureStorage rs))

    convert :: forall r . IsAvlEntry h r => AvlProof h r -> RootHash h -> AVLCacheEl h r -> m (AVLCacheEl h r)
    convert (AvlProof p@(AVL.Proof avl)) rootH cache = reThrowAVLEx @(HKey r) @h $ do
        when (not $ AVL.checkProof (unRootHash rootH) p) $ throwM BrokenProofError
        snd <$> runAVLCacheElT (saveAVL avl) def cache

clientModeToTempSt retrieveF RemoteMode rootH = pure $ ClientTempState (Right retrieveF) rootH

instance HasGetter (ClientTempState h xs n) (RootHashes h xs) where
    gett = ctRootHashes

----------------------------------------------------------------------------
-- AVL-storage datatypes
----------------------------------------------------------------------------

newtype AVLPureStorage h xs = AVLPureStorage { unAVLPureStorage :: Rec (AVLCacheEl h) xs }

instance Default (Rec (AVLCacheEl h) xs) => Default (AVLPureStorage h xs) where
    def = AVLPureStorage def

initAVLPureStorage
    :: forall xs h m .
    ( MonadIO m, MonadCatch m, MonadLogging m
    , AvlHashable h
    , AllAvlEntries h xs

    , Default (Rec (AVLCacheEl h) xs)
    )
    => HMap xs
    -> m (AVLServerState h xs)
initAVLPureStorage xs = initAVLPureStorageAll xs (unAVLPureStorage def)
  where
    initAVLPureStorageAll
        :: forall rs . AllAvlEntries h rs
        => HMap rs
        -> Rec (AVLCacheEl h) rs -- AVLPureStorage h rs
        -> m (AVLServerState h rs)
    initAVLPureStorageAll RNil RNil               = pure $ AMS RNil (AVLPureStorage RNil) RNil
    initAVLPureStorageAll rs@(_ :& _) cs@(_ :& _) = initAVLPureStorage' rs cs

    initAVLPureStorage'
        :: forall rs r rs' . (rs ~ (r ': rs'), AllAvlEntries h rs)
        => HMap rs
        -> Rec (AVLCacheEl h) rs -- AVLPureStorage h rs
        -> m (AVLServerState h rs)
    initAVLPureStorage' ((M.toList . unHMapEl -> kvs) :& accums) (cache :& restCache) = reThrowAVLEx @(HKey r) @h $ do
        logDebug "Initializing AVL+ pure storage"
        ((tree, rootH), cache') <-
            let body = do t <- AVL.fromList kvs
                          h <- saveAVL t
                          pure (t, h)
            in runAVLCacheElT body def cache
        logDebug . fromString $ "Built AVL+ tree: " <> show rootH <> "\n" <> (AVL.showMap tree)
        AMS{..} <- initAVLPureStorageAll accums restCache
        let newCache = AVLPureStorage $ cache' :& (unAVLPureStorage amsState)
        pure $ AMS (RootHashComp rootH :& amsRootHashes) newCache (mempty :& amsVisited)

-- | Accumulator for changes emerging from `save` operations
-- being performed on AVL tree

newtype AVLCacheEl h x = AVLCacheEl { unAVLCacheEl :: Map h (MapLayer h (HKey x) (HVal x) h) } deriving (Semigroup)

instance Default (AVLCacheEl h x) where
    def = AVLCacheEl M.empty

deriving instance (Ord (HKey x), Ord h) => Monoid (AVLCacheEl h x)

newtype AVLCache h xs = AVLCache { unAVLCache :: Rec (AVLCacheEl h) xs }

instance Default (Rec (AVLCacheEl h) xs) => Default (AVLCache h xs) where
    def = AVLCache def

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheT h xs m a = AVLCacheT (StateT (AVLCache h xs) m a)
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadCatch, MonadState (AVLCache h xs), MonadTrans)

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheElT h x m a = AVLCacheElT (StateT (AVLCacheEl h x) m a)
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadCatch, MonadState (AVLCacheEl h x), MonadTrans)

deriving instance MonadIO m => MonadIO (AVLCacheT h xs m)
deriving instance MonadIO m => MonadIO (AVLCacheElT h x m)

instance ( MonadThrow m
         , AvlHashable h
         , RContains xs x
         , k ~ HKey x
         , v ~ HVal x
         , RetrieveImpl h m x
         )
         => AVL.KVRetrieve h (AVL.MapLayer h k v h) (AVLCacheT h xs m) where
    retrieve :: h -> AVLCacheT h xs m (AVL.MapLayer h k v h)
    -- TODO: refactor using Maybe monad
    retrieve h = do cache <- rget @x . unAVLCache <$> get
                    case (M.lookup h (unAVLCacheEl cache)) of
                        Nothing -> do mx <- lift (retrieveImpl @_ @_ @x h)
                                      case mx of
                                          Nothing -> throwM $ DbProtocolError $ "Not found key: " <> show h
                                          Just x -> pure x
                        Just x  -> pure x

instance ( MonadThrow m
         , AvlHashable h
         , k ~ HKey x
         , v ~ HVal x
         , RetrieveImpl h m x
         )
         => AVL.KVRetrieve h (AVL.MapLayer h k v h) (AVLCacheElT h x m) where
    retrieve :: h -> AVLCacheElT h x m (AVL.MapLayer h k v h)
    retrieve h = do cache <- get
                    case M.lookup h (unAVLCacheEl cache) of
                        Nothing -> do mx <- lift (retrieveImpl @_ @_ @x h)
                                      case mx of
                                          Nothing -> throwM $ DbProtocolError $ "Not found key: " <> show h
                                          Just x -> pure x
                        Just x  -> pure x

instance ( MonadThrow m
         , AvlHashable h
         , k ~ HKey x
         , v ~ HVal x
         )
         => AVL.KVStore h (AVL.MapLayer h k v h) (AVLCacheElT h x m) where
    massStore :: [(h, MapLayer h k v h)] -> AVLCacheElT h x m ()
    massStore arr = do cache :: Map h (MapLayer h k v h) <- unAVLCacheEl <$> get
                       let res = foldl' (\acc (k, v) -> M.insert k v acc) cache arr
                       put (AVLCacheEl res)

instance ( MonadThrow m
         , AvlHashable h
         , RContains xs x
         , k ~ HKey x
         , v ~ HVal x
         )
         => AVL.KVStore h (AVL.MapLayer h k v h) (AVLCacheT h xs m) where
    massStore :: [(h, MapLayer h k v h)] -> AVLCacheT h xs m ()
    massStore arr = do cache <- unAVLCache <$> get
                       let cacheEl :: Map h (MapLayer h k v h) = unAVLCacheEl . rget @x $ cache
                       let resEl :: Map h (MapLayer h k v h) = foldl' (\acc (k, v) -> M.insert k v acc) cacheEl arr
                       let res = rput @x (AVLCacheEl resEl) cache
                       put (AVLCache res)

runAVLCacheT
    :: MonadThrow m
    => AVLCacheT h xs (ReaderT ctx m) a
    -> AVLCache h xs
    -> ctx
    -> m (a, AVLCache h xs)
runAVLCacheT (AVLCacheT ma) initSt ctx = runReaderT (runStateT ma initSt) ctx

runAVLCacheElT
    :: MonadThrow m
    => AVLCacheElT h x (ReaderT ctx m) a
    -> AVLCacheEl h x
    -> ctx
    -> m (a, AVLCacheEl h x)
runAVLCacheElT (AVLCacheElT ma) initSt ctx = runReaderT (runStateT ma initSt) ctx

asAVLCache :: forall x xs h m a . Monad m => AVLCacheElT h x m a -> AVLCacheT h (x ': xs) m a
asAVLCache (AVLCacheElT f) =
    AVLCacheT $ do (AVLCache cache) <- get
                   (a, s) <- lift $ runStateT f (rget @x cache)
                   put (AVLCache $ rput @x s cache)
                   return a

upcastAVLCache' :: forall x xs h m a
                . Monad m
                => AVLCacheT h xs m a
                -> AVLCacheT h (x ': xs) m a
upcastAVLCache' (AVLCacheT f) =
    AVLCacheT $ do (AVLCache (c :& cs)) <- get
                   (a, AVLCache s) <- lift $ runStateT f (AVLCache cs)
                   put (AVLCache $ c :& s)
                   return a

upcastAVLCache :: forall big small h m a
               . ( Monad m
                 , RecSubset Rec small big (RImage small big)
                 )
               => AVLCacheT h small m a
               -> AVLCacheT h big   m a
upcastAVLCache (AVLCacheT f) =
    AVLCacheT $ do (AVLCache cache) <- get
                   (a, AVLCache s) <- lift $ runStateT f (AVLCache $ rcast cache)
                   put (AVLCache $ rreplace s cache)
                   return a

----------------------------------------------------------------------------
-- Retrieve monad
----------------------------------------------------------------------------

newtype RetrieveEl m h x = RetrieveEl { runRetrieveEL :: h -> m (Maybe (AVL.MapLayer h (HKey x) (HVal x) h)) }

type RetrieveF h m xs    = Rec (RetrieveEl m h) xs

-- TODO replace with AVL.KVRetrieveM after this type is introduced into library
class Monad m => RetrieveImpl h m x where
    retrieveImpl :: h -> m (Maybe (AVL.MapLayer h (HKey x) (HVal x) h))

instance (Monad m, RContains xs x) => RetrieveImpl h (ReaderT (RetrieveF h m xs) m) x where
    retrieveImpl h = ask >>= lift . (\t -> runRetrieveEL t h) . rget @x

instance (Monad m, Ord h) => RetrieveImpl h (ReaderT (AVLCacheEl h x) m) x where
    retrieveImpl h = M.lookup h . unAVLCacheEl <$> ask

instance (Monad m, AvlHashable h, RContains xs x) => RetrieveImpl h (ReaderT (AVLServerState h xs) m) x where
    retrieveImpl h = asks ( M.lookup h . unAVLCacheEl . rget @x . unAVLPureStorage . amsState)

instance (Monad m, AvlHashable h, RContains xs x) => RetrieveImpl h (ReaderT (AVLPureStorage h xs) m) x where
    retrieveImpl h = asks ( M.lookup h . unAVLCacheEl . rget @x . unAVLPureStorage)

instance (AvlHashable h, MonadCatch m, RContains xs x) => RetrieveImpl h (ReaderT (ClientTempState h xs m) m) x where
    retrieveImpl k = asks ctRetrieve >>=
        lift . either (runReaderT $ retrieveImpl @_ @_ @x k) (runReaderT $ retrieveImpl @_ @_ @x k)
