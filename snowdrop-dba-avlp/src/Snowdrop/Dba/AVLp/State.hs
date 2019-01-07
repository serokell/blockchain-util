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
       , AVLCacheEl
       , AVLCacheElT
       , runAVLCacheElT
       , asAVLCache
       , upcastAVLCache
       , upcastAVLCache'
       ) where

import           Data.Vinyl (RecPointed (..), RecSubset, rcast, rget, rput, rreplace)
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
                                        deserialiseM, saveAVL)
import           Snowdrop.Dba.Base (ClientMode (..), DbActionsException (..))
import           Snowdrop.Hetero (HDownCastable, HKey, HMap, HVal, RContains, unHMapEl)
import           Snowdrop.Util (HasGetter (..), Serialisable (..))

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

clientModeToTempSt
    :: forall h xs m n .
    ( AvlHashable h
    , MonadCatch m, MonadCatch n
    , AllAvlEntries h xs
    )
    => RetrieveF h n xs
    -> ClientMode (AvlProofs h xs)
    -> RootHashes h xs
    -> m (ClientTempState h xs n)
clientModeToTempSt _ (ProofMode proofs') rootHashes = undefined
    flip ClientTempState rootHashes <$> fmap Left (convertAll proofs' rootHashes def)
  where
    convertAll
        :: AllAvlEntries h rs
        => AvlProofs h rs
        -> RootHashes h rs
        -> AVLPureStorage h rs
        -> m (AVLPureStorage h rs)
    convertAll RNil RNil res                                     = pure res
    convertAll (p :& proofs) (RootHashComp rootH :& roots) cache = undefined
        -- convert p rootH cache >>= convertAll proofs roots

    -- convert :: forall r . IsAvlEntry h r => AvlProof h r -> RootHash h -> Map (HKey r) (HVal r) -> m (Map (HKey r) (HVal r))
    -- convert (AvlProof p@(AVL.Proof avl)) rootH cache = reThrowAVLEx @(HKey r) @h $ do
    --     when (not $ AVL.checkProof (unRootHash rootH) p) $ throwM BrokenProofError
    --     AVLPureStorage . unAVLCache . snd <$> runAVLCacheT (saveAVL avl) def cache

clientModeToTempSt retrieveF RemoteMode rootH = pure $ ClientTempState (Right retrieveF) rootH

instance HasGetter (ClientTempState h xs n) (RootHashes h xs) where
    gett = ctRootHashes

----------------------------------------------------------------------------
-- AVL-storage datatypes
----------------------------------------------------------------------------

newtype AVLPureStorage h xs = AVLPureStorage { unAVLPureStorage :: Rec (AVLCacheEl h) xs }

instance RecPointed Default (AVLCacheEl h) xs

instance RecPointed Default f ts => Default (Rec f ts) where
  def = rpointMethod @Default def

instance Default (AVLPureStorage h xs) where
    def = AVLPureStorage (def)

initAVLPureStorage
    :: forall xs h m .
    ( MonadIO m, MonadCatch m, MonadLogging m
    , AvlHashable h
    , AllAvlEntries h xs
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
    initAVLPureStorageAll RNil RNil = pure $ AMS RNil (AVLPureStorage RNil) RNil
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

-- newtype AVLCacheEl h x = AVLCacheEl { unAVLCacheEl :: Map (HKey x) (HVal x) } deriving Default
newtype AVLCacheEl h x = AVLCacheEl { unAVLCacheEl :: Map h (MapLayer h (HKey x) (HVal x) h) } deriving (Default, Semigroup)

deriving instance (Ord (HKey x), Ord h) => Monoid (AVLCacheEl h x)

newtype AVLCache h xs = AVLCache { unAVLCache :: Rec (AVLCacheEl h) xs }

fromPureStorate :: AVLPureStorage h xs -> AVLCache h xs
fromPureStorate = AVLCache . unAVLPureStorage

toPureStorate :: AVLCache h xs -> AVLPureStorage h xs
toPureStorate = AVLPureStorage . unAVLCache

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheT h m xs a = AVLCacheT (StateT (AVLCache h xs) m a)
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadCatch, MonadState (AVLCache h xs))

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheElT h m x a = AVLCacheElT (StateT (AVLCacheEl h x) m a)
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadCatch, MonadState (AVLCacheEl h x))

deriving instance MonadIO m => MonadIO (AVLCacheT h m xs)
deriving instance MonadIO m => MonadIO (AVLCacheElT h m x)

instance ( MonadThrow m
         , AvlHashable h
         , RContains xs x
         , k ~ HKey x
         , v ~ HVal x
         )
         => AVL.KVRetrieve h (AVL.MapLayer h k v h) (AVLCacheT h m xs) where
    retrieve :: h -> AVLCacheT h m xs (AVL.MapLayer h k v h)
    retrieve h = do cache <- rget @x . unAVLCache <$> get
                    undefined

instance ( MonadThrow m
         , AvlHashable h
         , k ~ HKey x
         , v ~ HVal x
         )
         => AVL.KVRetrieve h (AVL.MapLayer h k v h) (AVLCacheElT h m x) where
    retrieve :: h -> AVLCacheElT h m x (AVL.MapLayer h k v h)
    retrieve h = do storage <- unAVLCacheEl <$> get
                    undefined

instance ( MonadThrow m
         , AvlHashable h
         , k ~ HKey x
         , v ~ HVal x
         )
         => AVL.KVStore h (AVL.MapLayer h k v h) (AVLCacheElT h m x)

store :: ( MonadThrow m
         , AvlHashable h
         , RContains xs x
         , AVL.KVRetrieve h (AVL.MapLayer h k v h) m
         , k ~ HKey x
         , v ~ HVal x
         )
      => (h, MapLayer h k v h) -> AVLCacheT h m xs ()
store (k, v) = undefined

runAVLCacheT
    :: MonadThrow m
    => AVLCacheT h (ReaderT ctx m) xs a
    -> AVLCache h xs
    -> ctx
    -> m (a, AVLCache h xs)
runAVLCacheT (AVLCacheT ma) initSt ctx = runReaderT (runStateT ma initSt) ctx

runAVLCacheElT
    :: MonadThrow m
    => AVLCacheElT h (ReaderT ctx m) x a
    -> AVLCacheEl h x
    -> ctx
    -> m (a, AVLCacheEl h x)
runAVLCacheElT (AVLCacheElT ma) initSt ctx = runReaderT (runStateT ma initSt) ctx

asAVLCache :: forall x xs h m a . Monad m => AVLCacheElT h m x a -> AVLCacheT h m (x ': xs) a
asAVLCache (AVLCacheElT f) =
    AVLCacheT $ do (AVLCache cache) <- get
                   (a, s) <- lift $ runStateT f (rget @x cache)
                   put (AVLCache $ rput @x s cache)
                   return a

-- asAVLCache :: forall x xs h m a . Monad m => AVLCacheElT h m x a -> AVLCacheT h m (x ': xs) a
-- asAVLCache (AVLCacheElT f) =
--     AVLCacheT $ do (AVLCache cache) <- get
--                    (a, s) <- lift $ runStateT f (rget @x cache)
--                    put (AVLCache $ rput @x s cache)
--                    return a

upcastAVLCache' :: forall x xs h m a
                . Monad m
                => AVLCacheT h m xs a
                -> AVLCacheT h m (x ': xs) a
upcastAVLCache' (AVLCacheT f) =
    AVLCacheT $ do (AVLCache (c :& cs)) <- get
                   (a, AVLCache s) <- lift $ runStateT f (AVLCache cs)
                   put (AVLCache $ c :& s)
                   return a

upcastAVLCache :: forall big small h m a
               . ( Monad m
                 , RecSubset Rec small big (RImage small big)
                 )
               => AVLCacheT h m small a
               -> AVLCacheT h m big   a
upcastAVLCache (AVLCacheT f) =
    AVLCacheT $ do (AVLCache cache) <- get
                   (a, AVLCache s) <- lift $ runStateT f (AVLCache $ rcast cache)
                   put (AVLCache $ rreplace s cache)
                   return a

----------------------------------------------------------------------------
-- Retrieve monad
----------------------------------------------------------------------------

newtype RetrieveEl h m x =
    RetrieveEl { unRetrieveEl :: h -> m (AVL.MapLayer h  (HKey x) (HVal x) h) }

type RetrieveF h m xs = Rec (RetrieveEl h m) xs

-- TODO replace with AVL.KVRetrieveM after this type is introduced into library
-- class Monad m => RetrieveImpl m h where
--     retrieveImpl :: RetrieveF h m

-- instance Monad m => RetrieveImpl (ReaderT (RetrieveF h m) m) h where
--     retrieveImpl k = ask >>= lift . ($ k)

-- instance (Monad m, AvlHashable h) => RetrieveImpl (ReaderT (AVLServerState h k) m) h where
--     retrieveImpl k = asks ( M.lookup k . unAVLPureStorage . gett )

-- instance (Monad m, AvlHashable h) => RetrieveImpl (ReaderT (AVLPureStorage h xs) m) h where
--     retrieveImpl k = asks ( M.lookup k . unAVLPureStorage )

-- instance (AvlHashable h, MonadCatch m) => RetrieveImpl (ReaderT (ClientTempState h xs m) m) h where
--     retrieveImpl k = asks ctRetrieve >>=
--         lift . either (runReaderT $ retrieveImpl k) (runReaderT $ retrieveImpl k)
