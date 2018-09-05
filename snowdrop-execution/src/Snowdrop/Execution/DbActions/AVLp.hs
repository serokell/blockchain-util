{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.AVLp
       (
         AVLCacheT (..)
       , AVLChgAccum
       , avlServerDbActions
       , avlClientDbActions
       , AVLServerState
       , RootHash
       , initAVLPureStorage
       , ClientError (..)
       , deserialiseM
       ) where

import           Universum

import           Control.Monad.Free (Free (Free))
import qualified Data.ByteString as BS
import           Data.Tree.AVL (KVStoreMonad (..), MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable as Buildable
import           Snowdrop.Execution.DbActions.Types (ClientMode (..), DbAccessActions (..),
                                                     DbActionsException (DbProtocolError),
                                                     DbActionsException (..), DbModifyActions (..),
                                                     RememberForProof (..))

import           Snowdrop.Core (CSMappendException (..), ChgAccumModifier (..), IdSumPrefixed (..),
                                Prefix (..), StateP, StateR, Undo (..), ValueOp (..),
                                ValueOpErr (..), changeSetToList, idSumPrefix)
import           Snowdrop.Util (HasGetter (..))

instance Hashable AVL.Tilt
instance Hashable b => Hashable (AVL.WithBounds b)
instance (Hashable h, Hashable k, Hashable v, Hashable s) => Hashable (MapLayer h k v s)

instance Default h => Default (MapLayer h k v s) where
    def = MLEmpty def

type AvlHashable h = (Ord h, Show h, Typeable h, Serialisable h)

-- | Data type for tracking keys which were requested
-- from access actions
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

-- | Data type used as state of `avlStateDbActions`
data AVLServerState h k = AMS
    { amsRootHash  :: RootHash h -- ^ Root hash of tree kept in storage
    , amsState     :: AVLPureStorage h -- ^ Storage of whole AVL tree (including old nodes)
    , amsRequested :: AMSRequested k
    -- ^ Set of keys that were requested since the last `apply` operation.
    -- Note, that keys which were requested with `RememberForProof False` passed to
    -- `avlServerDbActions` are not being added to this set.
    }

newtype RootHash h = RootHash { unRootHash :: h }
    deriving (Eq, Serialisable)

instance HasGetter (AVLServerState h k) (RootHash h) where
    gett = amsRootHash

instance HasGetter (AVLServerState h k) (AVLPureStorage h) where
    gett = amsState

-- | Pure implementation of permanent storage
newtype AVLPureStorage h = AVLPureStorage { unAVLPureStorage :: Map h ByteString }

-- | Accumulator for changes emerging from `save` operations
-- being performed on AVL tree
newtype AVLCache h = AVLCache { unAVLCache :: Map h ByteString }
    deriving (Default, Semigroup, Monoid)

-- | Monad transformer for caching `save` operations resulting from AVL+ actions
newtype AVLCacheT h m a = AVLCacheT (StateT (AVLCache h) m a)
    deriving (Functor, Applicative, Monad, MonadThrow,
              MonadCatch, MonadState (AVLCache h), MonadTrans)

runAVLCacheT
    :: MonadThrow m
    => AVLCacheT h (ReaderT ctx m) a
    -> AVLCache h
    -> ctx
    -> m (a, AVLCache h)
runAVLCacheT (AVLCacheT ma) initSt ctx = runReaderT (runStateT ma initSt) ctx

instance (Show h, Show k, Show v) => Buildable (AVL.Map h k v) where
    build = Buildable.build . AVL.showMap

instance (Show h, Show k, Show v) => Buildable (AVL.Proof h k v) where
    build (AVL.Proof tree) = Buildable.build tree

deserialiseM :: (MonadThrow m, Serialisable v) => ByteString -> m v
deserialiseM =
    either (throwM . DbProtocolError . ("Deserialisation error "<>) . toText) pure . deserialise

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

instance (MonadThrow m, AvlHashable h, RetrieveImpl m h) => KVStoreMonad h (AVLCacheT h m) where
    retrieve k = checkInAccum >>= deserialiseM
      where
        checkInAccum = M.lookup k . unAVLCache <$> get >>= maybe checkInState pure
        checkInState = lift (retrieveImpl k) >>= maybe (throwM $ AVL.NotFound k) pure
    store k v = modify' $ AVLCache . M.insert k (serialise v) . unAVLCache

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

saveAVL :: forall h k v m . (AVL.Stores h k v m, MonadCatch m) => AVL.Map h k v -> m (RootHash h)
saveAVL avl = AVL.save avl $> avlRootHash avl

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

materialize :: forall h k v m . AVL.Stores h k v m => AVL.Map h k v -> m (AVL.Map h k v)
materialize initAVL = flip AVL.openAndM initAVL $ \case
    MLBranch h m c t l r -> fmap Free $ MLBranch h m c t <$> materialize l <*> materialize r
    rest -> pure $ Free rest

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

type KVConstraint k v = (IdSumPrefixed k, Typeable k, Ord k, Show k,
                         Serialisable k, Show v)

instance (AvlHashable h, MonadCatch m) => RetrieveImpl (ReaderT (ClientTempState h k v m) m) h where
    retrieveImpl k = asks ctRetrieve >>=
        lift . either (runReaderT $ retrieveImpl k) (runReaderT $ retrieveImpl k)

data ClientTempState h k v n = ClientTempState
    { ctRetrieve :: Either (AVLPureStorage h) (RetrieveF h n)
    , ctRootHash :: RootHash h
    }

data ClientError = BrokenProofError | UnexpectedRootHash
    deriving Show

instance Exception ClientError

clientModeToTempSt
    :: forall h k v m n .
    ( MonadCatch m, AvlHashable h, AVL.Hash h k v, MonadIO n, MonadCatch n
    , KVConstraint k v, Serialisable (MapLayer h k v h))
    => RetrieveF h n -> ClientMode (AVL.Proof h k v) -> RootHash h -> m (ClientTempState h k v n)
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

reThrowAVLEx :: forall k m a . (MonadCatch m, Show k, Typeable k) => m a -> m a
reThrowAVLEx m =
    m `catch` (\(e :: ClientError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.DeserialisationError) -> throwM $ DbProtocolError $ show e)
      `catch` (\(e :: AVL.NotFound k) -> throwM $ DbProtocolError $ show e)

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

mkAVL :: RootHash h -> AVL.Map h k v
mkAVL = pure . unRootHash

avlRootHash :: AVL.Map h k v -> RootHash h
avlRootHash = RootHash . AVL.rootHash

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
            `catch` \e@(CSMappendException _ _) -> pure $ Left e
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
            (Upd f      ,Just oldVal) -> do
              newVal <- case f oldVal of
                Right res -> return res
                Left e    -> throwM $ CSMappendException k e
              (, touched') . snd <$> AVL.insert' k newVal avl'
            _                    -> throwM $ CSMappendException k (ValueOpErr "modAccum: AVL keys corrupted")

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
