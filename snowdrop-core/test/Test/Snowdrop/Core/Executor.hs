{-# LANGUAGE ScopedTypeVariables #-}
module Test.Snowdrop.Core.Executor
       ( TestExecutorT
       , TestCtx
       , runERoComp
       , countERoComp
       , Counter (..)
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError, catchError, throwError)
import qualified Data.ByteString as BS
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M

import           Loot.Log (NameSelector (..))
import           Snowdrop.Core (CSMappendException (..), ChangeSet (..), ChgAccum, ChgAccumCtx (..),
                                ChgAccumModifier (..), DbAccess (..), ERoComp, Effectful (..),
                                FoldF (..), IdSumPrefixed (..), StateP, Undo (..), ValueOp (..),
                                changeSetToList, getCAOrDefault, mappendChangeSet, unBaseM)
import           Snowdrop.Util

-- | SumChangeSet holds some change set which is sum of several ChangeSet
-- Copy-pasted from snowdrop-execution in order to avoid bad dependency between packages
newtype SumChangeSet id value = SumChangeSet (ChangeSet id value)
    deriving Show
instance Default (SumChangeSet id value) where
    def = SumChangeSet def

simpleStateAccessor
    :: (Ord id, IdSumPrefixed id)
    => StateP id value
    -> DbAccess (SumChangeSet id value) id value res
    -> res
simpleStateAccessor st (DbQuery q cont) = cont (st `M.intersection` toDummyMap q)
simpleStateAccessor st (DbIterator prefix (FoldF (e, foldf, applier))) = applier $
    foldr
      foldf
      e
      (M.toList $ M.filterWithKey (\i _ -> idSumPrefix i == prefix) st)
simpleStateAccessor st (DbModifyAccum (SumChangeSet acc) cam cont) =
    cont $ liftA2 (,) (SumChangeSet <$> acc `mappendChangeSet` cs') undo
  where
    vals = st `M.intersection` csMap
    undo =
      let processOne m (k, valueop) =
            case (valueop, k `M.lookup` vals) of
              (New _, Nothing)      -> pure $ M.insert k Rem m
              (Upd _, Just v0)      -> pure $ M.insert k (Upd v0) m
              (NotExisted, Nothing) -> pure $ M.insert k NotExisted m
              (Rem, Just v0)        -> pure $ M.insert k (New v0) m
              _                     -> throwError (CSMappendException k)
       in flip Undo BS.empty . ChangeSet <$> foldM processOne mempty (M.toList csMap)
    cs'@(ChangeSet csMap) = case cam of
            CAMRevert (Undo cs_ _) -> cs_
            CAMChange cs_          -> cs_

data TestCtx id value = TestCtx
    { tctxChgAccum :: ChgAccumCtx (TestCtx id value)
    }

type instance ChgAccum (TestCtx id value) = SumChangeSet id value

instance HasLens (TestCtx id value) (ChgAccumCtx (TestCtx id value)) where
    sett ctx val = ctx { tctxChgAccum = val }

instance HasGetter (TestCtx id value) (ChgAccumCtx (TestCtx id value)) where
    gett = tctxChgAccum

data Counter = Counter
    { _cntQuery  :: Int
    , _cntIter   :: Int
    , _cntModAcc :: Int
    }

makeLenses ''Counter

instance Default Counter where
    def = Counter 0 0 0

newtype TestExecutorT e id value m a = TestExecutorT
    { runTestExecutorT :: ReaderT (TestCtx id value) (StateT Counter (ExceptT e m)) a }
    deriving (Functor, Applicative, Monad, MonadError e, MonadReader (TestCtx id value))

-- Dummy logging
instance Monad m => MonadLogging (TestExecutorT e id value m) where
    log _ _ _ = pure ()
    logName = pure (GivenName "")

instance Monad m => ModifyLogName (TestExecutorT e id value m) where
    modifyLogNameSel _ = id

applyDiff
    :: (Ord id, HasException e (CSMappendException id), MonadError e m)
    => ChangeSet id value -> Map id value -> m (Map id value)
applyDiff cs initM = foldM applyDiffOne initM (changeSetToList cs)
  where
    maybeLookup k m act1 act2 = maybe act1 act2 $ M.lookup k m
    applyDiffOne m (k, New v) =
        maybeLookup k m (pure $ M.insert k v m) (\_ -> throwLocalError $ CSMappendException k)
    applyDiffOne m (k, Upd v) =
        maybeLookup k m (throwLocalError $ CSMappendException k) (\_ -> pure $ M.insert k v m)
    applyDiffOne m (k, Rem)   =
        maybeLookup k m (throwLocalError $ CSMappendException k) (\_ -> pure $ M.delete k m)
    applyDiffOne m (k, NotExisted) =
        maybeLookup k m (pure m) (\_ -> throwLocalError $ CSMappendException k)

instance (MonadReader (StateP id value) m, IdSumPrefixed id,
          Ord id, HasException e (CSMappendException id)) =>
    Effectful (DbAccess (SumChangeSet id value) id value) (TestExecutorT e id value m) where
        effect dbAccess = do
            SumChangeSet acc <- getCAOrDefault . tctxChgAccum <$> ask
            storage <- TestExecutorT $ lift $ lift ask
            storage' <- acc `applyDiff` storage
            case dbAccess of
                DbQuery _ _         -> TestExecutorT $ lift $ modify $ cntQuery %~ (+1)
                DbIterator _ _      -> TestExecutorT $ lift $ modify $ cntIter %~ (+1)
                DbModifyAccum _ _ _ -> TestExecutorT $ lift $ modify $ cntModAcc %~ (+1)
            pure $ simpleStateAccessor storage' dbAccess

countERoComp
    :: (MonadReader (StateP id value) m, Ord id,
        IdSumPrefixed id, HasException e (CSMappendException id))
    => ERoComp e id value (TestCtx id value) a
    -> m (Either e Counter)
countERoComp comp =
    runExceptT $ flip execStateT def $
        runReaderT (runTestExecutorT (unBaseM comp)) $ TestCtx CANotInitialized

runERoComp
    :: (MonadReader (StateP id value) m, Ord id,
        IdSumPrefixed id, HasException e (CSMappendException id))
    => ERoComp e id value (TestCtx id value) a
    -> m (Either e a)
runERoComp comp =
    runExceptT $ flip evalStateT def $
        runReaderT (runTestExecutorT (unBaseM comp)) $ TestCtx CANotInitialized
