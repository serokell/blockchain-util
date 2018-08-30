{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.IOExecutor
       (
         runBaseMIO
       , BaseMIOExec (..)
       , BaseMIO
       , BaseMException (..)
       , IOCtx (..)
       , runERwCompIO
       , runERoCompIO
       -- * Lens
       , ctxChgAccum
       , ctxExec
       , ctxRestrict
       , ctxConcurrent
       ) where

import           Universum

import qualified Control.Concurrent.Async.Lifted as Async
import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError (..))
import           Data.Default (Default (def))
import qualified Data.Text.Buildable as Buildable

import           Loot.Base.HasLens (HasLens', lensOf)
import qualified Loot.Base.HasLens as Loot
import qualified Loot.Log.Rio as Rio

import           Snowdrop.Core (BaseM (..), ChgAccum, ChgAccumCtx (..),
                                DbAccess (..), ERoComp, ERwComp, Effectful (..), FoldF (..),
                                StatePException, getCAOrDefault, runERwComp, CtxConcurrently (..))
import           Snowdrop.Execution.DbActions (DbAccessActions (..))
import           Snowdrop.Execution.Restrict (RestrictCtx)
import           Snowdrop.Util (ExecM, HasException, HasGetter (gett), HasLens (sett))
import qualified Snowdrop.Util as Log


newtype BaseMIOExec eff ctx = BaseMIOExec { unBaseMIOExec :: forall x . ctx -> eff x -> ExecM x }

newtype BaseMException e = BaseMException e
    deriving Show

instance Buildable e => Buildable (BaseMException e) where
    build (BaseMException e) = Buildable.build e

instance (Show e, Typeable e) => Exception (BaseMException e)

newtype BaseMIO e (eff :: * -> *) ctx a = BaseMIO
    { unBaseMIO :: ReaderT ctx ExecM a }
    deriving (Functor)

instance HasLens ctx CtxConcurrently => Applicative (BaseMIO e eff ctx) where
    pure a = BaseMIO $ pure a
    BaseMIO a <*> BaseMIO b = BaseMIO $ do
        ctxConcurrent <- gett <$> ask
        case ctxConcurrent of
            Sequential -> a <*> b
            Parallel   -> ReaderT $ \ctx -> do
                let g = flip sett Sequential
                (fun, val) <- Async.concurrently
                                (runReaderT (local g a) ctx)
                                (runReaderT (local g b) ctx)
                pure $ fun val

deriving instance HasLens ctx CtxConcurrently => Monad (BaseMIO e eff ctx)
deriving instance HasLens ctx CtxConcurrently => MonadReader ctx (BaseMIO e eff ctx)
deriving instance HasLens ctx CtxConcurrently => MonadIO (BaseMIO e eff ctx)

instance (Loot.HasLens' ctx Log.LoggingIO, HasLens ctx CtxConcurrently)
        => Log.MonadLogging (BaseMIO e eff ctx) where
    log = Rio.defaultLog
    logName = Rio.defaultLogName

instance (Loot.HasLens' ctx Log.LoggingIO, HasLens ctx CtxConcurrently)
        => Log.ModifyLogName (BaseMIO e eff ctx) where
    modifyLogNameSel = Rio.defaultModifyLogNameSel

instance (HasGetter ctx (BaseMIOExec eff ctx),
          HasLens ctx CtxConcurrently) => Effectful eff (BaseMIO e eff ctx) where
    effect eff = BaseMIO $ ReaderT $ \ctx -> unBaseMIOExec (gett ctx) ctx eff

instance (Show e, Typeable e, HasLens ctx CtxConcurrently) => MonadError e (BaseMIO e eff ctx) where
    throwError e = BaseMIO $ throwM $ BaseMException e
    catchError (BaseMIO ma) handler = BaseMIO $
        ma `catch` (\(BaseMException e) -> unBaseMIO $ handler e)

runBaseMIO
    :: forall e eff ctx a .
    (
      Show e, Typeable e, HasGetter ctx (BaseMIOExec eff ctx)
    , HasLens ctx CtxConcurrently
    , (Loot.HasLens Log.LoggingIO ctx Log.LoggingIO)
    )
    => BaseM e eff ctx a
    -> ctx
    -> ExecM a
runBaseMIO bm ctx = runReaderT (unBaseMIO @e @eff $ unBaseM bm) ctx

data IOCtx chgAccum id value = IOCtx
    { _ctxChgAccum   :: ChgAccumCtx (IOCtx chgAccum id value)
    , _ctxRestrict   :: RestrictCtx
    , _ctxExec       :: BaseMIOExec (DbAccess chgAccum id value) (IOCtx chgAccum id value)
    , _ctxLogger     :: Log.LoggingIO
    , _ctxConcurrent :: CtxConcurrently
    }

makeLenses ''IOCtx

type instance ChgAccum (IOCtx chgAccum id value) = chgAccum

instance Loot.HasLens Log.LoggingIO (IOCtx chgAccum id value) Log.LoggingIO where
    lensOf = ctxLogger

instance HasLens (IOCtx chgAccum id value) RestrictCtx where
    sett ctx val = ctx { _ctxRestrict = val }

instance HasGetter (IOCtx chgAccum id value) RestrictCtx where
    gett = _ctxRestrict

instance HasLens
           (IOCtx chgAccum id value)
           (BaseMIOExec (DbAccess chgAccum id value) (IOCtx chgAccum id value)) where
    sett ctx val = ctx { _ctxExec = val }

instance HasGetter
           (IOCtx chgAccum id value)
           (BaseMIOExec (DbAccess chgAccum id value) (IOCtx chgAccum id value)) where
    gett = _ctxExec

instance HasLens (IOCtx chgAccum id value) (ChgAccumCtx (IOCtx chgAccum id value)) where
    sett ctx val = ctx { _ctxChgAccum = val }

instance HasGetter (IOCtx chgAccum id value) (ChgAccumCtx (IOCtx chgAccum id value)) where
    gett = _ctxChgAccum

instance HasLens (IOCtx chgAccum id value) CtxConcurrently where
    sett ctx val = ctx { _ctxConcurrent = val }

instance HasGetter (IOCtx chgAccum id value) CtxConcurrently where
    gett = _ctxConcurrent

runERoCompIO
    :: forall e id value chgAccum a ctx m.
    ( Show e
    , Typeable e
    , Default chgAccum
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    )
    => DbAccessActions chgAccum id value ExecM
    -> Maybe chgAccum
    -> ERoComp e id value (IOCtx chgAccum id value) a
    -> m a
runERoCompIO dba initAcc comp = do
    logger <- view (lensOf @Log.LoggingIO)
    liftIO $ Log.runRIO logger $ runBaseMIO comp $
        IOCtx
          { _ctxRestrict = def
          , _ctxChgAccum = maybe CANotInitialized CAInitialized initAcc
          , _ctxExec = exec
          , _ctxLogger = logger
          , _ctxConcurrent = def
          }
  where
    exec = BaseMIOExec $ \(getCAOrDefault . _ctxChgAccum -> chgAccum) dAccess ->
        case dAccess of
            DbQuery req cont                         -> cont <$> daaGetter dba chgAccum req
            DbIterator prefix (FoldF (e, acc, cont)) -> cont <$> daaIter dba chgAccum prefix e acc
            DbModifyAccum accum chgSet cont          -> cont <$> daaModifyAccum dba accum chgSet

runERwCompIO
    :: ( Show e
       , Typeable e
       , Default chgAccum
       , HasException e StatePException
       , MonadIO m
       , MonadReader ctx m
       , HasLens' ctx Log.LoggingIO
       )
    => DbAccessActions chgAccum id value ExecM
    -> s
    -> ERwComp e id value (IOCtx chgAccum id value) s a
    -> m (a, s)
runERwCompIO dba initS comp = do
    logger <- view (lensOf @Log.LoggingIO)
    liftIO $ Log.runRIO logger $ runBaseMIO (runERwComp comp initS) $
        IOCtx
          { _ctxRestrict = def
          , _ctxChgAccum = CANotInitialized
          , _ctxExec = exec
          , _ctxLogger = logger
          , _ctxConcurrent = def
          }
  where
    exec = BaseMIOExec $ \(getCAOrDefault . _ctxChgAccum -> chgAccum) dAccess ->
        case dAccess of
            DbQuery req cont                         -> cont <$> daaGetter dba chgAccum req
            DbIterator prefix (FoldF (e, acc, cont)) -> cont <$> daaIter dba chgAccum prefix e acc
            DbModifyAccum accum chgSet cont          -> cont <$> daaModifyAccum dba accum chgSet
