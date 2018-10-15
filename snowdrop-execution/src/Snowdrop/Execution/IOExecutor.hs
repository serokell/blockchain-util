{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.IOExecutor
       (
         IOExecEffect
       , runBaseMIO
       , BaseMIOExec (..)
       , BaseMIO
       , BaseMException (..)
       , IOCtx (..)
       , runERwCompIO
       , runERoCompIO
       , applyERwComp
       -- * Lens
       , ctxChgAccum
       , ctxExec
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

import           Snowdrop.Core (BException, BaseM (..), ChgAccum, ChgAccumCtx (..), Ctx,
                                CtxConcurrently (..), ERwComp, Effectful (..), HasBException,
                                StatePException, Undo, getCAOrDefault, runERwComp)
import           Snowdrop.Execution.DbActions (DbAccessActionsU, DbActions (..), DbApplyProof,
                                               DbModifyActions (..))
import           Snowdrop.Util (ExecM, HasGetter (gett), HasLens (sett), HasReview)
import qualified Snowdrop.Util as Log

type family IOExecEffect conf :: * -> *

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

data IOCtx conf = IOCtx
    { _ctxChgAccum   :: ChgAccumCtx conf
    , _ctxExec       :: BaseMIOExec (IOExecEffect conf) (IOCtx conf)
    , _ctxLogger     :: Log.LoggingIO
    , _ctxConcurrent :: CtxConcurrently
    }

makeLenses ''IOCtx

instance Loot.HasLens Log.LoggingIO (IOCtx da) Log.LoggingIO where
    lensOf = ctxLogger

instance da ~ IOExecEffect conf => HasLens
           (IOCtx conf)
           (BaseMIOExec da (IOCtx conf)) where
    sett ctx val = ctx { _ctxExec = val }

instance da ~ IOExecEffect conf => HasGetter
           (IOCtx conf)
           (BaseMIOExec da (IOCtx conf)) where
    gett = _ctxExec

instance HasLens (IOCtx conf) (ChgAccumCtx conf) where
    sett ctx val = ctx { _ctxChgAccum = val }

instance HasGetter (IOCtx conf) (ChgAccumCtx conf) where
    gett = _ctxChgAccum

instance HasLens (IOCtx conf) CtxConcurrently where
    sett ctx val = ctx { _ctxConcurrent = val }

instance HasGetter (IOCtx conf) CtxConcurrently where
    gett = _ctxConcurrent

runERoCompIO
  :: forall ctx daa conf m a .
    ( Show (BException conf)
    , Typeable (BException conf)
    , Default (ChgAccum conf)
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    , DbActions (IOExecEffect conf) daa (ChgAccum conf) ExecM
    )
    => daa ExecM
    -> Maybe (ChgAccum conf)
    -> BaseM (BException conf) (IOExecEffect conf) (IOCtx conf) a
    -> m a
runERoCompIO daa initAcc comp = do
    logger <- view (lensOf @Log.LoggingIO)
    liftIO $ Log.runRIO logger $ runBaseMIO comp $
        IOCtx
          { _ctxChgAccum = maybe CANotInitialized CAInitialized initAcc
          , _ctxExec = exec
          , _ctxLogger = logger
          , _ctxConcurrent = def
          }
  where
    exec = BaseMIOExec $ \(getCAOrDefault . _ctxChgAccum -> chgAccum) da -> executeEffect da daa chgAccum

runERwCompIO
  :: forall ctx daa s conf m a .
    ( Show (BException conf)
    , Typeable (BException conf)
    , Default (ChgAccum conf)
    , HasBException conf StatePException
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    , DbActions (IOExecEffect conf) daa (ChgAccum conf) ExecM
    , Ctx conf ~ IOCtx conf
    )
    => daa ExecM
    -> s
    -> ERwComp conf (IOExecEffect conf) s a
    -> m (a, s)
runERwCompIO daa initS comp = do
    logger <- view (lensOf @Log.LoggingIO)
    liftIO $ Log.runRIO logger $ runBaseMIO (runERwComp comp initS) $
        IOCtx
          { _ctxChgAccum = CANotInitialized
          , _ctxExec = exec
          , _ctxLogger = logger
          , _ctxConcurrent = def
          }
  where
    exec = BaseMIOExec $ \(getCAOrDefault . _ctxChgAccum -> chgAccum) dAccess -> executeEffect dAccess daa chgAccum

applyERwComp
    :: forall stateConf conf a.
    ( Default (ChgAccum conf)
    , HasGetter (Undo conf) (Undo stateConf)
    , HasReview (Undo conf) (Undo stateConf)
    , Show (BException conf)
    , Typeable (BException conf)
    , HasBException conf StatePException
    , Ctx conf ~ IOCtx conf
    , DbActions (IOExecEffect conf) (DbAccessActionsU conf) (ChgAccum conf) ExecM
    )
    => DbModifyActions conf ExecM
    -> ERwComp conf (IOExecEffect conf) (ChgAccum conf) a
    -> ExecM (a, DbApplyProof conf)
applyERwComp dma rwComp = do
    (a, cs) <- runERwCompIO (dmaAccess dma) def rwComp
    (a,) <$> dmaApply dma cs
