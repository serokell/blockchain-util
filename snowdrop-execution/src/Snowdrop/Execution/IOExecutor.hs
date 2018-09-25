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

import           Snowdrop.Core (BaseM (..), ChgAccum, ChgAccumCtx (..), CtxConcurrently (..),
                                DbAccessM, DbAccessU, ERwComp, Effectful (..), StatePException,
                                getCAOrDefault, runERwComp)
import           Snowdrop.Execution.DbActions (DbActions (..))
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

data IOCtx daa = IOCtx
    { _ctxChgAccum   :: ChgAccumCtx (IOCtx daa)
    , _ctxRestrict   :: RestrictCtx
    , _ctxExec       :: BaseMIOExec daa (IOCtx daa)
    , _ctxLogger     :: Log.LoggingIO
    , _ctxConcurrent :: CtxConcurrently
    }

makeLenses ''IOCtx

type instance ChgAccum (IOCtx (DbAccessU chgAccum undo id value)) = chgAccum
type instance ChgAccum (IOCtx (DbAccessM chgAccum id value)) = chgAccum

instance Loot.HasLens Log.LoggingIO (IOCtx daa) Log.LoggingIO where
    lensOf = ctxLogger

instance HasLens (IOCtx daa) RestrictCtx where
    sett ctx val = ctx { _ctxRestrict = val }

instance HasGetter (IOCtx daa) RestrictCtx where
    gett = _ctxRestrict

instance HasLens
           (IOCtx daa)
           (BaseMIOExec daa (IOCtx daa)) where
    sett ctx val = ctx { _ctxExec = val }

instance HasGetter
           (IOCtx daa)
           (BaseMIOExec daa (IOCtx daa)) where
    gett = _ctxExec

instance HasLens (IOCtx daa) (ChgAccumCtx (IOCtx daa)) where
    sett ctx val = ctx { _ctxChgAccum = val }

instance HasGetter (IOCtx daa) (ChgAccumCtx (IOCtx daa)) where
    gett = _ctxChgAccum

instance HasLens (IOCtx daa) CtxConcurrently where
    sett ctx val = ctx { _ctxConcurrent = val }

instance HasGetter (IOCtx daa) CtxConcurrently where
    gett = _ctxConcurrent

runERoCompIO
  :: forall e da daa a ctx m.
    ( Show e
    , Typeable e
    , Default (ChgAccum (IOCtx da))
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    , DbActions da daa (ChgAccum (IOCtx da)) ExecM
    )
    => daa ExecM
    -> Maybe (ChgAccum (IOCtx da))
    -> BaseM e da (IOCtx da) a
    -> m a
runERoCompIO daa initAcc comp = do
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
    exec = BaseMIOExec $ \(getCAOrDefault . _ctxChgAccum -> chgAccum) da -> executeEffect da daa chgAccum

runERwCompIO
  :: forall e da daa s ctx m a .
    ( Show e
    , Typeable e
    , Default (ChgAccum (IOCtx da))
    , HasException e StatePException
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    , DbActions da daa (ChgAccum (IOCtx da)) ExecM
    )
    => daa ExecM
    -> s
    -> ERwComp e da (IOCtx da) s a
    -> m (a, s)
runERwCompIO daa initS comp = do
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
    exec = BaseMIOExec $ \(getCAOrDefault . _ctxChgAccum -> chgAccum) dAccess -> executeEffect dAccess daa chgAccum
