{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.IOExecutor
       (
         runBaseMIO
       , BaseMIOExec (..)
       , BaseMIO
       , BaseMException (..)
       , IOCtx (..)
--       , runERwCompIO
--       , runERoCompIO
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
import           Control.Monad.Trans.Control (MonadBaseControl)
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
import           Snowdrop.Util (HasException, HasGetter (gett), HasLens (sett), RIO)
import qualified Snowdrop.Util as Log


newtype BaseMIOExec eff m = BaseMIOExec { unBaseMIOExec :: forall x . eff x -> m x }

newtype BaseMException e = BaseMException e
    deriving Show

instance Buildable e => Buildable (BaseMException e) where
    build (BaseMException e) = Buildable.build e

instance (Show e, Typeable e) => Exception (BaseMException e)

newtype BaseMIO e (eff :: * -> *) m a = BaseMIO
    { unBaseMIO :: m a }
    deriving (Functor)

instance (BaseMIOContstraint ctx m, HasLens ctx CtxConcurrently) => Applicative (BaseMIO e eff m) where
    pure a = BaseMIO $ pure a
    BaseMIO a <*> BaseMIO b = BaseMIO $ do
        ctxConcurrent <- gett <$> ask
        case ctxConcurrent of
            Sequential -> a <*> b
            Parallel   -> do
                let g = flip sett Sequential
                (fun, val) <- Async.concurrently
                                (local g a)
                                (local g b)
                pure $ fun val

type BaseMIOContstraint ctx m = (MonadBaseControl IO m, MonadCatch m, MonadReader ctx m)

deriving instance
    ( BaseMIOContstraint ctx m
    , HasLens ctx CtxConcurrently
    ) => Monad (BaseMIO e eff m)
deriving instance
    ( BaseMIOContstraint ctx m
    , MonadIO m
    , HasLens ctx CtxConcurrently
    ) => MonadIO (BaseMIO e eff m)
deriving instance
    ( BaseMIOContstraint ctx m
    , HasLens ctx CtxConcurrently
    ) => MonadReader ctx (BaseMIO e eff m)

instance (BaseMIOContstraint ctx m, Loot.HasLens' ctx Log.LoggingIO, HasLens ctx CtxConcurrently, MonadIO m)
        => Log.MonadLogging (BaseMIO e eff m) where
    log = Rio.defaultLog
    logName = Rio.defaultLogName

instance (BaseMIOContstraint ctx m, Loot.HasLens' ctx Log.LoggingIO, HasLens ctx CtxConcurrently, MonadIO m)
        => Log.ModifyLogName (BaseMIO e eff m) where
    modifyLogNameSel = Rio.defaultModifyLogNameSel

instance (BaseMIOContstraint ctx m, HasGetter ctx (BaseMIOExec eff m),
          HasLens ctx CtxConcurrently)
  => Effectful eff (BaseMIO e eff m) where
    effect eff = BaseMIO $ asks gett >>= flip unBaseMIOExec eff

instance (BaseMIOContstraint ctx m, Show e, Typeable e, HasLens ctx CtxConcurrently)
  => MonadError e (BaseMIO e eff m) where
    throwError e = BaseMIO $ throwM $ BaseMException e
    catchError (BaseMIO ma) handler = BaseMIO $
        ma `catch` (\(BaseMException e) -> unBaseMIO $ handler e)

runBaseMIO
    :: forall e eff ctx a m .
    ( Show e
    , Typeable e
    , HasGetter ctx (BaseMIOExec eff m)
    , HasLens ctx CtxConcurrently
    , Loot.HasLens Log.LoggingIO ctx Log.LoggingIO
    , BaseMIOContstraint ctx m
    , MonadIO m
    )
    => BaseM e eff ctx a
    -> m a
runBaseMIO bm = unBaseMIO @e @eff $ unBaseM bm

data IOCtx daa m = IOCtx
    { _ctxChgAccum   :: ChgAccumCtx (IOCtx daa m)
    , _ctxRestrict   :: RestrictCtx
    , _ctxExec       :: BaseMIOExec daa (IOCtx daa) m
    , _ctxLogger     :: Log.LoggingIO
    , _ctxConcurrent :: CtxConcurrently
    }

makeLenses ''IOCtx

type instance ChgAccum (IOCtx (DbAccessU chgAccum undo id value) m) = chgAccum
type instance ChgAccum (IOCtx (DbAccessM chgAccum id value) m) = chgAccum

instance Loot.HasLens Log.LoggingIO (IOCtx daa m) Log.LoggingIO where
    lensOf = ctxLogger

instance HasLens (IOCtx daa m) RestrictCtx where
    sett ctx val = ctx { _ctxRestrict = val }

instance HasGetter (IOCtx daa m) RestrictCtx where
    gett = _ctxRestrict

instance HasLens
           (IOCtx daa m)
           (BaseMIOExec daa (IOCtx daa) m) where
    sett ctx val = ctx { _ctxExec = val }

instance HasGetter
           (IOCtx daa m)
           (BaseMIOExec daa (IOCtx daa) m) where
    gett = _ctxExec

instance HasLens (IOCtx daa m) (ChgAccumCtx (IOCtx daa m)) where
    sett ctx val = ctx { _ctxChgAccum = val }

instance HasGetter (IOCtx daa m) (ChgAccumCtx (IOCtx daa m)) where
    gett = _ctxChgAccum

instance HasLens (IOCtx daa m) CtxConcurrently where
    sett ctx val = ctx { _ctxConcurrent = val }

instance HasGetter (IOCtx daa m) CtxConcurrently where
    gett = _ctxConcurrent

runERoCompIO
  :: forall e da daa a ctx m.
    ( Show e
    , Typeable e
    , Default (ChgAccum (IOCtx da m))
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    , DbActions da daa (ChgAccum (IOCtx da m)) m
    )
    => daa m
    -> Maybe (ChgAccum (IOCtx da m))
    -> BaseM e da (IOCtx da m) a
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
    , Default (ChgAccum (IOCtx da m))
    , HasException e StatePException
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    , DbActions da daa (ChgAccum (IOCtx da m)) m
    )
    => daa m
    -> s
    -> ERwComp e da (IOCtx da m) s a
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
