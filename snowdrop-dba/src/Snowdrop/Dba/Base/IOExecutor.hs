{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.Base.IOExecutor
       (
         BaseMIOExec (..)
       , BaseMIO
       , BaseMException (..)
       , IOCtx (..)
       , runERoCompIO
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

import           Snowdrop.Core (BaseM (..), ChgAccumMaybe (..), CtxConcurrently (..),
                                Effectful (..), getCAOrDefault)
import           Snowdrop.Dba.Base.DbActions (DbActions (..))
import           Snowdrop.Util (ExecM, HasGetter (gett), HasLens (sett))
import qualified Snowdrop.Util as Log

newtype BaseMIOExec eff chgAcc = BaseMIOExec { unBaseMIOExec :: forall x . ChgAccumMaybe chgAcc -> eff x -> ExecM x }

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

instance ( HasGetter (IOCtx eff chgAccum) (BaseMIOExec eff chgAccum)
         , HasLens (IOCtx eff chgAccum) CtxConcurrently
         , HasGetter (IOCtx eff chgAccum) (ChgAccumMaybe chgAccum)
         ) => Effectful eff (BaseMIO e eff (IOCtx eff chgAccum)) where
   effect eff = BaseMIO $ ReaderT $ \ctx -> unBaseMIOExec @_ @chgAccum (gett ctx) (gett ctx) eff

instance (Show e, Typeable e, HasLens ctx CtxConcurrently) => MonadError e (BaseMIO e eff ctx) where
    throwError e = BaseMIO $ throwM $ BaseMException e
    catchError (BaseMIO ma) handler = BaseMIO $
        ma `catch` (\(BaseMException e) -> unBaseMIO $ handler e)

data IOCtx effect chgAccum = IOCtx
    { _ctxChgAccum   :: ChgAccumMaybe chgAccum
    , _ctxExec       :: BaseMIOExec effect chgAccum
    , _ctxLogger     :: Log.LoggingIO
    , _ctxConcurrent :: CtxConcurrently
    }

makeLenses ''IOCtx

instance Loot.HasLens Log.LoggingIO (IOCtx da chgAcc) Log.LoggingIO where
    lensOf = ctxLogger

instance HasLens (IOCtx da chgAcc) (BaseMIOExec da chgAcc) where
    sett ctx val = ctx { _ctxExec = val }

instance HasGetter (IOCtx da chgAcc) (BaseMIOExec da chgAcc) where
    gett = _ctxExec

instance HasLens (IOCtx da chgAcc) (ChgAccumMaybe chgAcc) where
    sett ctx val = ctx { _ctxChgAccum = val }

instance HasGetter (IOCtx da chgAcc) (ChgAccumMaybe chgAcc) where
    gett = _ctxChgAccum

instance HasLens (IOCtx da chgAcc) CtxConcurrently where
    sett ctx val = ctx { _ctxConcurrent = val }

instance HasGetter (IOCtx da chgAcc) CtxConcurrently where
    gett = _ctxConcurrent

runERoCompIO
  :: forall chgAccum da daa ctx e m a.
    ( Show e
    , Typeable e
    , Default chgAccum
    , MonadIO m
    , MonadReader ctx m
    , HasLens' ctx Log.LoggingIO
    , DbActions da daa chgAccum ExecM
    )
    => daa ExecM
    -> Maybe chgAccum
    -> BaseM e da (IOCtx da chgAccum) a
    -> m a
runERoCompIO daa initAcc comp = do
    logger <- view (lensOf @Log.LoggingIO)
    liftIO $ Log.runRIO logger $ runReaderT (unBaseMIO @_ @da (unBaseM comp))
        IOCtx
          { _ctxChgAccum = maybe CANotInitialized CAInitialized initAcc
          , _ctxExec = exec
          , _ctxLogger = logger
          , _ctxConcurrent = def
          }
  where
    exec = BaseMIOExec @_ @chgAccum $ \(getCAOrDefault -> chgAccum) da -> executeEffect da daa chgAccum
