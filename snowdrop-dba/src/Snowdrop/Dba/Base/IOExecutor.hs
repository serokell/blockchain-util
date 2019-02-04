{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.Base.IOExecutor
       ( BaseMException (..)
       , IOCtx (..)
       , runERoCompIO
       ) where

import           Universum

import qualified Control.Concurrent.Async.Lifted as Async
import           Control.Monad.Except (MonadError (..))
import           Data.Default (Default (def))
import qualified Data.Text.Buildable as Buildable

import           Snowdrop.Core (BaseM (..), ChgAccumMaybe (..), CtxConcurrently (..),
                                Effectful (..), getCAOrDefault)
import           Snowdrop.Dba.Base.DbActions (DbActions (..))
import           Snowdrop.Util (ExecM, HasGetter (gett), HasLens (sett))
import qualified Snowdrop.Util as Log

newtype BaseMException e = BaseMException e
    deriving Show

instance Buildable e => Buildable (BaseMException e) where
    build (BaseMException e) = Buildable.build e

instance (Show e, Typeable e) => Exception (BaseMException e)

data IOCtx effect chgAccum = IOCtx
    { ctxChgAccum   :: ChgAccumMaybe chgAccum
    , ctxExec       :: forall x . ChgAccumMaybe chgAccum -> effect x -> ExecM x
    , ctxLogger     :: Log.LoggingIO
    , ctxConcurrent :: CtxConcurrently
    }

newtype BaseMIO e eff chgAccum a = BaseMIO
    { unBaseMIO :: ReaderT (IOCtx eff chgAccum) ExecM a }
    deriving (Functor)

instance Applicative (BaseMIO e eff ctx) where
    pure a = BaseMIO $ pure a
    BaseMIO a <*> BaseMIO b = BaseMIO $ do
        ctxConc <- ctxConcurrent <$> ask
        case ctxConc of
            Sequential -> a <*> b
            Parallel   -> ReaderT $ \ctx -> do
                let g ioctx = ioctx {ctxConcurrent = Sequential}
                (fun, val) <- Async.concurrently
                                (runReaderT (local g a) ctx)
                                (runReaderT (local g b) ctx)
                pure $ fun val

deriving instance Monad (BaseMIO e eff chgAccum)
deriving instance MonadReader (IOCtx eff chgAccum) (BaseMIO e eff chgAccum)
deriving instance MonadIO (BaseMIO e eff chgAccum)

-- These are used elsewhere (FIXME?)
instance HasLens (IOCtx da chgAcc) (ChgAccumMaybe chgAcc) where
    sett ctx val = ctx { ctxChgAccum = val }

instance HasGetter (IOCtx da chgAcc) (ChgAccumMaybe chgAcc) where
    gett = ctxChgAccum

instance HasLens (IOCtx da chgAcc) CtxConcurrently where
    sett ctx val = ctx { ctxConcurrent = val }

instance HasGetter (IOCtx da chgAcc) CtxConcurrently where
    gett = ctxConcurrent
-----------------------------------

instance Log.MonadLogging (BaseMIO e eff chgAccum) where
    log l t = Log.defLog ctxLogger l t

instance Effectful eff (BaseMIO e eff chgAccum) where
    effect eff = BaseMIO $ ReaderT $ \IOCtx {..} -> ctxExec ctxChgAccum eff

instance (Show e, Typeable e) => MonadError e (BaseMIO e eff ca) where
    throwError e = BaseMIO $ throwM $ BaseMException e
    catchError (BaseMIO ma) handler = BaseMIO $
        ma `catch` (\(BaseMException e) -> unBaseMIO $ handler e)

runERoCompIO
  :: forall chgAccum da daa e m a.
    ( Show e
    , Typeable e
    , Default chgAccum
    , MonadIO m
    , MonadReader Log.LoggingIO m
    , DbActions da daa chgAccum ExecM
    )
    => daa ExecM
    -> Maybe chgAccum
    -> BaseM e da (IOCtx da chgAccum) a
    -> m a
runERoCompIO daa initAcc comp = do
    logger <- ask
    liftIO $ Log.runRIO logger $ runReaderT (unBaseMIO $ unBaseM comp)
        IOCtx
          { ctxChgAccum = maybe CANotInitialized CAInitialized initAcc
          , ctxExec = \(getCAOrDefault -> chgAccum) da -> executeEffect da daa chgAccum
          , ctxLogger = logger
          , ctxConcurrent = def
          }
