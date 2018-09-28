{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.IOExecutor
       (
         runBaseMIO
       , BaseMIOExec (..)
       , BaseMIO
       , BaseMException (..)
       , BaseMIOContstraint
       , BaseMIOCtx (..)
       , mkBaseMIOCtx
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

import qualified Loot.Base.HasLens as Loot
import qualified Loot.Log.Rio as Rio

import           Snowdrop.Core (BaseM (..), ChgAccum, ChgAccumCtx (..), CtxConcurrently (..),
                                DbAccessM, DbAccessU, Effectful (..), getCAOrDefault)
import           Snowdrop.Execution.DbActions (DbActions (..))
import           Snowdrop.Execution.Restrict (RestrictCtx)
import           Snowdrop.Util (HasGetter (gett), HasLens (sett))
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

instance (BaseMIOContstraint ctx m, HasLens ctx CtxConcurrently)
    => Applicative (BaseMIO e eff m) where
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

instance (BaseMIOContstraint ctx m, Loot.HasLens' ctx Log.LoggingIO,
          HasLens ctx CtxConcurrently, MonadIO m)
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

data BaseMIOCtx daa m = BaseMIOCtx
    { _ctxChgAccum   :: ChgAccumCtx (BaseMIOCtx daa m)
    , _ctxRestrict   :: RestrictCtx
    , _ctxExec       :: BaseMIOExec daa m
    , _ctxConcurrent :: CtxConcurrently
    }

makeLenses ''BaseMIOCtx

type instance ChgAccum (BaseMIOCtx (DbAccessU chgAccum undo id value) m) = chgAccum
type instance ChgAccum (BaseMIOCtx (DbAccessM chgAccum id value) m) = chgAccum

instance HasLens (BaseMIOCtx daa m) RestrictCtx where
    sett ctx val = ctx { _ctxRestrict = val }

instance HasGetter (BaseMIOCtx daa m) RestrictCtx where
    gett = _ctxRestrict

instance HasLens
           (BaseMIOCtx daa m)
           (BaseMIOExec daa m) where
    sett ctx val = ctx { _ctxExec = val }

instance HasGetter
           (BaseMIOCtx daa m)
           (BaseMIOExec daa m) where
    gett = _ctxExec

instance HasLens (BaseMIOCtx daa m) (ChgAccumCtx (BaseMIOCtx daa m)) where
    sett ctx val = ctx { _ctxChgAccum = val }

instance HasGetter (BaseMIOCtx daa m) (ChgAccumCtx (BaseMIOCtx daa m)) where
    gett = _ctxChgAccum

instance HasLens (BaseMIOCtx daa m) CtxConcurrently where
    sett ctx val = ctx { _ctxConcurrent = val }

instance HasGetter (BaseMIOCtx daa m) CtxConcurrently where
    gett = _ctxConcurrent

mkBaseMIOCtx
  ::
    ( Monad m
    , MonadReader (BaseMIOCtx da m) m
    , Default (ChgAccum (BaseMIOCtx da m))
    , DbActions da daa (ChgAccum (BaseMIOCtx da m)) m
    )
  => daa m
  -> Maybe (ChgAccum (BaseMIOCtx da m))
  -> BaseMIOCtx da m
mkBaseMIOCtx daa initAcc =
    BaseMIOCtx
      { _ctxRestrict = def
      , _ctxChgAccum = maybe CANotInitialized CAInitialized initAcc
      , _ctxExec = exec
      , _ctxConcurrent = def
      }
  where
    exec = BaseMIOExec $ \da -> do
        chgAccum <- asks (getCAOrDefault . _ctxChgAccum)
        executeEffect da daa chgAccum
