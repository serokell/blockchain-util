{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types    #-}

module Snowdrop.Core.BaseM
       (
         BaseMConstraint
       , BaseM (..)
       , Effectful (..)
       , CtxConcurrently (..)
       , concurrently
       , raceMany
       , hoistEffectful
       ) where

import           Universum hiding (log)

import           Data.Default (Default (def))
import           Data.List.NonEmpty (fromList)

import           Control.Monad.Except (MonadError (..))
import           Snowdrop.Util (HasLens, sett)
import qualified Snowdrop.Util as Log

data CtxConcurrently =
      Parallel
    | Sequential
    deriving (Show, Eq)

instance Default CtxConcurrently where
    def = Sequential

concurrently :: (MonadReader ctx m, HasLens ctx CtxConcurrently) => m a -> m b -> m (a, b)
concurrently = local (flip sett Parallel) ... liftA2 (,)

-- | Run all computations and return the first result.
-- | TODO: make an effective implementation
raceMany :: (MonadReader ctx m, HasLens ctx CtxConcurrently) => NonEmpty (m a) -> m a
raceMany (a :| []) = a
raceMany a = concurrently left right >>= pure . fst
  where
    len   = length a
    left  = run $ fromList $ take (len `div` 2) $ toList a
    right = run $ fromList $ drop (len `div` 2) $ toList a
    run (b :| [])   = b
    run (b :| rest) = b <* (run $ fromList rest)

class Monad m => Effectful eff m where
    -- | Executes effect `eff` in monad `m`.
    -- A natural transformation from effect data type to monad.
    effect :: eff a -> m a

instance Effectful eff m => Effectful eff (ReaderT ctx m) where
    effect = lift . effect

type BaseMConstraint e eff ctx m = (Effectful eff m,
                                    MonadReader ctx m,
                                    MonadError e m,
                                    Log.MonadLogging m,
                                    Log.ModifyLogName m)

-- | Base execution monad.
-- Notice, `BaseM` is not existential type, one would look like:
--    `data B2 eff a = forall m . Effectful eff m => B2 (m a)`
-- Having, say, `instance Effectful Maybe IO`,
-- term `BaseM (pure () :: IO ())` would cause compile,
-- while `B2 (pure () :: IO ())` would be perfectly valid.
newtype BaseM e eff ctx a = BaseM { unBaseM :: forall m . BaseMConstraint e eff ctx m => m a }
    deriving Functor

instance Effectful eff (BaseM e eff ctx) where
    effect eff = BaseM $ effect eff

instance Semigroup a => Semigroup (BaseM e eff ctx a) where
    BaseM a <> BaseM b = BaseM $ liftA2 (<>) a b

instance (Semigroup a, Monoid a) => Monoid (BaseM e eff ctx a) where
    mempty = pure $ mempty
    mappend = (<>)

instance Applicative (BaseM e eff ctx) where
    pure a = BaseM $ pure a
    BaseM a <*> BaseM b = BaseM $ a <*> b

instance Monad (BaseM e eff ctx) where
    a >>= b = BaseM $ unBaseM a >>= unBaseM . b

instance MonadReader ctx (BaseM e eff ctx) where
    local f a = BaseM $ local f $ unBaseM a
    ask = BaseM ask

instance MonadError e (BaseM e eff ctx) where
    throwError e = BaseM $ throwError e
    catchError (BaseM ma) cont = BaseM $ ma `catchError` \e -> unBaseM $ cont e

instance Log.MonadLogging (BaseM e eff ctx) where
    log l s t = BaseM (Log.log l s t)
    logName = BaseM Log.logName

instance Log.ModifyLogName (BaseM e eff ctx) where
    modifyLogNameSel f (BaseM ma) = BaseM $ Log.modifyLogNameSel f ma

------------------------
-- Hoist effectful
------------------------

data TransformEff eff1 eff2 = TransformEff {getTransformEff :: forall b . eff1 b -> eff2 b }

newtype EffT eff1 eff2 m a = EffT { runEffT :: ReaderT (TransformEff eff1 eff2) m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadError e)

askF :: Monad m => EffT eff eff1 m (eff b -> eff1 b)
askF = EffT $ asks getTransformEff

instance Effectful eff2 m => Effectful eff1 (EffT eff1 eff2 m) where
    effect eff1a = askF >>= lift . effect . (eff1a &)

instance MonadReader ctx m => MonadReader ctx (EffT eff eff1 m) where
    local f r = EffT $ ReaderT $ \tr -> local f $ runReaderT (runEffT r) tr
    ask = EffT (lift ask)

instance (Log.MonadLogging m, Monad m) => Log.MonadLogging (EffT eff eff1 m) where
    log l s t = EffT $ lift $ Log.log l s t
    logName = EffT $ lift Log.logName

instance (Log.ModifyLogName m, Monad m) => Log.ModifyLogName (EffT eff eff1 m) where
    modifyLogNameSel f (EffT ma) = EffT $ ReaderT $ \ctx -> Log.modifyLogNameSel f (runReaderT ma ctx)

hoistEffectful
    :: forall e eff1 eff2 ctx a .
       (forall b . eff1 b -> eff2 b)
    -> BaseM e eff1 ctx a
    -> BaseM e eff2 ctx a
hoistEffectful f (BaseM ma) = BaseM $ runReaderT (runEffT ma) (TransformEff f)
