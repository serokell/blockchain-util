{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}

module Snowdrop.Core.BaseM
    (
      BaseMConstraint
    , BaseM (..)
    , Concurrently (..)
    , Race (..)
    , Effectful (..)
    ) where

import           Universum hiding (log)

import           Control.Monad.Except (MonadError (..))
import qualified Snowdrop.Util as Log

class Monad m => Concurrently m where
    concurrentlySg :: Semigroup a => m a -> m a -> m a
    concurrentlySg = concurrentlySgImpl (<>)

    concurrentlyManySg :: Semigroup a => NonEmpty (m a) -> m a
    concurrentlyManySg = concurrentlyMany' (<>)

    concurrentlyMany' :: (a -> a -> a) -> NonEmpty (m a) -> m a
    concurrentlyMany' append (ma :| rest) = foldr (concurrentlySgImpl append) ma rest

    concurrentlySgImpl :: (a -> a -> a) -> m a -> m a -> m a
    concurrentlySgImpl f ma mb = uncurry f <$> concurrently ma mb

    concurrently :: m a -> m b -> m (a, b)

class MonadError e m => Race e m where
    race :: m a -> m b -> m (Either a b)

    race' :: m a -> m a -> m a
    race' = fmap (either id id) ... race

    raceMany :: NonEmpty (m a) -> m a
    raceMany (f :| rest) = foldr race' f rest

class Monad m => Effectful eff m where
    -- | Executes effect `eff` in monad `m`.
    -- A natural transformation from effect data type to monad.
    effect :: eff a -> m a

instance Race e m => Race e (ReaderT ctx m) where
    race ma mb = ReaderT $ \ctx -> race (runReaderT ma ctx) (runReaderT mb ctx)

instance Concurrently m => Concurrently (ReaderT ctx m) where
    -- TODO implement other methods of type class as well
    concurrentlySgImpl f ma mb = ReaderT $ \ctx -> concurrentlySgImpl f (runReaderT ma ctx) (runReaderT mb ctx)
    concurrently ma mb = ReaderT $ \ctx -> concurrently (runReaderT ma ctx) (runReaderT mb ctx)

instance Effectful eff m => Effectful eff (ReaderT ctx m) where
    effect = lift . effect

type BaseMConstraint e eff ctx m = (Effectful eff m, Concurrently m, MonadReader ctx m, MonadError e m, Race e m, Log.MonadLogging m, Log.ModifyLogName m)

-- | Base execution monad.
-- Notice, `BaseM` is not existential type, one would look like:
--    `data B2 eff a = forall m . Effectful eff m => B2 (m a)`
-- Having, say, `instance Effectful Maybe IO`,
-- term `BaseM (pure () :: IO ())` would cause compile,
-- while `B2 (pure () :: IO ())` would be perfectly valid.
newtype BaseM e eff ctx a = BaseM { unBaseM :: forall m . BaseMConstraint e eff ctx m => m a }
    deriving Functor

instance Concurrently (BaseM e eff ctx) where
    concurrently (BaseM a) (BaseM b) = BaseM $ concurrently a b
    concurrentlySgImpl f (BaseM a) (BaseM b) = BaseM $ concurrentlySgImpl f a b

instance Race e (BaseM e eff ctx) where
    race (BaseM a) (BaseM b) = BaseM $ race a b

instance Effectful eff (BaseM e eff ctx) where
    effect eff = BaseM $ effect eff

instance Semigroup a => Semigroup (BaseM e eff ctx a) where
    BaseM a <> BaseM b = BaseM $ concurrentlySg a b

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
