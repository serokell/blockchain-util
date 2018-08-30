{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}

module Snowdrop.Core.BaseM
       (
         BaseMConstraint
       , BaseM (..)
       , Effectful (..)
       , CtxConcurrently (..)
       ) where

import           Universum hiding (log)

import           Data.Default (Default (def))

import           Control.Monad.Except (MonadError (..))
import qualified Snowdrop.Util as Log

data CtxConcurrently =
      Parallel -- ^ TODO: not used currently
    | Sequential
    deriving (Show, Eq)

instance Default CtxConcurrently where
    def = Sequential

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
