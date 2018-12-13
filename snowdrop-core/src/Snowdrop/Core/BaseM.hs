{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types    #-}

module Snowdrop.Core.BaseM
       (
         BaseMConstraint
       , BaseM (..)
       , CtxConcurrently (..)
       , concurrently
       , raceMany
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

type BaseMConstraint e ctx m = (MonadReader ctx m,
                                MonadError e m,
                                Log.MonadLogging m,
                                Log.ModifyLogName m)

-- | Base execution monad.
newtype BaseM e ctx a = BaseM { unBaseM :: forall m . BaseMConstraint e ctx m => m a }
    deriving Functor

instance Semigroup a => Semigroup (BaseM e ctx a) where
    BaseM a <> BaseM b = BaseM $ liftA2 (<>) a b

instance (Semigroup a, Monoid a) => Monoid (BaseM e ctx a) where
    mempty = pure $ mempty
    mappend = (<>)

instance Applicative (BaseM e ctx) where
    pure a = BaseM $ pure a
    BaseM a <*> BaseM b = BaseM $ a <*> b

instance Monad (BaseM e ctx) where
    a >>= b = unBaseM a >>= b

instance MonadReader ctx (BaseM e ctx) where
    local f a = BaseM $ local f $ unBaseM a
    ask = BaseM ask

instance MonadError e (BaseM e ctx) where
    throwError e = BaseM $ throwError e
    catchError (BaseM ma) cont = ma `catchError` cont

instance Log.MonadLogging (BaseM e ctx) where
    log l s t = BaseM (Log.log l s t)
    logName = BaseM Log.logName

instance Log.ModifyLogName (BaseM e ctx) where
    modifyLogNameSel f (BaseM ma) = BaseM $ Log.modifyLogNameSel f ma
