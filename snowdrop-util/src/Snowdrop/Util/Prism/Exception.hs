{-# LANGUAGE DataKinds #-}

module Snowdrop.Util.Prism.Exception
    (
      HasException
    , HasExceptions
    , HasReviews
    , HasPrisms

    , eitherThrow
    , maybeThrow
    , maybeThrowLocal
    , throwLocalError
    ) where

import           Universum

import           Control.Monad.Except (MonadError, throwError)
import           Data.Kind (Constraint)

import           Snowdrop.Util.Prism.Class

type HasException e e1 = HasReview e e1

type family HasReviews (e :: *) (exs :: [*]) where
    HasReviews e '[]    = (() :: Constraint)
    HasReviews e (e1:xs) = (HasReview e e1, HasReviews e xs)

type HasExceptions e xs = HasReviews e xs

type family HasPrisms (e :: *) (exs :: [*]) where
    HasPrisms e '[]    = (() :: Constraint)
    HasPrisms e (e1:xs) = (HasPrism e e1, HasPrisms e xs)

eitherThrow :: MonadError e m => Either e a -> m a
eitherThrow (Left e)  =  throwError e
eitherThrow (Right a) = pure a

maybeThrow :: MonadError e m => e -> Maybe a -> m a
maybeThrow e Nothing  = throwError e
maybeThrow _ (Just a) = pure a

maybeThrowLocal :: forall e1 e m a . (HasReview e e1, MonadError e m) => e1 -> Maybe a -> m a
maybeThrowLocal e Nothing  = throwError (inj e)
maybeThrowLocal _ (Just a) = pure a

throwLocalError :: forall e1 e m a . (HasReview e e1, MonadError e m) => e1 -> m a
throwLocalError = throwError . inj
