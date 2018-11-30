module Snowdrop.Core.ChangeSet.SumChangeSet
       (
         SumChangeSet (..)
       , modifySumChgSet
       , mappendStOrThrow
       ) where

import           Universum

import           Data.Default (Default (def))

import           Snowdrop.Core.ChangeSet.Type (HChangeSet, CSMappendException, mappendChangeSet)
import           Snowdrop.Hetero (ExnHKeyConstr)
import           Snowdrop.Util (HasReview (..))

-- | SumChangeSet holds some change set which is sum of several ChangeSet
newtype SumChangeSet xs = SumChangeSet {unSumCS :: HChangeSet xs}

deriving instance Show (HChangeSet xs) => Show (SumChangeSet xs)

instance Default (HChangeSet xs) => Default (SumChangeSet xs) where
    def = SumChangeSet def

modifySumChgSet
    :: ExnHKeyConstr xs
    => SumChangeSet xs
    -> HChangeSet xs
    -> Either CSMappendException (SumChangeSet xs)
modifySumChgSet (SumChangeSet cs1) cs2 = SumChangeSet <$> mappendChangeSet cs1 cs2

mappendStOrThrow
    :: forall e xs m .
    ( Monad m
    , MonadState (SumChangeSet xs) m
    , HasReview e CSMappendException
    , ExnHKeyConstr xs
    )
    => HChangeSet xs
    -> m (Either e ())
mappendStOrThrow chg = (flip modifySumChgSet chg) <$> get >>=
    either (pure . Left . inj) (\s -> put s $> Right ())
