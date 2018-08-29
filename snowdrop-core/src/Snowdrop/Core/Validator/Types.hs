module Snowdrop.Core.Validator.Types
       ( PreValidator (..)
       , mkPreValidator
       , Validator (..)
       , ValidatorExecException (..)
       , mkValidator
       , runValidator
       ) where

import           Universum

import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Core.ERoComp.Types
import           Snowdrop.Core.Transaction
import           Snowdrop.Util

-- | Prevalidator validates part of the transaction.
newtype PreValidator e id proof value ctx =
    PreValidator (StateTx id proof value -> ERoComp e id value ctx ())

mkPreValidator
    :: (StateTx id proof value -> ERoComp e id value ctx ())
    -> PreValidator e id proof value ctx
mkPreValidator = PreValidator

-- | StateTxValidatos is a union of several (pre)validators for
-- different transaction types.
newtype Validator e id proof value ctx =
    Validator (Map StateTxType (PreValidator e id proof value ctx))

instance Ord id => Semigroup (PreValidator e id proof value ctx) where
     PreValidator a <> PreValidator b = PreValidator $ a <> b

instance Ord id => Monoid (PreValidator e id proof value ctx) where
    mempty = PreValidator $ const (pure ())
    mappend = (<>)

mkValidator
    :: Ord id
    => StateTxType
    -> [PreValidator e id proof value ctx]
    -> Validator e id proof value ctx
mkValidator txType = Validator . M.singleton txType . mconcat

data ValidatorExecException = PreValidatorNotFound StateTxType
    deriving (Show)

instance Buildable ValidatorExecException where
    build (PreValidatorNotFound stxType) =
        bprint ("Prevalidator was not found for "%build) stxType

runValidator
    :: (HasException e ValidatorExecException)
    => Validator e id proof value ctx
    -> StateTx id proof value
    -> ERoComp e id value ctx ()
runValidator (Validator prevalidators) statetx =
    case M.lookup (txType statetx) prevalidators of
        Nothing                    -> throwLocalError $ PreValidatorNotFound $ txType statetx
        Just (PreValidator action) -> action statetx

instance Ord id => Semigroup (Validator e id proof value ctx) where
    Validator m1 <> Validator m2 = Validator $ M.unionWith mappend m1 m2

instance Ord id => Monoid (Validator e id proof value ctx) where
    mempty = Validator M.empty
    mappend = (<>)
