{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Core.Validator.Types
       ( PreValidator (..)
       , mkPreValidator
       , Validator
       , mkValidator
       , runValidator
       , upcastPreValidator
       , fromPreValidator
       , getPreValidator
       ) where

import           Universum hiding (Nat)

import           Data.Default (Default (..))
import           Data.Vinyl

import           Snowdrop.Core.ERoComp (ERoComp, UpCastableERo, upcastEffERoComp)
import           Snowdrop.Core.Transaction (DownCastableTx, StateTx, TxComponents, downcastStateTx)

import           Snowdrop.Hetero (RContains)

------------------------------------------------------
-- PreValidator
------------------------------------------------------

-- TODO pva701: maybe we should create ValidatorComponents and use it instead of TxComponents
-- | Function which validates transaction, assuming a transaction
-- of appropriate type is supplied.
-- It's expected to project some id types and validate them alone
-- without considering other id types.
newtype PreValidator conf txtype
    = PreValidator {runPrevalidator :: StateTx txtype -> ERoComp conf (TxComponents txtype) () }

-- | Alias for 'PreValidator' constructor
mkPreValidator
    :: (StateTx txtype -> ERoComp conf (TxComponents txtype) ())
    -> PreValidator conf txtype
mkPreValidator = PreValidator

------------------------------------------------------
-- Cast of PreValidator
------------------------------------------------------

upcastPreValidator
    :: forall txtype1 txtype2 conf .
    ( UpCastableERo (TxComponents txtype1) (TxComponents txtype2)
    , DownCastableTx txtype2 txtype1
    )
    => PreValidator conf txtype1
    -> PreValidator conf txtype2
upcastPreValidator prev = PreValidator $ upcastEffERoComp @_ @_ @conf . runPrevalidator prev . downcastStateTx

------------------------------------------------------
-- Validator
------------------------------------------------------

-- | Object to validate transaction fully.
-- Contains a mapping from transaction type to a corresponding pre-validator
-- (which may be a concatenation of few other pre-validators).
-- Each entry in the mapping (a pre-validator) is expected to fully validate
-- the transaction, consider all id types in particular.
type Validator conf = Rec (PreValidator conf)

instance Semigroup (PreValidator conf txtype) where
     PreValidator a <> PreValidator b = PreValidator $ a <> b

instance Monoid (PreValidator conf txtype) where
    mempty = PreValidator $ const (pure ())
    mappend = (<>)

instance Default (PreValidator conf txtype) where
    def = mempty

-- | Smart constructor for 'Validator' type
mkValidator :: [PreValidator conf txtype] -> Validator conf '[txtype]
mkValidator ps = mconcat ps :& RNil

fromPreValidator :: PreValidator conf txtype -> Validator conf '[txtype]
fromPreValidator ps = ps :& RNil

getPreValidator :: Validator conf '[txtype] -> PreValidator conf txtype
getPreValidator (ps :& RNil) = ps

-- | Execute validator on a given transaction.
runValidator
    :: forall conf txtype txtypes . (RContains txtypes txtype)
    => Validator conf txtypes
    -> StateTx txtype
    -> ERoComp conf (TxComponents txtype) ()
runValidator prevalidators statetx =
    runPrevalidator (rget @txtype prevalidators) statetx
