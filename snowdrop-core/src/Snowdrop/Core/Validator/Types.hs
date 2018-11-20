{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Core.Validator.Types
       ( PreValidator (..)
       , mkPreValidator
       , unitePreValidators
       , Validator
       , mkValidator
       , runValidator
       , upcastPreValidator
       -- , castPreValidator
       , fromPreValidator
       , getPreValidator
       ) where

import           Universum hiding (Nat)

import           Data.Default (Default (..))
import           Data.Vinyl
import           Data.Vinyl.TypeLevel (AllConstrained, Nat (..), RLength)

import           Snowdrop.Core.ERoComp (ERoComp, UpCastableERo, upcastEffERoComp)
import           Snowdrop.Core.Stuff (UnitedTxType)
import           Snowdrop.Core.Transaction (DownCastableTx, StateTx, TxComponents, downcastStateTx)

import           Snowdrop.Hetero (RContains, RecToList (..))

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

type family ReplicateU (n :: Nat) (el :: [*]) where
    ReplicateU  'Z _     = '[]
    ReplicateU ('S x) el = (UnitedTxType el ': ReplicateU x el)

class ( UpCastableERo (TxComponents txtype1) (TxComponents txtype2)
      , DownCastableTx txtype2 txtype1)
      => CastableC txtype2 txtype1
instance ( UpCastableERo (TxComponents txtype1) (TxComponents txtype2)
      , DownCastableTx txtype2 txtype1)
      => CastableC txtype2 txtype1

type UniteRecToList txtypes conf =
    RecToList (PreValidator conf) (ReplicateU (RLength txtypes) txtypes) (PreValidator conf (UnitedTxType txtypes))

unitePreValidators
    :: forall txtypes conf .
    ( UniteRecToList txtypes conf
    , AllConstrained (CastableC (UnitedTxType txtypes)) txtypes
    )
    => Rec (PreValidator conf) txtypes
    -> PreValidator conf (UnitedTxType txtypes)
unitePreValidators = mconcat . recToList @(PreValidator conf) . rmapCast
  where
    rmapCast
        :: ( AllConstrained (CastableC (UnitedTxType txtypes)) xs)
        => Rec (PreValidator conf) xs -> Rec (PreValidator conf) (ReplicateU (RLength xs) txtypes)
    rmapCast RNil           = RNil
    rmapCast (pr :& others) = upcastPreValidator pr :& rmapCast others

upcastPreValidator
    :: forall txtype1 txtype2 conf .
    ( UpCastableERo (TxComponents txtype1) (TxComponents txtype2)
    , DownCastableTx txtype2 txtype1
    )
    => PreValidator conf txtype1
    -> PreValidator conf txtype2
upcastPreValidator prev = PreValidator $ upcastEffERoComp @_ @_ @conf . runPrevalidator prev . downcastStateTx

-- castPreValidator
--     :: forall id value txtype1 txtype2 conf .
--        e
--     -> (TxProof txtype2 -> Maybe (TxProof txtype1))
--     -> PreValidator e id valuconf txtype1
--     -> PreValidator e id valuconf txtype2
-- castPreValidator err castProof prev = PreValidator $ maybe (throwError err) (runPrevalidator prev) . castStateTx castProof

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
