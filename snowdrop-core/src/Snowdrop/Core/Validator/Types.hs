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
       , validate
       ) where

import           Universum hiding (Nat)

import           Data.Vinyl
import           Data.Vinyl.TypeLevel (Nat (..), RLength)

import           Snowdrop.Core.ERoComp (ERoComp, UpCastableERo, upcastEffERoComp)
import           Snowdrop.Core.Stuff (UnitedTxType)
import           Snowdrop.Core.Transaction (DownCastableTx, StateTx (..), TxComponents, downcastStateTx)

import           Snowdrop.Util (RContains, RecAll', RecToList (..), SomeData (..))

------------------------------------------------------
-- PreValidator
------------------------------------------------------

-- TODO pva701: maybe we should create ValidatorComponents and use it instead of TxComponents
-- | Function which validates transaction, assuming a transaction
-- of appropriate type is supplied.
-- It's expected to project some id types and validate them alone
-- without considering other id types.
newtype PreValidator e ctx txtype
    = PreValidator {runPrevalidator :: StateTx txtype -> ERoComp e (TxComponents txtype) ctx () }

-- | Alias for 'PreValidator' constructor
mkPreValidator
    :: (StateTx txtype -> ERoComp e (TxComponents txtype) ctx ())
    -> PreValidator e ctx txtype
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

type UniteRecToList txtypes e ctx =
    RecToList (PreValidator e ctx) (ReplicateU (RLength txtypes) txtypes) (PreValidator e ctx (UnitedTxType txtypes))

unitePreValidators
    :: forall txtypes e ctx .
    ( UniteRecToList txtypes e ctx
    , RecAll' txtypes (CastableC (UnitedTxType txtypes))
    )
    => Rec (PreValidator e ctx) txtypes
    -> PreValidator e ctx (UnitedTxType txtypes)
unitePreValidators = mconcat . recToList @(PreValidator e ctx) . rmapCast
  where
    rmapCast
        :: ( RecAll' xs (CastableC (UnitedTxType txtypes)))
        => Rec (PreValidator e ctx) xs -> Rec (PreValidator e ctx) (ReplicateU (RLength xs) txtypes)
    rmapCast RNil           = RNil
    rmapCast (pr :& others) = upcastPreValidator pr :& rmapCast others

upcastPreValidator
    :: forall txtype1 txtype2 e ctx .
    ( UpCastableERo (TxComponents txtype1) (TxComponents txtype2)
    , DownCastableTx txtype2 txtype1
    )
    => PreValidator e ctx txtype1
    -> PreValidator e ctx txtype2
upcastPreValidator prev = PreValidator $ upcastEffERoComp . runPrevalidator prev . downcastStateTx

-- castPreValidator
--     :: forall id value txtype1 txtype2 e ctx .
--        e
--     -> (TxProof txtype2 -> Maybe (TxProof txtype1))
--     -> PreValidator e id value ctx txtype1
--     -> PreValidator e id value ctx txtype2
-- castPreValidator err castProof prev = PreValidator $ maybe (throwError err) (runPrevalidator prev) . castStateTx castProof

------------------------------------------------------
-- Validator
------------------------------------------------------

-- | Object to validate transaction fully.
-- Contains a mapping from transaction type to a corresponding pre-validator
-- (which may be a concatenation of few other pre-validators).
-- Each entry in the mapping (a pre-validator) is expected to fully validate
-- the transaction, consider all id types in particular.
type Validator e ctx (txtypes :: [*]) = Rec (PreValidator e ctx) txtypes

instance Semigroup (PreValidator e ctx txtype) where
     PreValidator a <> PreValidator b = PreValidator $ a <> b

instance Monoid (PreValidator e ctx txtype) where
    mempty = PreValidator $ const (pure ())
    mappend = (<>)

-- | Smart constructor for 'Validator' type
mkValidator :: [PreValidator e ctx txtype] -> Validator e ctx '[txtype]
mkValidator ps = mconcat ps :& RNil

fromPreValidator :: PreValidator e ctx txtype -> Validator e ctx '[txtype]
fromPreValidator ps = ps :& RNil

getPreValidator :: Validator e ctx '[txtype] -> PreValidator e ctx txtype
getPreValidator (ps :& RNil) = ps

-- | Execute validator on a given transaction.
runValidator
    :: forall e ctx txtype txtypes . (RContains txtypes txtype)
    => Validator e ctx txtypes
    -> StateTx txtype
    -> ERoComp e (TxComponents txtype) ctx ()
runValidator prevalidators statetx =
    runPrevalidator (rget @txtype prevalidators) statetx

validate
    :: forall e ctx txtype txtypes .
    ( RContains txtypes txtype
    )
    => Validator e ctx txtypes
    -> [SomeData StateTx (RContains txtypes)]
    -> ERoComp e (TxComponents txtypes) ctx ()
validate _ [] = pure ()
validate validator ((SomeData (StateTx a b)) : txs) = undefined
