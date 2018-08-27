module Snowdrop.Core.Validator.Basic
       ( StructuralValidationException (..)
       , structuralPreValidator

       , inputsExist
       , inputsSigned
       , exampleStateTxValidator
       , redundantIdsPreValidator
       , RedundantIdException (..)
       ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Core.ChangeSet.Type (changeSetToList, csNew, csRemove)
import           Snowdrop.Core.ERoComp.Helpers (query, validateAll, validateIff)
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Core.Transaction (TxProof, txBody, txProof)
import           Snowdrop.Core.Validator.Types (PreValidator (..), Validator, mkValidator)
import           Snowdrop.Util

---------------------------
-- Structural validator
---------------------------

data StructuralValidationException id
    = DpRemoveDoesntExist id
    | DpAddExist id
    deriving (Show)

instance Buildable id => Buildable (StructuralValidationException id) where
    build = \case
        DpRemoveDoesntExist i ->
            bprint ("Removing entry associated with key "%build%
                    ", which isn't present in store") i
        DpAddExist i ->
            bprint ("Adding entry associated with key "%build%
                    ", which is already present in store") i
            -- this is an error as soon as we have separated 'New' and 'Upd'
            -- 'ValueOp's.

csRemoveExist :: (Ord id, HasException e (StructuralValidationException id)) => PreValidator e id value ctx txtype
csRemoveExist = PreValidator $ \(csRemove . txBody -> inRefs) -> do
    ins <- query inRefs
    validateAll (inj . DpRemoveDoesntExist) (flip M.member ins) inRefs

csNewNotExist :: (Ord id, HasException e (StructuralValidationException id)) => PreValidator e id value ctx txtype
csNewNotExist = PreValidator $ \tx -> do
    let ks = M.keysSet (csNew (txBody tx))
    mp <- query ks
    validateAll (inj . DpAddExist) (not . flip M.member mp) ks

structuralPreValidator :: (Ord id, HasException e (StructuralValidationException id)) => PreValidator e id value ctx txtype
structuralPreValidator = csRemoveExist <> csNewNotExist

data RedundantIdException = RedundantIdException (NonEmpty Prefix)
    deriving (Show, Generic)

instance Buildable RedundantIdException where
    build (RedundantIdException pref) =
        bprint ("encountered unexpected prefixes: "%listF ", " build) (toList pref)

redundantIdsPreValidator
  :: forall e id txtype value ctx.
    ( IdSumPrefixed id
    , HasException e RedundantIdException
    )
    => [Prefix]
    -> PreValidator e id value ctx txtype
redundantIdsPreValidator prefixes = PreValidator $ \statetx -> do
    let prefixesSet = S.fromList prefixes
    let obtainedPrefixesSet = S.fromList $ (idSumPrefix . fst) <$> (changeSetToList $ txBody statetx)
    let rest = obtainedPrefixesSet `S.difference` prefixesSet
    unless (S.null rest) $
        throwLocalError $ RedundantIdException $ NE.fromList $ S.toList rest

---------------------------
-- Example validators
---------------------------

data GlobalError = StateTxError StateTxValidationException

-- instance of HasReview declared at bottom with TH

data StateTxValidationException
    = InputDoesntExist
    | InputNotSigned

inputsExist :: (Ord id, HasException e StateTxValidationException) => PreValidator e id value ctx txtype
inputsExist = PreValidator $ \(csRemove . txBody -> inRefs) -> do
    ins <- query inRefs
    validateIff InputDoesntExist (all (flip M.member ins) inRefs)

inputsSigned :: (Ord id, HasException e StateTxValidationException) => PreValidator e id (TxProof txtype -> Bool, value) ctx txtype
inputsSigned = PreValidator $ \tx -> do
    let inRefs = csRemove $ txBody tx
        proof = txProof tx
    ins <- query inRefs
    validateIff InputNotSigned (all (($ proof) . fst) ins)

exampleStateTxValidator :: Ord id => Validator GlobalError id ((TxProof txtype) -> Bool, value) ctx '[txtype]
exampleStateTxValidator = mkValidator [inputsExist, inputsSigned]

---------------------------
-- HasReview instances
---------------------------

deriveView withInj ''GlobalError
