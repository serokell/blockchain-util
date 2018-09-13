module Snowdrop.Core.Validator.Basic
       ( TxValidationException (..)
       , valid
       , validateIff
       , validateAll

       , StructuralValidationException (..)
       , structuralPreValidator

       , redundantIdsPreValidator
       , RedundantIdException (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Core.ChangeSet.Type (changeSetToList, csNew, csRemove)
import           Snowdrop.Core.ERoComp.Helpers (query)
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Core.Transaction (TxProof, txBody, txProof)
import           Snowdrop.Core.Validator.Types (PreValidator (..), Validator, mkValidator)
import           Snowdrop.Util

-- | Exception type for transaction validation (general failures)
data TxValidationException
    = PayloadProjectionFailed String Prefix
    -- ^ Failed to project payload (from general type to concrete subtype)
    deriving (Show)

instance Buildable TxValidationException where
    build = \case
        PayloadProjectionFailed tx p ->
            bprint
              ("Projection of payload is failed during validation of StateTx with type: "
               %build%", got prefix: "%build)
              tx
              p

-- | Helper, which can be used for a validator to specify success case.
valid :: (Monoid a, Monad m) => m a
valid = pure mempty

-- | Helper, which can be used for a validator to specify conditional success case.
-- Error will be returned iff the given boolean is $False.
validateIff :: forall e e1 m a . (Monoid a, MonadError e m, HasReview e e1) => e1 -> Bool -> m a
validateIff e1 = bool (throwLocalError e1) valid

-- | Helper, which can be used for a validator to specify conditional success case.
-- Error value is determined for a given element of container and is returned only
-- if check for the element results in $False.
validateAll
    :: (Foldable f, MonadError e m, Container (f a))
    => (Element (f a) -> e)
    -> (Element (f a) -> Bool)
    -> f a
    -> m ()
validateAll ex p ls = maybe (pure ()) (throwError . ex) (find (not . p) ls)

---------------------------
-- Structural validator
---------------------------

-- | Exception type for structural validation ($structuralPreValidator).
data StructuralValidationException id
    = DpRemoveDoesntExist id
    -- ^ Id is expected to exist in state, but doesn't.
    | DpAddExist id
    -- ^ Id is expected to be absent in state, but exists.
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

csRemoveExist
    :: (Ord id, HasException e (StructuralValidationException id))
    => PreValidator e id value ctx txtype
csRemoveExist = PreValidator $ \(csRemove . txBody -> inRefs) -> do
    ins <- query inRefs
    validateAll (inj . DpRemoveDoesntExist) (flip M.member ins) inRefs

csNewNotExist
    :: (Ord id, HasException e (StructuralValidationException id))
    => PreValidator e id value ctx txtype
csNewNotExist = PreValidator $ \tx -> do
    let ks = M.keysSet (csNew (txBody tx))
    mp <- query ks
    validateAll (inj . DpAddExist) (not . flip M.member mp) ks

-- | Structural validation checks whether the ids which are to be removed
-- (or to be modified) exist in state and vice versa.
structuralPreValidator
    :: (Ord id, HasException e (StructuralValidationException id))
    => PreValidator e id value ctx txtype
structuralPreValidator = csRemoveExist <> csNewNotExist

-- | Exception type for $redundantIdsPreValidator.
data RedundantIdException = RedundantIdException (NonEmpty Prefix)
    deriving (Show, Generic)

instance Buildable RedundantIdException where
    build (RedundantIdException pref) =
        bprint ("encountered unexpected prefixes: "%listF ", " build) (toList pref)

-- | Redundant id validation asserts that transaction contains no keys with unexpected prefixes.
redundantIdsPreValidator
    :: forall e id txtype value ctx.
    ( IdSumPrefixed id
    , HasException e RedundantIdException
    )
    => [Prefix]
    -> PreValidator e id value ctx txtype
redundantIdsPreValidator prefixes = PreValidator $ \statetx -> do
    let prefixesSet = S.fromList prefixes
    let obtainedPrefixesSet = S.fromList $ (idSumPrefix . fst)
                                        <$> (changeSetToList $ txBody statetx)
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

inputsExist
    :: (Ord id, HasException e StateTxValidationException)
    => PreValidator e id value ctx txtype
inputsExist = PreValidator $ \(csRemove . txBody -> inRefs) -> do
    ins <- query inRefs
    validateIff InputDoesntExist (all (flip M.member ins) inRefs)

inputsSigned
    :: (Ord id, HasException e StateTxValidationException)
    => PreValidator e id (TxProof txtype -> Bool, value) ctx txtype
inputsSigned = PreValidator $ \tx -> do
    let inRefs = csRemove $ txBody tx
        proof  = txProof tx
    ins <- query inRefs
    validateIff InputNotSigned (all (($ proof) . fst) ins)

_exampleStateTxValidator :: Ord id => Validator GlobalError id ((TxProof txtype) -> Bool, value) ctx '[txtype]
_exampleStateTxValidator = mkValidator [inputsExist, inputsSigned]

---------------------------
-- HasReview instances
---------------------------

deriveView withInj ''GlobalError
