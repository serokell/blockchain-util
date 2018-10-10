module Snowdrop.Core.Validator.Basic
       ( valid
       , validateIff
       , validateAll

       , StructuralValidationException (..)
       , structuralPreValidator

       -- , inputsExist
       -- , inputsSigned
       -- , exampleStateTxValidator
       ) where

import           Control.Monad.Except (MonadError (..))
import           Universum

import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable
import           Data.Vinyl.Core (Rec (..))
import           Formatting (bprint, build, (%))

import           Snowdrop.Core.ChangeSet (HChangeSet, csNew, csRemove)
import           Snowdrop.Core.ERoComp (ERoComp, QueryERo, query)
import           Snowdrop.Core.Transaction (TxComponents, txBody)
import           Snowdrop.Core.Validator.Types (PreValidator (..))
import           Snowdrop.Util

---------------------------
-- Helpers for validation
---------------------------

-- | Helper, which can be used for a validator to specify success case.
valid :: (Monoid a, Monad m) => m a
valid = pure mempty

-- | Helper, which can be used for a validator to specify conditional success case.
-- Error will be returned iff the given boolean is 'False'.
validateIff :: forall e e1 m a . (Monoid a, MonadError e m, HasReview e e1) => e1 -> Bool -> m a
validateIff e1 = bool (throwLocalError e1) valid

-- | Helper, which can be used for a validator to specify conditional success case.
-- Error value is determined for a given element of container and is returned only
-- if check for the element results in 'False'.
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

-- | Exception type for structural validation ('structuralPreValidator').
data StructuralValidationException
    = forall id . (Show id, Buildable id) => DpRemoveDoesntExist id
    -- ^ Id is expected to exist in state, but doesn't.
    | forall id . (Show id, Buildable id) => DpAddExist id
    -- ^ Id is expected to be absent in state, but exists.
deriving instance Show StructuralValidationException

instance Buildable StructuralValidationException where
    build = \case
        DpRemoveDoesntExist i ->
            bprint ("Removing entry associated with key "%build%
                    ", which isn't present in store") i
        DpAddExist i ->
            bprint ("Adding entry associated with key "%build%
                    ", which is already present in store") i
            -- this is an error as soon as we have separated 'New' and 'Upd'
            -- 'ValueOp's.

type StructuralC txtype xs = (
    RecAll' xs (QueryERo (TxComponents txtype))
  , RecAll' xs ExnHKey
  )

csRemoveExist
    :: forall e ctx txtype .
    ( HasException e StructuralValidationException
    , StructuralC txtype (TxComponents txtype)
    )
    => PreValidator e ctx txtype
csRemoveExist = PreValidator $ checkBody . txBody
  where
    checkBody :: forall xs . StructuralC txtype xs => HChangeSet xs -> ERoComp e (TxComponents txtype) ctx ()
    checkBody RNil         = pure ()
    checkBody gs'@(_ :& _) = checkBody' gs'

    checkBody' :: forall t xs' . StructuralC txtype (t ': xs') => HChangeSet (t ': xs') -> ERoComp e (TxComponents txtype) ctx ()
    checkBody' (cs :& gs) = do
        let inRefs = csRemove cs
        ins <- query @t inRefs
        validateAll (inj . DpRemoveDoesntExist) (flip M.member ins) inRefs
        checkBody gs

csNewNotExist
    :: forall e ctx txtype .
    ( HasException e StructuralValidationException
    , StructuralC txtype (TxComponents txtype)
    ) => PreValidator e ctx txtype
csNewNotExist = PreValidator $ checkBody . txBody
  where
    checkBody :: forall xs . StructuralC txtype xs => HChangeSet xs -> ERoComp e (TxComponents txtype) ctx ()
    checkBody RNil         = pure ()
    checkBody gs'@(_ :& _) = checkBody' gs'

    checkBody' :: forall t xs' . StructuralC txtype (t ': xs') => HChangeSet (t ': xs') -> ERoComp e (TxComponents txtype) ctx ()
    checkBody' (cs :& gs) = do
        let ks = M.keysSet (csNew cs)
        mp <- query @t ks
        validateAll (inj . DpAddExist) (not . flip M.member mp) ks
        checkBody gs

-- | Structural validation checks whether the ids which are to be removed
-- (or to be modified) exist in state and vice versa.
structuralPreValidator
    :: forall e ctx txtype .
    ( HasException e StructuralValidationException
    , StructuralC txtype (TxComponents txtype)
    ) => PreValidator e ctx txtype
structuralPreValidator = csRemoveExist <> csNewNotExist

---------------------------
-- Example validators
---------------------------

-- data GlobalError = StateTxError StateTxValidationException

-- -- instance of HasReview declared at bottom with TH

-- data StateTxValidationException
--     = InputDoesntExist
--     | InputNotSigned

-- inputsExist
--     :: (Ord id, HasException e StateTxValidationException)
--     => PreValidator e id value ctx txtype
-- inputsExist = PreValidator $ \(csRemove . txBody -> inRefs) -> do
--     ins <- query inRefs
--     validateIff InputDoesntExist (all (flip M.member ins) inRefs)

-- inputsSigned
--     :: (Ord id, HasException e StateTxValidationException)
--     => PreValidator e id (TxProof txtype -> Bool, value) ctx txtype
-- inputsSigned = PreValidator $ \tx -> do
--     let inRefs = csRemove $ txBody tx
--         proof  = txProof tx
--     ins <- query inRefs
--     validateIff InputNotSigned (all (($ proof) . fst) ins)

-- exampleStateTxValidator :: Ord id => Validator GlobalError id ((TxProof txtype) -> Bool, value) ctx '[txtype]
-- exampleStateTxValidator = mkValidator [inputsExist, inputsSigned]

-- ---------------------------
-- -- HasReview instances
-- ---------------------------

-- deriveView withInj ''GlobalError
