{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Types
       (
         DbAccessActions (..)
       , DbAccessActionsM (..)
       , DbAccessActionsU (..)
       , IterAction (..)
       , DGetter
       , DGetter'
       , DIter
       , DIter'
       , DModify
       , DbModifyActions (..)
       , DbActionsException (..)
       , RememberForProof (..)
       , ClientMode (..)
       , DbActions (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Data.Vinyl.Core (Rec)
import           Formatting (bprint, build, (%))

import           Snowdrop.Core (CSMappendException (..), DbAccess (..), DbAccessM (..),
                                DbAccessU (..), FoldF (..), HChangeSet)
import           Snowdrop.Util (HKey, HMap, HSet, HVal, NewestFirst, OldestFirst)

type DGetter' xs m = HSet xs -> m (HMap xs)
type DGetter chgAccum xs m = chgAccum -> DGetter' xs m

type DModify chgAccum xs m = chgAccum -> OldestFirst [] (HChangeSet xs) -> m (Either CSMappendException (OldestFirst [] chgAccum))
type DModifyUndo chgAccum undo m = chgAccum -> NewestFirst [] undo -> m (Either CSMappendException chgAccum)
type DComputeUndo chgAccum undo m = chgAccum -> chgAccum -> m (Either CSMappendException undo)

newtype IterAction m t = IterAction {runIterAction :: forall b . b -> ((HKey t, HVal t) -> b -> b) -> m b }
type DIter' xs m = Rec (IterAction m) xs
type DIter chgAccum xs m = chgAccum -> m (DIter' xs m)

-- | Actions to execute 'DbAccess' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActions chgAccum xs m = DbAccessActions
    { daaGetter :: DGetter chgAccum xs m
    -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaIter   :: DIter chgAccum xs m
    -- ^ Iterate through keys with specified prefix (doesn't modify the state)
    }

-- | Actions to execute 'DbAccessM' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActionsM chgAccum xs m = DbAccessActionsM
    { daaAccess      :: DbAccessActions chgAccum xs m
    -- ^ Actions to execute 'DbAccess' effect.
    , daaModifyAccum :: DModify chgAccum xs m
    -- ^ Modify change accumulator with specified list of change sets.
    -- First argument is a change accumulator to modify.
    -- Second argument is a list of change sets (in oldest first order).
    -- Function applies each change set to accumulator in oldest first order,
    -- returns modified accumulator.
    }

-- | Actions to execute 'DbAccessU' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActionsU chgAccum undo (xs :: [*]) m = DbAccessActionsU
    { daaAccessM         :: DbAccessActionsM chgAccum xs m
    -- ^ Actions to execute 'DbAccessM' effect.
    , daaModifyAccumUndo :: DModifyUndo chgAccum undo m
    -- ^ Applies undos to a given accumulator.
    -- First argument is a change accumulator to modify.
    -- Second argument is a list of undo object (in newest first order).
    -- Function applies each undo to accumulator in newest first order,
    -- returns modified accumulator.
    , daaComputeUndo     :: DComputeUndo chgAccum undo m
    -- ^ Computes undo. First argument is a base change accumulator.
    -- Second argument is a modified change accumulator.
    -- Function returns an undo object, such that @modified `applyUndo` undo = base@
    }

-- | Class provided to execute effect with actions corresponding to it.
class DbActions effect actions param m where
    -- | Execute @effect r@ using @actions m@ and return value @r@ in monad @m@.
    executeEffect :: effect r -> actions m -> param -> m r

instance Monad m => DbActions (DbAccess xs)
                                (DbAccessActions chgAccum xs) chgAccum m where
    executeEffect (DbQuery req cont) daa chgAccum =
        cont <$> daaGetter daa chgAccum req
    executeEffect (DbIterator getComp (FoldF (e, acc, cont))) daa chgAccum =
        cont <$> ((\record -> runIterAction (getComp record) e acc) =<< daaIter daa chgAccum)

instance Monad m => DbActions (DbAccessM chgAccum xs)
                                (DbAccessActionsM chgAccum xs) chgAccum m where
    executeEffect (DbAccess da) (daaAccess -> daa) chgAccum = executeEffect da daa chgAccum
    executeEffect (DbModifyAccum chgSet cont) daaM chgAccum =
        cont <$> daaModifyAccum daaM chgAccum chgSet

instance Monad m => DbActions (DbAccessU chgAccum undo xs)
                                (DbAccessActionsU chgAccum undo xs) chgAccum m where
    executeEffect (DbAccessM daM) (daaAccessM -> daaM) chgAccum = executeEffect daM daaM chgAccum
    executeEffect (DbComputeUndo cs cont) daaU chgAccum =
        cont <$> daaComputeUndo daaU chgAccum cs
    executeEffect (DbModifyAccumUndo undos cont) daaU chgAccum =
        cont <$> daaModifyAccumUndo daaU chgAccum undos

-- | Actions to access and modify state.
data DbModifyActions chgAccum undo xs m proof = DbModifyActions
    { dmaAccess :: DbAccessActionsU chgAccum undo xs m
    -- ^ Actions to execute 'DbAccessU' effect.
    , dmaApply  :: chgAccum -> m proof
    -- ^ Apply provided change accumulator to internal state
    -- and return proof of the internal state change.
    -- This proof is further to be used on light client
    -- in order to validate change without full state access
    -- (in case actions provide this functionality).
    }

data DbActionsException
    = DbApplyException Text
    | DbWrongIdQuery Text
    | DbWrongPrefixIter Text
    | DbProtocolError Text
    | DbUndoProjectionError
    deriving Show

instance Exception DbActionsException

instance Buildable DbActionsException where
    build = \case
        DbApplyException t -> bprint ("DB apply exception: "%build) t
        DbWrongIdQuery t -> bprint ("Wrong id query to DB: "%build) t
        DbWrongPrefixIter t -> bprint ("Wrong prefix iterator: "%build) t
        DbProtocolError t -> bprint ("Db protocol error: "%build) t
        DbUndoProjectionError -> "Wrong undo object provided: projection failed"

-- | Toggle on whether to record proof within DbModifyActions
data RememberForProof = RememberForProof { unRememberForProof :: Bool }

data ClientMode proof
    = ProofMode { cmProof :: proof }
    | RemoteMode
