{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Types
       (
         DbAccessActions (..)
       , DbAccessActionsM (..)
       , DbAccessActionsU (..)
       , DbModifyActions (..)
       , DbActionsException (..)
       , RememberForProof (..)
       , ClientMode (..)
       , DbActions (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Core (CSMappendException (..), ChangeSet, DbAccess (..), DbAccessM (..),
                                DbAccessU (..), FoldF (..), Prefix (..), StateP, StateR)
import           Snowdrop.Util (NewestFirst, OldestFirst)

-- | Actions to execute 'DbAccess' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActions chgAccum id value m = DbAccessActions
    { daaGetter :: chgAccum -> StateR id -> m (StateP id value)
    -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaIter   :: forall b . chgAccum -> Prefix -> b -> ((id, value) -> b -> b) -> m b
    -- ^ Iterate through keys with specified prefix (doesn't modify the state)
    }

-- | Actions to execute 'DbAccessM' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActionsM chgAccum id value m = DbAccessActionsM
    { daaAccess      :: DbAccessActions chgAccum id value m
    -- ^ Actions to execute 'DbAccess' effect.
    , daaModifyAccum :: chgAccum
                     -> OldestFirst [] (ChangeSet id value)
                     -> m (Either (CSMappendException id) (OldestFirst [] chgAccum))
    -- ^ Modify change accumulator with specified list of change sets.
    -- First argument is a change accumulator to modify.
    -- Second argument is a list of change sets (in oldest first order).
    -- Function applies each change set to accumulator in oldest first order,
    -- returns modified accumulator.
    }

-- | Actions to execute 'DbAccessU' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActionsU chgAccum undo id value m = DbAccessActionsU
    { daaAccessM          :: DbAccessActionsM chgAccum id value m
    -- ^ Actions to execute 'DbAccessM' effect.
    , daaModifyAccumUndo  :: chgAccum
                          -> NewestFirst [] undo
                          -> m (Either (CSMappendException id) chgAccum)
    -- ^ Applies undos to a given accumulator.
    -- First argument is a change accumulator to modify.
    -- Second argument is a list of undo object (in newest first order).
    -- Function applies each undo to accumulator in newest first order,
    -- returns modified accumulator.
    , daaComputeUndo      :: chgAccum
                          -> chgAccum
                          -> m (Either (CSMappendException id) undo)
    -- ^ Computes undo. First argument is a base change accumulator.
    -- Second argument is a modified change accumulator.
    -- Function returns an undo object, such that @modified `applyUndo` undo = base@
    }

-- | Class provided to execute effect with ations corresponding to it.
class DbActions effect actions param m where
    -- | Execute @effect r@ using @actions m@ and return value @r@ in monad @m@.
    executeEffect :: effect r -> actions m -> param -> m r

instance Functor m => DbActions (DbAccess id value)
                                (DbAccessActions chgAccum id value) chgAccum m where
    executeEffect (DbQuery req cont) daa chgAccum =
        cont <$> daaGetter daa chgAccum req
    executeEffect (DbIterator prefix (FoldF (e, acc, cont))) daa chgAccum =
        cont <$> daaIter daa chgAccum prefix e acc

instance Functor m => DbActions (DbAccessU chgAccum undo id value)
                                (DbAccessActionsU chgAccum undo id value) chgAccum m where
    executeEffect (DbAccessM daM) (daaAccessM -> daaM) chgAccum = executeEffect daM daaM chgAccum
    executeEffect (DbComputeUndo cs cont) daaU chgAccum =
        cont <$> daaComputeUndo daaU chgAccum cs
    executeEffect (DbModifyAccumUndo undos cont) daaU chgAccum =
        cont <$> daaModifyAccumUndo daaU chgAccum undos

instance Functor m => DbActions (DbAccessM chgAccum id value)
                                (DbAccessActionsM chgAccum id value) chgAccum m where
    executeEffect (DbAccess da) (daaAccess -> daa) chgAccum = executeEffect da daa chgAccum
    executeEffect (DbModifyAccum chgSet cont) daaM chgAccum =
        cont <$> daaModifyAccum daaM chgAccum chgSet

-- | Actions to access and modify state.
data DbModifyActions chgAccum undo id value m proof = DbModifyActions
    { dmaAccess :: DbAccessActionsU chgAccum undo id value m
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
    deriving Show

instance Exception DbActionsException

instance Buildable DbActionsException where
    build = \case
        DbApplyException t -> bprint ("DB apply exception: "%build) t
        DbWrongIdQuery t -> bprint ("Wrong id query to DB: "%build) t
        DbWrongPrefixIter t -> bprint ("Wrong prefix iterator: "%build) t
        DbProtocolError t -> bprint ("Db protocol error: "%build) t

-- | Toggle on whether to record proof within DbModifyActions
data RememberForProof = RememberForProof { unRememberForProof :: Bool }

data ClientMode proof
    = ProofMode { cmProof :: proof }
    | RemoteMode
