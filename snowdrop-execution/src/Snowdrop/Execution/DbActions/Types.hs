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

data DbAccessActions chgAccum id value m = DbAccessActions
    { daaGetter :: chgAccum -> StateR id -> m (StateP id value)
      -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaIter   :: forall b . chgAccum -> Prefix -> b -> ((id, value) -> b -> b) -> m b
      -- ^ Iterate through keys with specified prefix (doesn't modify the state)
    }

data DbAccessActionsM chgAccum id value m = DbAccessActionsM
    { daaAccess      :: DbAccessActions chgAccum id value m
      -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaModifyAccum :: chgAccum
                     -> OldestFirst [] (ChangeSet id value)
                     -> m (Either (CSMappendException id) (OldestFirst [] chgAccum))
      -- ^ Modify change accumulator with specified change set (doesn't modify the state)
    }

data DbAccessActionsU chgAccum undo id value m = DbAccessActionsU
    { daaAccessM          :: DbAccessActionsM chgAccum id value m
      -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaModifyAccumUndo  :: chgAccum
                          -> NewestFirst [] undo
                          -> m (Either (CSMappendException id) chgAccum)
    , daaComputeUndo      :: chgAccum
                             -- ^ base change accumulator
                          -> chgAccum
                             -- ^ modified change accumulator
                          -> m (Either (CSMappendException id) undo)
                             -- ^ undo object, such that @modified `applyUndo` undo = base@
    }

class DbActions effect actions param m where
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

-- | Db modify access actions model following use of
data DbModifyActions chgAccum undo id value m proof = DbModifyActions
    { dmaAccess :: DbAccessActionsU chgAccum undo id value m
    , dmaApply  :: chgAccum -> m proof
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
