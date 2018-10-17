{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Types
       (
         DbComponents
       , DbApplyProof
       , DbAccessActions (..)
       , DbAccessActionsM (..)
       , DbAccessActionsU (..)
       , IterAction (..)
       , DGetter
       , DGetter'
       , DIter
       , DIter'
       , DModify
       , DModify'
       , DbModifyActions (..)
       , DbActionsException (..)
       , RememberForProof (..)
       , ClientMode (..)
       , DbActions (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Data.Vinyl.Core (Rec)
import           Data.Vinyl.Recursive (rmap)
import           Formatting (bprint, build, (%))

import           Snowdrop.Core (CSMappendException (..), ChgAccum, DbAccess (..), DbAccessM (..),
                                DbAccessU (..), FoldF (..), HChangeSet, Undo)
import           Snowdrop.Util (HFunctor (..), HKey, HMap, HSet, HVal, NewestFirst, OldestFirst)

type family DbComponents conf :: [*]
type family DbApplyProof conf :: *

type DGetter' xs m = HSet xs -> m (HMap xs)
type DGetter conf m = ChgAccum conf -> DGetter' (DbComponents conf) m

type DModify' chgAccum xs m =
     OldestFirst [] (HChangeSet xs)
  -> m (Either CSMappendException (OldestFirst [] chgAccum))

type DModify conf m = ChgAccum conf -> DModify' (ChgAccum conf) (DbComponents conf) m

type DModifyUndo conf m =
       ChgAccum conf
    -> NewestFirst [] (Undo conf)
    -> m (Either CSMappendException (ChgAccum conf))

type DComputeUndo conf m =
       ChgAccum conf
    -> ChgAccum conf
    -> m (Either CSMappendException (Undo conf))

newtype IterAction m t = IterAction {runIterAction :: forall b . b -> (b -> (HKey t, HVal t) -> b) -> m b }
type DIter' xs m = Rec (IterAction m) xs
type DIter conf m = ChgAccum conf -> m (DIter' (DbComponents conf) m)

-- | Actions to execute 'DbAccess' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActions conf m = DbAccessActions
    { daaGetter :: DGetter conf m
    -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaIter   :: DIter conf m
    -- ^ Iterate through keys with specified prefix (doesn't modify the state)
    }

-- | Actions to execute 'DbAccessM' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActionsM conf m = DbAccessActionsM
    { daaAccess      :: DbAccessActions conf m
    -- ^ Actions to execute 'DbAccess' effect.
    , daaModifyAccum :: DModify conf m
    -- ^ Modify change accumulator with specified list of change sets.
    -- First argument is a change accumulator to modify.
    -- Second argument is a list of change sets (in oldest first order).
    -- Function applies each change set to accumulator in oldest first order,
    -- returns modified accumulator.
    }

-- | Actions to execute 'DbAccessU' effect.
-- Methods provided in the data type are not intended to modify any internal state.
data DbAccessActionsU conf m = DbAccessActionsU
    { daaAccessM         :: DbAccessActionsM conf m
    -- ^ Actions to execute 'DbAccessM' effect.
    , daaModifyAccumUndo :: DModifyUndo conf m
    -- ^ Applies undos to a given accumulator.
    -- First argument is a change accumulator to modify.
    -- Second argument is a list of undo object (in newest first order).
    -- Function applies each undo to accumulator in newest first order,
    -- returns modified accumulator.
    , daaComputeUndo     :: DComputeUndo conf m
    -- ^ Computes undo. First argument is a base change accumulator.
    -- Second argument is a modified change accumulator.
    -- Function returns an undo object, such that @modified `applyUndo` undo = base@
    }

-- | Class provided to execute effect with actions corresponding to it.
class DbActions effect actions param m where
    -- | Execute @effect r@ using @actions m@ and return value @r@ in monad @m@.
    executeEffect :: effect r -> actions m -> param -> m r

instance (chgAccum ~ ChgAccum conf, Monad m, xs ~ DbComponents conf) =>
    DbActions (DbAccess xs) (DbAccessActions conf) chgAccum m where

    executeEffect (DbQuery req cont) daa conf =
        cont <$> daaGetter daa conf req
    executeEffect (DbIterator getComp (FoldF (e, acc, cont))) daa conf =
        cont <$> ((\record -> runIterAction (getComp record) e acc) =<< daaIter daa conf)

instance (chgAccum ~ ChgAccum conf, Monad m, xs ~ DbComponents conf) =>
    DbActions (DbAccessM conf xs) (DbAccessActionsM conf) chgAccum m where

    executeEffect (DbAccess da) (daaAccess -> daa) conf = executeEffect da daa conf
    executeEffect (DbModifyAccum chgSet cont) daaM conf =
        cont <$> daaModifyAccum daaM conf chgSet

instance (chgAccum ~ ChgAccum conf, Monad m, xs ~ DbComponents conf) =>
    DbActions (DbAccessU conf xs) (DbAccessActionsU conf) chgAccum m where

    executeEffect (DbAccessM daM) (daaAccessM -> daaM) conf = executeEffect daM daaM conf
    executeEffect (DbComputeUndo cs cont) daaU conf =
        cont <$> daaComputeUndo daaU conf cs
    executeEffect (DbModifyAccumUndo undos cont) daaU chgAccum =
      cont <$> daaModifyAccumUndo daaU chgAccum undos

-- | Actions to access and modify state.
data DbModifyActions conf m = DbModifyActions
    { dmaAccess :: DbAccessActionsU conf m
    -- ^ Actions to execute 'DbAccessU' effect.
    , dmaApply  :: ChgAccum conf -> m (DbApplyProof conf)
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

instance HFunctor (DbAccessActions conf) where
    fmapH
      :: forall m n .
        Functor m
      => (forall x . m x -> n x)
      -> DbAccessActions conf m
      -> DbAccessActions conf n
    fmapH f (DbAccessActions {..}) =
      DbAccessActions
        { daaGetter = f ... daaGetter
        , daaIter = f . fmap (rmap fIterAction) . daaIter
        }
      where
        fIterAction :: IterAction m t -> IterAction n t
        fIterAction (IterAction g) = IterAction (f ... g)

instance HFunctor (DbAccessActionsM conf) where
    fmapH f (DbAccessActionsM {..}) =
      DbAccessActionsM
        { daaAccess = fmapH f daaAccess
        , daaModifyAccum = f ... daaModifyAccum
        }

instance HFunctor (DbAccessActionsU conf) where
    fmapH f (DbAccessActionsU {..}) =
      DbAccessActionsU
        { daaAccessM = fmapH f daaAccessM
        , daaModifyAccumUndo = f ... daaModifyAccumUndo
        , daaComputeUndo = f ... daaComputeUndo
        }

instance HFunctor (DbModifyActions conf) where
    fmapH f (DbModifyActions {..}) =
      DbModifyActions
        { dmaAccess = fmapH f dmaAccess
        , dmaApply = f ... dmaApply
        }
