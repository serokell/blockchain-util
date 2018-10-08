{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Types
       ( DbAccessActions (..)
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
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Data.Vinyl.Core (Rec)
import           Formatting (bprint, build, (%))

import           Snowdrop.Core (CSMappendException (..), ChgAccumModifier, Undo)
import           Snowdrop.Util (HKey, HMap, HSet, HVal)

type DGetter' xs m = HSet xs -> m (HMap xs)
type DGetter chgAccum xs m = chgAccum -> DGetter' xs m

type DModify chgAccum xs m = chgAccum -> ChgAccumModifier xs -> m (Either CSMappendException (chgAccum, Undo xs))

newtype IterAction m t = IterAction {runIterAction :: forall b . b -> ((HKey t, HVal t) -> b -> b) -> m b }
type DIter' xs m = Rec (IterAction m) xs
type DIter chgAccum xs m = chgAccum -> m (DIter' xs m)

data DbAccessActions chgAccum components m = DbAccessActions
    { daaGetter      :: DGetter chgAccum components m
      -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaModifyAccum :: DModify chgAccum components m
      -- ^ Modify change accumulater with specified change set (doesn't modify the state)
    , daaIter        :: DIter chgAccum components m
     -- ^ Iterate through keys with specified prefix (doesn't modify the state)
    }

-- | Db modify access actions model following use of
data DbModifyActions chgAccum components m proof = DbModifyActions
    { dmaAccessActions :: DbAccessActions chgAccum components m
    , dmaApply         :: chgAccum -> m proof
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
