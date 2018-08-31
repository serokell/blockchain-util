{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Types
       (
         DbAccessActions (..)
       , DbModifyActions (..)
       , DbActionsException (..)
       , RememberForProof (..)
       , ClientMode (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Core (CSMappendException (..), ChgAccumModifier, Prefix (..), StateP,
                                StateR, Undo)

data DbAccessActions chgAccum id value m = DbAccessActions
    { daaGetter      :: chgAccum -> StateR id -> m (StateP id value)
      -- ^ Retrieves values corresponding to specified keys (doesn't modify the state)
    , daaModifyAccum :: chgAccum
                     -> ChgAccumModifier id value
                     -> m (Either (CSMappendException id) (chgAccum, Undo id value))
      -- ^ Modify change accumulater with specified change set (doesn't modify the state)
    , daaIter        :: forall b . chgAccum -> Prefix -> b -> ((id, value) -> b -> b) -> m b
      -- ^ Iterate through keys with specified prefix (doesn't modify the state)
    }

-- | Db modify access actions model following use of
data DbModifyActions chgAccum id value m proof = DbModifyActions
    { dmaAccessActions :: DbAccessActions chgAccum id value m
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
