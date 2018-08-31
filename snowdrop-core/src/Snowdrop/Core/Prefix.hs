-- | TODO: this module fixes cycle dep between Snowdrop.Core.ERoComp.Types and
-- Snowdrop.Core.ChangeSet type,
-- which is caused by that fact that dbAccess is defined using ChangeSet and
-- Change set requires Prefix -- instance. Should be moved to Transaction.hs according to the plan

module Snowdrop.Core.Prefix
       (
         Prefix (..)
       , IdSumPrefixed (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, int, (%))

import           Snowdrop.Util

-- | Prefixes are usually used as extra identities for some keys.
newtype Prefix = Prefix Int
    deriving (Show, Eq, Ord, Enum)

instance Buildable Prefix where
    build (Prefix i) = bprint ("prefix #"%int) i

-- | Value-level mapping between some identity datatype and its
-- prefixes.
class IdSumPrefixed id where
    idSumPrefix :: id -> Prefix

instance HasReview Prefix Prefix where
    reviewOf = identity

instance HasPrism Prefix Prefix where
    proj = Just
