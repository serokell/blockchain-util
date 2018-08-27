{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Core.Expander
       ( Expander (..)
       , SeqExpanders (..)
       , SeqExpanders'

       , DiffChangeSet (..)
       , mkDiffCS
       ) where

import           Universum

import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NE

import           Snowdrop.Core.ChangeSet (ChangeSet (..), ValueOp (..))
import           Snowdrop.Core.ERoComp (ERoComp)
import           Snowdrop.Core.Prefix (Prefix)

-- | Expander allows you to convert one raw tx to StateTx.
--  _inpSet_ is set of Prefixes which expander gets access to during computation.
--  _outSet_ is set of Prefixes which expander returns as id of ChangeSet.
--  expanderAct takes raw tx, returns addition to txBody.
--  So the result StateTx is constructed as
--  _StateTx txType proofFromRawTx (addtionFromExpander1 <> additionFromExpander2 <> ...)_
data Expander e id txtype value ctx rawTx = Expander
    { inpSet      :: Set Prefix
    , outSet      :: Set Prefix
    , expanderAct :: rawTx -> ERoComp e id value ctx (DiffChangeSet id value)
    }
instance Contravariant (Expander e id txtype value ctx) where
    contramap g (Expander s1 s2 f) = Expander s1 s2 (f . g)

type SeqExpanders' e id txtype value ctx rawTx = NonEmpty (Expander e id txtype value ctx rawTx)

newtype SeqExpanders e id txtype value ctx rawTx
  = SeqExpanders { getSeqExpanders :: SeqExpanders' e id txtype value ctx rawTx }

instance Contravariant (SeqExpanders e id txtype value ctx) where
    contramap g = SeqExpanders . NE.map (contramap g) . getSeqExpanders

-- | DiffChangeSet holds changes which one expander returns
newtype DiffChangeSet id value = DiffChangeSet {unDiffCS :: ChangeSet id value}

mkDiffCS :: Map id (ValueOp v) -> DiffChangeSet id v
mkDiffCS = DiffChangeSet . ChangeSet
