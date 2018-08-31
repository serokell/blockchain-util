{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Core.Expander
       ( PreExpander (..)
       , Expander (..)
       , PreExpanderSeq (..)
       , PreExpandersSeq'

       , DiffChangeSet (..)
       , mkDiffCS
       ) where

import           Universum

import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NE

import           Snowdrop.Core.ChangeSet (ChangeSet (..), ValueOp (..))
import           Snowdrop.Core.ERoComp (ERoComp)
import           Snowdrop.Core.Prefix (Prefix)
import           Snowdrop.Core.Transaction (StateTxType)


newtype Expander e id proof value ctx rawTx = Expander
    { unExpander :: Map StateTxType (PreExpanderSeq e id proof value ctx rawTx) }

-- | PreExpander allows you to convert one raw tx to StateTx.
--  _inpSet_ is set of Prefixes which expander gets access to during computation.
--  _outSet_ is set of Prefixes which expander returns as id of ChangeSet.
--  expanderAct takes raw tx, returns addition to txBody.
--  So the result StateTx is constructed as
--  _StateTx txType proofFromRawTx (addtionFromExpander1 <> additionFromExpander2 <> ...)_
data PreExpander e id proof value ctx rawTx = PreExpander
    { inpSet      :: Set Prefix
    , outSet      :: Set Prefix
    , expanderAct :: rawTx -> ERoComp e id value ctx (DiffChangeSet id value)
    }

instance Contravariant (PreExpander e id proof value ctx) where
    contramap g (PreExpander s1 s2 f) = PreExpander s1 s2 (f . g)

type PreExpandersSeq' e id proof value ctx rawTx = NonEmpty (PreExpander e id proof value ctx rawTx)

newtype PreExpanderSeq e id proof value ctx rawTx
    = PreExpanderSeq { getSeqExpanders :: PreExpandersSeq' e id proof value ctx rawTx }

instance Contravariant (PreExpanderSeq e id proof value ctx) where
    contramap g = PreExpanderSeq . NE.map (contramap g) . getSeqExpanders

-- | DiffChangeSet holds changes which one expander returns
newtype DiffChangeSet id value = DiffChangeSet {unDiffCS :: ChangeSet id value}

mkDiffCS :: Map id (ValueOp v) -> DiffChangeSet id v
mkDiffCS = DiffChangeSet . ChangeSet
