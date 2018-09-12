{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Core.Expander
       ( SeqExpander (..)
       , SeqExpander'
       , PreExpander (..)

       , DiffChangeSet (..)
       , mkDiffCS
       ) where

import           Universum

import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NE

import           Snowdrop.Core.ChangeSet (ChangeSet (..), ValueOp (..))
import           Snowdrop.Core.ERoComp (ERoComp)
import           Snowdrop.Core.Prefix (Prefix)

-- type Expander e id value ctx rawTx (txtypes :: [*]) = Rec (PreExpanderSeq e id value ctx rawTx) txtypes

newtype SeqExpander e id value ctx rawTx
  = SeqExpander { getSeqExpanders :: SeqExpander' e id value ctx rawTx }

instance Contravariant (SeqExpander e id value ctx) where
    contramap g = SeqExpander . NE.map (contramap g) . getSeqExpanders

type SeqExpander' e id value ctx rawTx = NonEmpty (PreExpander e id value ctx rawTx)

-- | PreExpander allows you to convert one raw tx to StateTx.
--  _inpSet_ is set of Prefixes which expander gets access to during computation.
--  _outSet_ is set of Prefixes which expander returns as id of ChangeSet.
--  expanderAct takes raw tx, returns addition to txBody.
--  So the result StateTx is constructed as
--  _StateTx proofFromRawTx (addtionFromExpander1 <> additionFromExpander2 <> ...)_
data PreExpander e id value ctx rawTx = PreExpander
    { inpSet      :: Set Prefix
    , outSet      :: Set Prefix
    , expanderAct :: rawTx -> ERoComp e id value ctx (DiffChangeSet id value)
    }

instance Contravariant (PreExpander e id value ctx) where
    contramap g (PreExpander s1 s2 f) = PreExpander s1 s2 (f . g)

-- | DiffChangeSet holds changes which one expander returns
newtype DiffChangeSet id value = DiffChangeSet {unDiffCS :: ChangeSet id value}

mkDiffCS :: Map id (ValueOp v) -> DiffChangeSet id v
mkDiffCS = DiffChangeSet . ChangeSet
