{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Snowdrop.Core.Expander
       ( SeqExpander (..)
       , SeqExpander'
       , PreExpander (..)
       , ExpRestriction (..)

       , DiffChangeSet (..)
       ) where

import           Universum

import           Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NE

import           Snowdrop.Core.ChangeSet (HChangeSet)
import           Snowdrop.Core.ERoComp (ERoComp)

-- type Expander e id value ctx rawTx (txtypes :: [*]) = Rec (PreExpanderSeq e id value ctx rawTx) txtypes

newtype SeqExpander e ctx xs rawTx
  = SeqExpander { getSeqExpanders :: SeqExpander' e ctx xs rawTx }

instance Contravariant (SeqExpander e ctx xs) where
    contramap g = SeqExpander . NE.map (contramap g) . getSeqExpanders

type SeqExpander' e ctx xs rawTx = NonEmpty (PreExpander e ctx xs rawTx)

-- | PreExpander allows you to convert one raw tx to StateTx.
--  _inpSet_ is set of Prefixes which expander gets access to during computation.
--  _outSet_ is set of Prefixes which expander returns as id of ChangeSet.
--  expanderAct takes raw tx, returns addition to txBody.
--  So the result StateTx is constructed as
--  _StateTx proofFromRawTx (addtionFromExpander1 <> additionFromExpander2 <> ...)_
newtype PreExpander e ctx ioRestr rawTx = PreExpander
    { runExpander :: rawTx -> ERoComp e ctx (ExpInps ioRestr) (DiffChangeSet (ExpOuts ioRestr))
    }

instance Contravariant (PreExpander e ctx ioRestr) where
    contramap g (PreExpander f) = PreExpander (f . g)

------------------------------------------
-- Restriction on PreExpander
------------------------------------------

-- This datatype to be intended to use as kind and constructor of types instead of pair
data ExpRestriction i o = ExRestriction i o -- different type and constructor names to avoid going crazy
type family ExpInps r where ExpInps ('ExRestriction i o) = i
type family ExpOuts r where ExpOuts ('ExRestriction i o) = o

-- | DiffChangeSet holds changes which one expander returns
newtype DiffChangeSet xs = DiffChangeSet {unDiffCS :: HChangeSet xs}
