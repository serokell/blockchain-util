{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Snowdrop.Core.Expand.Type
       ( ProofNExp (..)
       , SeqExpander
       , PreExpander (..)
       , contramapSeqExpander
       , contramapPreExpander
       , DiffChangeSet (..)

       , ExpRestriction (..)
       , ExpInpComps
       , ExpOutComps
       , SeqExpanderComponents
       ) where

import           Universum

import           Data.Default (Default)
import           Data.Kind
import           Data.Vinyl (Rec (..))

import           Snowdrop.Core.ChangeSet (HChangeSet)
import           Snowdrop.Core.ERoComp (ERoComp)
import           Snowdrop.Core.Transaction (TxProof, TxRaw)

newtype ProofNExp conf txtype =
    ProofNExp (TxRaw txtype -> TxProof txtype, SeqExpander conf txtype)

-- | Sequence of expand stages to be consequently executed upon a given transaction.
type SeqExpander conf txtype = Rec (PreExpander conf (TxRaw txtype)) (SeqExpanderComponents txtype)

contramapSeqExpander :: (a -> b) -> Rec (PreExpander conf b) xs -> Rec (PreExpander conf a) xs
contramapSeqExpander _ RNil         = RNil
contramapSeqExpander f (ex :& rest) = contramapPreExpander f ex :& contramapSeqExpander f rest

-- | PreExpander allows you to convert one raw tx to StateTx.
--  _inpSet_ is set of Prefixes which expander gets access to during computation.
--  _outSet_ is set of Prefixes which expander returns as id of ChangeSet.
--  expanderAct takes raw tx, returns addition to txBody.
--  So the result StateTx is constructed as
--  _StateTx proofFromRawTx (addtionFromExpander1 <> additionFromExpander2 <> ...)_
newtype PreExpander conf rawTx ioRestr = PreExpander
    { runExpander :: rawTx
                  -> ERoComp conf (ExpInpComps ioRestr) (DiffChangeSet (ExpOutComps ioRestr))
    }

contramapPreExpander :: (a -> b) -> PreExpander conf b ioRestr -> PreExpander conf a ioRestr
contramapPreExpander f (PreExpander act) = PreExpander $ act . f

-- | DiffChangeSet holds changes which one expander returns
newtype DiffChangeSet xs = DiffChangeSet {unDiffCS :: HChangeSet xs}

deriving instance Eq (HChangeSet xs) => Eq (DiffChangeSet xs)
deriving instance Show (HChangeSet xs) => Show (DiffChangeSet xs)
deriving instance Semigroup (HChangeSet xs) => Semigroup (DiffChangeSet xs)
deriving instance Monoid (HChangeSet xs) => Monoid (DiffChangeSet xs)
deriving instance Default (HChangeSet xs) => Default (DiffChangeSet xs)

------------------------------------------
-- Restrictions of expanders
------------------------------------------

-- This datatype to be intended to use as kind and constructor of types instead of pair
data ExpRestriction i o = ExRestriction i o -- different type and constructor names to avoid going crazy
type family ExpInpComps r where ExpInpComps ('ExRestriction i o) = i
type family ExpOutComps r where ExpOutComps ('ExRestriction i o) = o

-- This type family should be defined for each seq expander like
-- type instance SeqExpanderComponents DlgTx =
--                  '[ ExRestriction '[TxIn] '[UtxoComponent],
--                     ExRestriction '[DlgIssuer, DlgDelegate] '[DlgIssuerComponent, DlgDelegateComponent]
--                   ]
-- this SeqExpander contains two PreExpanders
type family SeqExpanderComponents (txtype :: *) :: [ExpRestriction [*] [*]]
