{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Snowdrop.Core.Expander
       ( ProofNExp (..)
       , SeqExpander
       , PreExpander (..)
       , contramapProofNExp
       , contramapSeqExpander
       , contramapPreExpander
       , DiffChangeSet (..)

       , ExpRestriction (..)
       , ExpInpComps
       , ExpOutComps
       , SeqExpanderComponents
       ) where

import           Universum

import           Data.Kind
import           Data.Vinyl (Rec (..))

import           Snowdrop.Core.ChangeSet (HChangeSet)
import           Snowdrop.Core.ERoComp (ERoComp)
import           Snowdrop.Core.Transaction (TxProof)

newtype ProofNExp e ctx rawtx txtype =
    ProofNExp (TxProof txtype, SeqExpander e ctx rawtx txtype)

contramapProofNExp :: (a -> b) -> ProofNExp e ctx b txtype -> ProofNExp e ctx a txtype
contramapProofNExp f (ProofNExp (prf, se)) = ProofNExp (prf, contramapSeqExpander f se)

-- | Sequence of expand stages to be consequently executed upon a given transaction.
type SeqExpander e ctx rawtx txtype = Rec (PreExpander e ctx rawtx) (SeqExpanderComponents txtype)

contramapSeqExpander :: (a -> b) -> Rec (PreExpander e ctx b) xs -> Rec (PreExpander e ctx a) xs
contramapSeqExpander _ RNil         = RNil
contramapSeqExpander f (ex :& rest) = contramapPreExpander f ex :& contramapSeqExpander f rest

-- | PreExpander allows you to convert one raw tx to StateTx.
--  _inpSet_ is set of Prefixes which expander gets access to during computation.
--  _outSet_ is set of Prefixes which expander returns as id of ChangeSet.
--  expanderAct takes raw tx, returns addition to txBody.
--  So the result StateTx is constructed as
--  _StateTx proofFromRawTx (addtionFromExpander1 <> additionFromExpander2 <> ...)_
newtype PreExpander e ctx rawtx ioRestr = PreExpander
    { runExpander :: rawtx -> ERoComp e (ExpInpComps ioRestr) ctx (DiffChangeSet (ExpOutComps ioRestr))
    }

contramapPreExpander :: (a -> b) -> PreExpander e ctx b ioRestr -> PreExpander e ctx a ioRestr
contramapPreExpander f (PreExpander act) = PreExpander $ act . f

-- | DiffChangeSet holds changes which one expander returns
newtype DiffChangeSet xs = DiffChangeSet {unDiffCS :: HChangeSet xs}

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
