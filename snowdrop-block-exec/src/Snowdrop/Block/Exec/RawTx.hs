{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Snowdrop.Block.Exec.RawTx
       ( OpenBlockRawTxType
       , CloseBlockRawTxType
       , OpenBlockRawTx (..)
       , CloseBlockRawTx (..)
       ) where

import           Universum

import qualified Data.Text.Buildable

import           Snowdrop.Core (ExpRestriction, SeqExpanderComponents, TxProof, TxRawImpl)
import           Snowdrop.Util (DBuildable)

newtype OpenBlockRawTx header  = OpenBlockRawTx  { unOpenBlockRawTx :: header }
newtype CloseBlockRawTx header = CloseBlockRawTx { unCloseBlockRawTx :: header }

instance Buildable (CloseBlockRawTx h) where
    build _ = "Block close tx"
instance DBuildable (CloseBlockRawTx h)

instance Buildable (OpenBlockRawTx h) where
    build _ = "Block open tx"
instance DBuildable (OpenBlockRawTx h)

deriving instance Hashable header => Hashable (OpenBlockRawTx header)
deriving instance Hashable header => Hashable (CloseBlockRawTx header)

data OpenBlockRawTxType header (expRestr :: [ExpRestriction [*] [*]])
data CloseBlockRawTxType header (expRestr :: [ExpRestriction [*] [*]])

type instance TxRawImpl (OpenBlockRawTxType header expRestr) = OpenBlockRawTx header
type instance TxRawImpl (CloseBlockRawTxType header expRestr) = CloseBlockRawTx header

type instance TxProof (OpenBlockRawTxType header exp) = ()
type instance TxProof (CloseBlockRawTxType header exp) = ()

type instance SeqExpanderComponents (OpenBlockRawTxType header exp) = exp
type instance SeqExpanderComponents (CloseBlockRawTxType header exp) = exp
