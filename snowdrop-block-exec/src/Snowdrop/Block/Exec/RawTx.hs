{-# LANGUAGE DataKinds #-}

module Snowdrop.Block.Exec.RawTx
       ( OpenBlockRawTxType
       , CloseBlockRawTxType
       ) where

import           Universum

import           Data.Union (UElem, USubset, Union, absurdUnion, ulift, union, urelax)
import           Data.Vinyl.TypeLevel (RImage, RIndex)

import           Snowdrop.Block (BlockHeader, CloseBlockRawTx (..), OpenBlockRawTx (..))
import           Snowdrop.Core (TxRaw (..), TxRawImpl)
import           Snowdrop.Util (HasGetter (..), HasReview (..))

data OpenBlockRawTxType blkType
data CloseBlockRawTxType blkType

instance UElem (OpenBlockRawTxType blkType) ts (RIndex (OpenBlockRawTxType blkType) ts)
  => HasReview (Union TxRaw ts) (OpenBlockRawTx blkType) where
    inj = inj . TxRaw @(OpenBlockRawTxType blkType)

instance UElem (CloseBlockRawTxType blkType) ts (RIndex (CloseBlockRawTxType blkType) ts)
  => HasReview (Union TxRaw ts) (CloseBlockRawTx blkType) where
    inj = inj . TxRaw @(CloseBlockRawTxType blkType)

type instance TxRawImpl (OpenBlockRawTxType blkType) = OpenBlockRawTx blkType
type instance TxRawImpl (CloseBlockRawTxType blkType) = CloseBlockRawTx blkType

type BlockRawTxs blkType = '[OpenBlockRawTxType blkType, CloseBlockRawTxType blkType]

instance
  BlockHeader blkType1 ~ BlockHeader blkType2
  => HasGetter (Union TxRaw ( BlockRawTxs blkType1 )) (Union TxRaw ( BlockRawTxs blkType2 )) where
    gett =
      union (union absurdUnion
        (ulift . TxRaw @(CloseBlockRawTxType blkType2) . CloseBlockRawTx . unCloseBlockRawTx . unTxRaw) )
      (ulift . TxRaw @(OpenBlockRawTxType blkType2) . OpenBlockRawTx . unOpenBlockRawTx . unTxRaw)

instance
  ( HasGetter (Union TxRaw (t2 : t3 : t4)) (Union TxRaw (t2' : t3' : t4'))
  , UElem t2' (t1 : t2' : t3' : t4') (RIndex t2' (t1 : t2' : t3' : t4'))
  , UElem t3' (t1 : t2' : t3' : t4') (RIndex t3' (t1 : t2' : t3' : t4'))
  , USubset t4' (t1 : t2' : t3' : t4') (RImage t4' (t1 : t2' : t3' : t4'))
  )
  => HasGetter (Union TxRaw (t1 : t2 : t3 : t4)) (Union TxRaw (t1 : t2' : t3' : t4')) where
    gett = union (urelax . gett @_ @(Union TxRaw (t2' : t3' : t4'))) ulift
