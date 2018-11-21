{-# LANGUAGE DataKinds #-}

module Snowdrop.Block.Exec.RawTx
       ( OpenBlockRawTxType
       , CloseBlockRawTxType
       , OpenBlockRawTx (..)
       , CloseBlockRawTx (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Data.Union (UElem, Union)
import           Data.Vinyl.TypeLevel (RIndex)

import           Snowdrop.Core (TxRaw (..), TxRawImpl)
import           Snowdrop.Util (DBuildable, HasReview (..))

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

data OpenBlockRawTxType header
data CloseBlockRawTxType header

instance UElem (OpenBlockRawTxType header) ts (RIndex (OpenBlockRawTxType header) ts)
  => HasReview (Union TxRaw ts) (OpenBlockRawTx header) where
    inj = inj . TxRaw @(OpenBlockRawTxType header)

instance UElem (CloseBlockRawTxType header) ts (RIndex (CloseBlockRawTxType header) ts)
  => HasReview (Union TxRaw ts) (CloseBlockRawTx header) where
    inj = inj . TxRaw @(CloseBlockRawTxType header)

type instance TxRawImpl (OpenBlockRawTxType header) = OpenBlockRawTx header
type instance TxRawImpl (CloseBlockRawTxType header) = CloseBlockRawTx header

-- instance
--   BlockHeader blkType1 ~ BlockHeader blkType2
--   => HasGetter (Union TxRaw ( BlockRawTxs blkType1 )) (Union TxRaw ( BlockRawTxs blkType2 )) where
--     gett =
--       union (union absurdUnion
--         (ulift . TxRaw @(CloseBlockRawTxType blkType2) . CloseBlockRawTx . unCloseBlockRawTx . unTxRaw) )
--       (ulift . TxRaw @(OpenBlockRawTxType blkType2) . OpenBlockRawTx . unOpenBlockRawTx . unTxRaw)
--
-- instance
--   ( HasGetter (Union TxRaw (t2 : t3 : t4)) (Union TxRaw (t2' : t3' : t4'))
--   , UElem t2' (t1 : t2' : t3' : t4') (RIndex t2' (t1 : t2' : t3' : t4'))
--   , UElem t3' (t1 : t2' : t3' : t4') (RIndex t3' (t1 : t2' : t3' : t4'))
--   , USubset t4' (t1 : t2' : t3' : t4') (RImage t4' (t1 : t2' : t3' : t4'))
--   )
--   => HasGetter (Union TxRaw (t1 : t2 : t3 : t4)) (Union TxRaw (t1 : t2' : t3' : t4')) where
--     gett = union (urelax . gett @_ @(Union TxRaw (t2' : t3' : t4'))) ulift
