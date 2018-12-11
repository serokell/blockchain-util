{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module Snowdrop.Block.Exec.Extra
  ( BlockExtraH
  , Block (..)
  , BlkHeaderData
  ) where

import           Universum

import qualified Data.Foldable as F
import           Formatting (bprint, (%))

import           Snowdrop.Block (BlkStructuralData, BlockBodyProof, BlockRef)
import           Snowdrop.Util (DBuildable (..), HasGetter (..), OldestFirst (..), docF, indented,
                                newlineF)

-- | Block extra header data, parametrized by unified parameter of block configuration @blkType@
type family BlockExtraH blkType :: *

-- | Type, representing a block.
-- Isomorphic to a pair @(header, payload)@.
data Block header tx = Block
    { blkHeader  :: header
    , blkPayload :: OldestFirst [] tx
    }
    deriving (Eq, Show, Generic)

instance HasGetter (Block header tx) header where
    gett = blkHeader

instance Container (Block header tx) where
    type Element (Block header tx) = tx

instance Foldable (Block header) where
    foldr f b = F.foldr f b . blkPayload

instance (DBuildable header, DBuildable tx) => DBuildable (Block header tx) where
    dbuild (Block h p) dp =
        bprint ("Block header: "%docF (indented dp)%newlineF dp%
                "Block content: "%docF (indented dp)) h p

type BlkHeaderData blkType = (BlkStructuralData (BlockRef blkType) (BlockBodyProof blkType), BlockExtraH blkType)


