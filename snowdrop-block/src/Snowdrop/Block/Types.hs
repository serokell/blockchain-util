{-# LANGUAGE Rank2Types #-}

module Snowdrop.Block.Types
       ( CurrentBlockRef (..)
       , PrevBlockRef (..)
       , BlkStructuralData (..)
       , BlockHeight (..)
       , BlkType (..)
       ) where

import           Universum

import           Snowdrop.Util (HasGetter)

-- | Type class which defines a set of type families required for block handling.
class HasGetter (BlockHeader blkType) (BlkStructuralData (BlockRef blkType) (BlockBodyProof blkType)) => BlkType blkType where
    -- | Block header type, parametrized by unified parameter of block configuration @blkType@
    type family BlockHeader blkType :: *

    -- | Block reference type, parametrized by unified parameter of block configuration @blkType@
    type family BlockRef blkType :: *

    -- | OS params type, parametrized by unified parameter of block configuration @blkType@
    type family OSParams blkType :: *

    -- | Proof of block body (data uniquely reffering to body, such as hash of block body or tx merkle tree root)
    type family BlockBodyProof blkType :: *

    -- | Raw block type, parametrized by unified parameter of block configuration @blkType@
    type family RawBlk blkType :: *

-- | Wrapper type for reference of itself for some block.
newtype CurrentBlockRef blkRef = CurrentBlockRef
    { unCurrentBlockRef :: blkRef
    }

-- | Wrapper type for reference of previous block for some block.
newtype PrevBlockRef blkRef = PrevBlockRef
    { unPrevBlockRef :: (Maybe blkRef)
    }

-- | Data type for block height
newtype BlockHeight = BlockHeight { getBlockHeight :: Word }
  deriving (Num, Integral, Real, Enum, Ord, Eq, Show, Hashable, Buildable)

-- | Data, holding structural data for a block.
data BlkStructuralData blockRef blkBodyProof = BlkStructuralData
    { blkHeight    :: BlockHeight
    , blkPrevRef   :: Maybe blockRef
    , blkBodyProof :: blkBodyProof
    }
    deriving (Eq, Generic, Show)

instance (Hashable blkRef, Hashable blkBodyProof) => Hashable (BlkStructuralData blkRef blkBodyProof)
