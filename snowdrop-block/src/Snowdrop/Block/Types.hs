{-# LANGUAGE Rank2Types #-}

module Snowdrop.Block.Types
       ( Block (..)
       , Blund (..)
       , CurrentBlockRef (..)
       , PrevBlockRef (..)
       , RawBlk
       , ExpandedBlk
       , BlkStructuralData (..)
       , BlockHeight (..)
       , BlkType (..)
       , BlkHeaderData
       , BlockExtraH
       ) where

import           Universum

import           Formatting (bprint, (%))

import           Snowdrop.Util (DBuildable (..), HasGetter, docF, indented, newlineF)

-------------------------------
-- Block storage
-------------------------------

-- | Type class which defines a set of type families required for block handling.
class HasGetter (BlockHeader blkType) (BlkStructuralData (BlockRef blkType) (BlockBodyProof blkType)) => BlkType blkType where
    -- | Block header type, parametrized by unified parameter of block configuration @blkType@
    type family BlockHeader blkType :: *

    -- | Block reference type, parametrized by unified parameter of block configuration @blkType@
    type family BlockRef blkType :: *

    -- | Block raw tx type, parametrized by unified parameter of block configuration @blkType@
    type family BlockRawTx blkType :: *

    -- | Block expanded tx type, parametrized by unified parameter of block configuration @blkType@
    type family BlockExpandedTx blkType :: *

    -- | Block undo type, parametrized by unified parameter of block configuration @blkType@
    type family BlockUndo blkType :: *

    -- | OS params type, parametrized by unified parameter of block configuration @blkType@
    type family OSParams blkType :: *

    -- | Proof of block body (data uniquely reffering to body, such as hash of block body or tx merkle tree root)
    type family BlockBodyProof blkType :: *

-- | Type, representing a block.
-- Isomorphic to a pair @(header, payload)@.
data Block header tx = Block
    { blkHeader  :: header
    , blkPayload :: [tx]
    }
    deriving (Eq, Show, Generic)


instance (DBuildable header, DBuildable rawTx) => DBuildable (Block header rawTx) where
    dbuild (Block h p) dp =
        bprint ("Block header: "%docF (indented dp)%newlineF dp%
                "Block content: "%docF (indented dp)) h p

instance (Hashable h, Hashable p) => Hashable (Block h p)
instance ( Hashable (BlockHeader blkType)
         , Hashable (BlockRawTx blkType)
         , Hashable (BlockUndo blkType)
         ) => Hashable (Blund blkType)


-- | Raw block type
type RawBlk blkType = Block (BlockHeader blkType) (BlockRawTx blkType)

-- | Expanded block type
type ExpandedBlk blkType = Block (BlockHeader blkType) (BlockExpandedTx blkType)

-- | Type for a block header with respective undo object.
-- Isomorphic to a tuple @(header, undo)@.
-- Note, payload is not stored as for the block handling it's of no interest.
data Blund blkType = Blund
    { buHeader  :: BlockHeader blkType
    , buPayload :: [BlockRawTx blkType]
    , buUndo    :: BlockUndo blkType
    }
    deriving Generic

deriving instance (Eq (BlockHeader blkType), Eq (BlockRawTx blkType), Eq (BlockUndo blkType)) => Eq (Blund blkType)
deriving instance (Show (BlockHeader blkType), Show (BlockRawTx blkType), Show (BlockUndo blkType)) => Show (Blund blkType)

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

-- | Block extra header data, parametrized by unified parameter of block configuration @blkType@
type family BlockExtraH blkType :: *

type BlkHeaderData blkType = (BlkStructuralData (BlockRef blkType) (BlockBodyProof blkType), BlockExtraH blkType)
