{-# LANGUAGE Rank2Types #-}

module Snowdrop.Block.Types
       ( Block (..)
       , HasBlock (..)
       , Blund (..)
       , CurrentBlockRef (..)
       , PrevBlockRef (..)
       , BlockRef
       , Payload
       , BlockHeader
       , BlockUndo
       , RawBlk
       , RawBlund
       , RawPayload
       , OSParams
       ) where

import           Universum

-------------------------------
-- Block storage
-------------------------------

-- | Block reference type, parametrized by unified parameter of block configuration @blkType@
type family BlockRef blkType :: *

-- | Block payload type, parametrized by unified parameter of block configuration @blkType@
type family Payload blkType :: *

-- | Block header type, parametrized by unified parameter of block configuration @blkType@
type family BlockHeader blkType :: *

-- | Block undo type, parametrized by unified parameter of block configuration @blkType@
type family BlockUndo blkType :: *

-- | OS params type, parametrized by unified parameter of block configuration @blkType@
type family OSParams blkType :: *

-- | Raw block type, parametrized by unified parameter of block configuration @blkType@
type family RawBlk blkType :: *

-- | Raw block's payload type, parametrized by unified parameter of block configuration @blkType@
type family RawPayload blkType :: *

-- | Type, representing a block.
-- Isomorphic to a pair @(header, payload)@.
data Block header payload = Block
    { blkHeader  :: header
    , blkPayload :: payload
    } deriving (Show, Eq, Ord, Generic)

-- TODO consider removing HasBlock type class and using $HasGetter in its stead

-- | Getter type class for a block
class HasBlock header payload b where
    getBlock :: b -> Block header payload

instance HasBlock header payload (Block header payload) where
    getBlock = identity

-- | Type for a block with respective undo object.
-- Isomorphic to a tuple @(header, payload, undo)@.
data Blund header payload undo = Blund
    { buBlock :: Block header payload
    , buUndo  :: undo
    } deriving (Eq, Ord, Show, Generic)

-- | $Blund type, parametrized with raw payload.
type RawBlund blkType =
    (Blund (BlockHeader blkType) (RawPayload blkType) (BlockUndo blkType))

instance HasBlock header payload (Blund header payload undo) where
    getBlock = getBlock . buBlock

-- | Wrapper type for reference of itself for some block.
newtype CurrentBlockRef blkType = CurrentBlockRef
    { unCurrentBlockRef :: BlockRef blkType
    }

-- | Wrapper type for reference of previous block for some block.
newtype PrevBlockRef blkType = PrevBlockRef
    { unPrevBlockRef :: (Maybe (BlockRef blkType))
    }
