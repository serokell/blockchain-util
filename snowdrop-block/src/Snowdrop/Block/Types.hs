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
       , RawBlk
       , RawBlund
       , RawPayload
       , OSParams
       , Time
       ) where

import           Universum

import           Data.Time.Clock (UTCTime)

type Time = UTCTime

-------------------------------
-- Block storage
-------------------------------

type family BlockRef a :: *

type family Payload a :: *

type family BlockHeader a :: *

type family OSParams a :: *

type family RawBlk a :: *

type family RawPayload a :: *

data Block header payload = Block
    { blkHeader  :: header
    , blkPayload :: payload
    } deriving (Show, Eq, Ord, Generic)

class HasBlock header payload b where
    getBlock :: b -> Block header payload

instance HasBlock header payload (Block header payload) where
    getBlock = identity

data Blund header payload undo = Blund
    { buBlock :: Block header payload
    , buUndo  :: undo
    } deriving (Eq, Ord, Show, Generic)

type RawBlund blkType undo =
    (Blund (BlockHeader blkType) (RawPayload blkType) undo)

instance HasBlock header payload (Blund header payload undo) where
    getBlock = getBlock . buBlock

newtype CurrentBlockRef blkType = CurrentBlockRef
    { unCurrentBlockRef :: BlockRef blkType
    }

newtype PrevBlockRef blkType = PrevBlockRef
    { unPrevBlockRef :: (Maybe (BlockRef blkType))
    }
