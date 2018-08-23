{-# LANGUAGE Rank2Types #-}

module Snowdrop.Block.Types
       ( Block (..)
       , HasBlock (..)
       , Blund (..)
       , CurrentBlockRef (..)
       , PrevBlockRef (..)
       ) where

import           Universum

-------------------------------
-- Block storage
-------------------------------

data Block header payload = Block
    { blkHeader  :: header
    , blkPayload :: payload
    } deriving (Show, Eq, Ord, Generic)

class HasBlock header payload b where
    getBlock :: b -> Block header payload

instance HasBlock header payload (Block header payload) where
    getBlock = identity

data Blund header payload undo = Blund {
    buBlock :: Block header payload
  , buUndo  :: undo
  } deriving (Eq, Ord, Show, Generic)

instance HasBlock header payload (Blund header payload undo) where
    getBlock = getBlock . buBlock

newtype CurrentBlockRef blockRef = CurrentBlockRef
    { unCurrentBlockRef :: blockRef
    }

newtype PrevBlockRef blockRef = PrevBlockRef
    { unPrevBlockRef :: (Maybe blockRef)
    }
