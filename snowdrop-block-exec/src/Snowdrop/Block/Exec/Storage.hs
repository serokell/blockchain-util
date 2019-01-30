{-# LANGUAGE DataKinds #-}

module Snowdrop.Block.Exec.Storage
       ( Blund (..)
       , TipComponent
       , BlundComponent

       , TipKey (..)
       , TipValue (..)

       , BlockStorage (..)
       , simpleBlockDbActions
       , bsTip
       , bsBlunds
       ) where

import           Universum

import           Control.Lens (lens, makeLenses)
import           Data.Default (def)
import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable
import           Data.Vinyl (Rec (..))

import           Snowdrop.Block (BlockRef, RawBlk)
import           Snowdrop.Dba.Base (DbModifyActions)
import           Snowdrop.Dba.Simple (HMapLensEl (..), SimpleConf, simpleDbActions)
import           Snowdrop.Hetero (HKeyVal)

-- | Type for a block header with respective undo object.
-- Isomorphic to a tuple @(header, undo)@.
-- Note, payload is not stored as for the block handling it's of no interest.
data Blund rawBlk undo = Blund
    { buRawBlk :: rawBlk
    , buUndo   :: undo
    }
    deriving (Eq, Show, Generic)

instance (Hashable rawBlk, Hashable undo) => Hashable (Blund rawBlk undo)

data TipComponent blkType
data BlundComponent blkType undo
type instance HKeyVal (TipComponent blkType) = '(TipKey, TipValue (BlockRef blkType))
type instance HKeyVal (BlundComponent blkType undo) = '(BlockRef blkType, Blund (RawBlk blkType) undo)

data TipKey = TipKey
  deriving (Eq, Ord, Show, Generic)

instance Buildable TipKey where
    build TipKey = "tip"

data TipValue blockRef = TipValue {unTipValue :: blockRef}
  deriving (Eq, Ord, Show, Generic)

data BlockStorage blkType undo = BlockStorage
    { _bsBlunds :: Map (BlockRef blkType) (Blund (RawBlk blkType) undo)
    , _bsTip    :: Maybe (TipValue (BlockRef blkType))
    }

makeLenses ''BlockStorage

bsTipMap
  :: ( Show (BlockRef blkType)
     , Buildable (BlockRef blkType)
     , Typeable (BlockRef blkType)
     )
  => Lens' (BlockStorage blkType undo) (Map TipKey (TipValue (BlockRef blkType)))
bsTipMap = bsTip . maybeToMapLens
  where
    maybeToMapLens = lens (maybe def $ M.singleton TipKey) $ const (M.lookup TipKey)

simpleBlockDbActions
  :: forall blkType undo .
    ( Ord (BlockRef blkType)
    , Show (BlockRef blkType)
    , Buildable (BlockRef blkType)
    , Typeable (BlockRef blkType)
    )
 => STM (DbModifyActions (SimpleConf '[TipComponent blkType, BlundComponent blkType undo]) STM)
simpleBlockDbActions =
    simpleDbActions (BlockStorage def def)
        (HMapLensEl (bsTipMap @blkType) :& HMapLensEl bsBlunds :& RNil)
