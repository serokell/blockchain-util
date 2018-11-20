{-# LANGUAGE DataKinds #-}

module Snowdrop.Block.Exec.Storage
       ( TipComponent
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

import           Snowdrop.Block (BlockRef, Blund)
import           Snowdrop.Dba.Base (DbModifyActions)
import           Snowdrop.Dba.Simple (HMapLensEl (..), SimpleConf, simpleDbActions)
import           Snowdrop.Hetero (HKeyVal)

data TipComponent blkType
data BlundComponent blkType
type instance HKeyVal (TipComponent blkType) = '(TipKey, TipValue (BlockRef blkType))
type instance HKeyVal (BlundComponent blkType)  =
      '(BlockRef blkType, Blund blkType)

data TipKey = TipKey
  deriving (Eq, Ord, Show, Generic)

instance Buildable TipKey where
    build TipKey = "tip"

data TipValue blockRef = TipValue {unTipValue :: blockRef}
  deriving (Eq, Ord, Show, Generic)

data BlockStorage blkType = BlockStorage
    { _bsBlunds :: Map (BlockRef blkType) (Blund blkType)
    , _bsTip    :: Maybe (TipValue (BlockRef blkType))
    }

makeLenses ''BlockStorage

bsTipMap
  :: ( Show (BlockRef blkType)
     , Buildable (BlockRef blkType)
     , Typeable (BlockRef blkType)
     )
  => Lens' (BlockStorage blkType) (Map TipKey (TipValue (BlockRef blkType)))
bsTipMap = bsTip . maybeToMapLens
  where
    maybeToMapLens = lens (maybe def $ M.singleton TipKey) $ const (M.lookup TipKey)

simpleBlockDbActions
  :: forall blkType .
    ( Ord (BlockRef blkType)
    , Show (BlockRef blkType)
    , Buildable (BlockRef blkType)
    , Typeable (BlockRef blkType)
    )
  => STM (DbModifyActions (SimpleConf '[TipComponent blkType, BlundComponent blkType]) STM)
simpleBlockDbActions =
    simpleDbActions (BlockStorage def def)
        (HMapLensEl (bsTipMap @blkType) :& HMapLensEl bsBlunds :& RNil)
