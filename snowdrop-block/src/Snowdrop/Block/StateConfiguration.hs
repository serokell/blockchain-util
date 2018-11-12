{-# LANGUAGE Rank2Types #-}

module Snowdrop.Block.StateConfiguration
       ( BlkStateConfiguration (..)
       ) where

import           Universum

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.Types (BlockExpandedTx, BlockRawTx, BlockRef, BlockUndo, Blund)

-- | Block handling configuration.
-- Contains methods for to perform handling of block sequence (chain):
--
--  * load neccessary information from block storage,
--  * perform structural validation (using methods from `bscConfig :: BlkConfiguration`),
--  * apply validated sequence of blocks to current state and block storage.
--
--  Interface assumes two distinct storages to exist and be accessible via `m` monad:
--  block storage and state.
--
--  Block storage is used to store information required for block processing
--  (including tip block, currently adopted "best" chain),
--  while state contains actual blockchain state
--  as result of application of currently adopted "best" chain on initial blockchain state.
data BlkStateConfiguration blkType m = BlkStateConfiguration
    { bscApplyPayload :: [BlockExpandedTx blkType] -> m (BlockUndo blkType)
    -- ^ Apply block payload to state. Note, that this method encapsulates validation of
    -- payload (and inidividual transactions contained in it) as well as actual application
    -- of payload to state.
    -- Payload application produces an undo object, which may further be used to revert
    -- the changes of payload application.

    , bscExpand       :: [BlockRawTx blkType] -> m [BlockExpandedTx blkType]
    -- ^ Expand raw block

    , bscApplyUndo    :: BlockUndo blkType -> m ()
    -- ^ Apply undo: revert changes made by application of block,
    -- which produced the passed undo object.

    , bscRemoveBlund  :: BlockRef blkType -> m ()
    -- ^ Remove block from block storage

    , bscStoreBlund   :: Blund blkType -> m ()
    -- ^ Store block along with undo in block storage

    , bscGetBlund     :: BlockRef blkType
                      -> m (Maybe (Blund blkType))
    -- ^ Retrieve block along with undo from block storage

    , bscBlockExists  :: BlockRef blkType -> m Bool
    -- ^ Check on whether block with given reference exists in block storage

    , bscGetTip       :: m (Maybe (BlockRef blkType))
    -- ^ Retrieve tip block reference
    -- (`Nothing` in case of empty chain being a currently adopted "best" chain)

    , bscSetTip       :: Maybe (BlockRef blkType) -> m ()
    -- ^ Update tip block reference for new adopted "best" chain
    -- (`Nothing` in case of empty chain)

    , bscConfig       :: BlkConfiguration blkType
    -- ^ Configuration for block sequence validation
    }
