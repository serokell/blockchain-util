{-# LANGUAGE Rank2Types #-}

module Snowdrop.Block.StateConfiguration
       ( BlkStateConfiguration (..)
       ) where

import           Universum

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.Types (Block (..), Blund (..))

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
data BlkStateConfiguration header payload rawBlock rawPayload undo blockRef m =
  BlkStateConfiguration
    { bscApplyPayload :: payload -> m undo
    -- ^ Apply block payload to state. Note, that this method encapsulates validation of
    -- payload (and inidividual transactions contained in it) as well as actual application
    -- of payload to state.
    -- Payload application produces an undo object, which may further be used to revert
    -- the changes of payload application.

    , bscExpand       :: rawBlock -> m (Block header payload)
    -- ^ Expand raw block

    , bscApplyUndo    :: undo    -> m ()
    -- ^ Apply undo: revert changes made by application of block,
    -- which produced the passed undo object.

    , bscRemoveBlund  :: blockRef -> m ()
    -- ^ Remove block from block storage

    , bscStoreBlund   :: Blund header rawPayload undo -> m ()
    -- ^ Store block along with undo in block storage

    , bscGetBlund     :: blockRef -> m (Maybe (Blund header rawPayload undo))
    -- ^ Retrieve block along with undo from block storage

    , bscBlockExists  :: blockRef -> m Bool
    -- ^ Check on whether block with given reference exists in block storage

    , bscGetTip       :: m (Maybe blockRef)
    -- ^ Retrieve tip block reference
    -- (`Nothing` in case of empty chain being a currently adopted "best" chain)

    , bscSetTip       :: Maybe blockRef -> m ()
    -- ^ Update tip block reference for new adopted "best" chain
    -- (`Nothing` in case of empty chain)

    , bscConfig       :: BlkConfiguration header payload blockRef
    -- ^ Configuration for block sequence validation
    }
