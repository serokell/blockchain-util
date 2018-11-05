{-# LANGUAGE Rank2Types #-}

module Snowdrop.Block.StateConfiguration
       ( BlkStateConfiguration (..)
       ) where

import           Universum

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.Types (BlockHeader, BlockRef, RawBlk, Tx)
import           Snowdrop.Util (OldestFirst)

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
data BlkStateConfiguration chgAccum blkType m = BlkStateConfiguration
    { bscExpandHeaders   :: chgAccum -> [ RawBlk blkType ] -> m ( OldestFirst [] (BlockHeader blkType) )
    -- ^ Required for step 1
    , bscExpandPayloads  :: chgAccum -> [ RawBlk blkType ] -> m ( OldestFirst [] (OldestFirst [] (Tx blkType, chgAccum)) )
    -- ^ Required for steps 4-5
    , bscValidateTx      :: chgAccum -> Tx blkType -> m ()
    -- ^ Required for step 6
    , bscGetHeader       :: BlockRef blkType -> m (Maybe (BlockHeader blkType))
    -- ^ Required for step 2
    , bscBlockExists     :: BlockRef blkType -> m Bool
    -- ^ Required for step 2
    , bscGetTip          :: m (Maybe (BlockRef blkType))
    -- ^ Required for step 2
    , bscInmemRollback   :: BlockRef blkType -> m chgAccum
    -- ^ Required for step 3
    , bscVerifyConfig    :: BlkConfiguration blkType
    -- ^ Required for step 2
    }
