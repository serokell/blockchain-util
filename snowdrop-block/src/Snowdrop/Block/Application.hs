{-# LANGUAGE DataKinds #-}

module Snowdrop.Block.Application
       ( applyBlock
       , expandAndApplyBlock
       , tryApplyFork

       , BlockApplicationException (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Block.Configuration (BlkConfiguration (..), unBIV)
import           Snowdrop.Block.Fork (ForkVerResult (..), ForkVerificationException, verifyFork)
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockRef, Blund (..), CurrentBlockRef (..),
                                       BlockHeader, Payload, PrevBlockRef (..), RawBlk,
                                       OSParams, RawPayload)
import           Snowdrop.Util

data BlockApplicationException blockRef
    = TipMismatched (Maybe blockRef) (Maybe blockRef)
    | BlockIntegrityVerifierFailed
    deriving (Show)

instance Buildable blockRef => Buildable (BlockApplicationException blockRef) where
    build = \case
        TipMismatched given expected ->
            bprint ("Wrong reference to previous block, \
                    \given "%maybeF build%", expected (tip) "%maybeF build)
              given expected
        BlockIntegrityVerifierFailed ->
            "Block integrity verification failed"

-- | Applies an individual block if it's a direct continuation of currently adopted "best" chain.
-- Current implementation checks only block integrity,
-- no comparison with `bcIsBetterThan` is performed (which is better to be changed).
applyBlock
    :: forall blkType e m
    . ( MonadError e m
      , Eq (BlockRef blkType)
      , HasException e (BlockApplicationException (BlockRef blkType))
      , HasGetter (RawBlk blkType) (RawPayload blkType)
      )
    => OSParams blkType
    -> BlkStateConfiguration blkType m
    -> RawBlk blkType
    -> m ()
-- TODO: compare old chain with new one via `bcIsBetterThan`
applyBlock = expandAndApplyBlock True

expandAndApplyBlock
    :: forall blkType e m
    . ( MonadError e m
      , Eq (BlockRef blkType)
      , HasException e (BlockApplicationException (BlockRef blkType))
      , HasGetter (RawBlk blkType) (RawPayload blkType)
      )
    => Bool
    -> OSParams blkType
    -> BlkStateConfiguration blkType m
    -> RawBlk blkType
    -> m ()
expandAndApplyBlock checkBIV osParams bsc rawBlk = do
    blk <- bscExpand bsc rawBlk
    applyBlockImpl checkBIV osParams bsc (gett rawBlk) blk

applyBlockImpl
    :: forall blkType e m
    . ( MonadError e m
      , Eq (BlockRef blkType)
      , HasException e (BlockApplicationException (BlockRef blkType))
      )
    => Bool
    -> OSParams blkType
    -> BlkStateConfiguration blkType m
    -> RawPayload blkType
    -> Block (BlockHeader blkType) (Payload blkType)
    -> m ()
applyBlockImpl checkBIV osParams (BlkStateConfiguration {..}) rawPayload blk@Block{..} = do
    tip <- bscGetTip
    when (checkBIV && any not [ unBIV (bcBlkVerify bscConfig) blk
                              , bcValidateFork bscConfig osParams (OldestFirst [blkHeader])]) $
        throwLocalError $ BlockIntegrityVerifierFailed @(BlockRef blkType)
    let prev = unPrevBlockRef $ bcPrevBlockRef bscConfig blkHeader
    if prev == tip then do
        undo <- bscApplyPayload blkPayload
        bscStoreBlund $ Blund (Block blkHeader rawPayload) undo
        bscSetTip $ Just . unCurrentBlockRef $ bcBlockRef bscConfig blkHeader
    else
        throwLocalError $ TipMismatched prev tip

-- | Function `tryApplyFork` uses `verifyFork` from `Snowdrop.Block.Fork` in order to verify
-- block sequence and decide on whether proposed chain is better
-- than currently adopted "best" chain.
--
-- If `verifyFork` returns `ApplyFork`:
--
-- 1. rollback for a series of blocks is performed;
-- 2. tip is set to reference of block, which preceedes the first block in the fork;
-- 3. for each block in the fork: payload is applied, blund is stored and tip updated.
tryApplyFork
    -- TODO `undo` is not Monoid, even for ChangeSet
    :: forall blkType e m
    . ( HasGetter (RawBlk blkType) (BlockHeader blkType)
      -- pva701: TODO ^ this constraint should be eliminated and
      -- either expanding of headers should be made separately from blocks
      -- or fork should be verified using scheme:
      -- 1. rollback
      -- 2. expand alt chain
      -- 3. compare chains
      -- 4. apply appropriate chain
      , HasGetter (RawBlk blkType) (RawPayload blkType)
      , Eq (BlockRef blkType)
      , HasExceptions e [ ForkVerificationException (BlockRef blkType)
                        , BlockApplicationException (BlockRef blkType)]
      , MonadError e m
      )
    => BlkStateConfiguration blkType m
    -> OSParams blkType
    -> OldestFirst NonEmpty (RawBlk blkType)
    -> m Bool
tryApplyFork bcs@(BlkStateConfiguration {..}) osParams (OldestFirst rawBlocks) = do
    -- fork <- traverse toFork (unOldestFirst rawBlocks)
    verifyFork bcs osParams (OldestFirst $ NE.map gett rawBlocks) >>= \case
        RejectFork     -> pure False
        ApplyFork{..} -> do
            forM_ (unNewestFirst fvrToRollback) $ \blund -> do
                bscApplyUndo (buUndo blund)
                bscRemoveBlund $ unCurrentBlockRef $
                    bcBlockRef bscConfig (blkHeader $ buBlock blund)
            bscSetTip fvrLCA
            mapM_ (applyBlock osParams bcs) $ NE.toList rawBlocks
            pure True

-- How to express functionality which shall decide upon inclusion of fork into blockchain?
--

-- For block application we need diff of change a.k.a. undo

-- Can Storage be used to represent whole state?
-- Can Transaction with validator considering only inputs/outputs uniquely represent state change?
--    No.
--    1) We need periodically do some recomputation w/o any actual transactions
--        ^ Block boundary is a transaction? Which inputs to use?
--    2) Validator sometimes would require whole state traversal:
--         * leader computation
--         * update system stake snapshots
-- So we need to think how to restrict access of validator, while allowing it to read needed data.
-- Validator to state ids it needs after considering transaction in O(|payload size|)?
-- And via proofs express this set of ids to be bounded?


--
-- -------------------------
--
-- Also, what's the model of accounting? If Id ~> Value is upayloado,
-- we need to maintain accounts somehow. How?
--   ^ This is precisely doing some (Id ~> Value) transition in addition to what's state
--   in transaction. Perhaps it can be expressed via appropriate transaction type?
--   I.e. do a straightforward transition in-memory
-- This is easy, only thing we need to ensure is that each transaction
-- shall be a O(|payload size|) change of state
--
-- But Block boundary payload can not always be processed in O(|payload size|)!
-- We may do more elaborate analysis per payload type and explicitly distinguish transactions
-- for block boundaries for blocks `8k`, `10k` (or few other interesting blocks)
