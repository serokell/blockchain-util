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
import           Snowdrop.Block.Types (Block (..), BlockHeader, BlockRef, Blund (..),
                                       CurrentBlockRef (..), OSParams, Payload, PrevBlockRef (..),
                                       RawBlk, RawPayload)
import           Snowdrop.Util

-- | Exception type for block application.
data BlockApplicationException blockRef
    = TipMismatched (Maybe blockRef) (Maybe blockRef)
    -- ^ Expected one tip value, but encountered another.
    | BlockIntegrityVerifierFailed
    -- ^ Block integrity verification produced negative result.
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
      , HasReview e (BlockApplicationException (BlockRef blkType))
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
      , HasReview e (BlockApplicationException (BlockRef blkType))
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
      , HasReview e (BlockApplicationException (BlockRef blkType))
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
      , HasReviews e [ ForkVerificationException (BlockRef blkType)
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
