{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

module Snowdrop.Block.Fork
       ( ForkVerificationException (..)
       , verifyForkStructure
       , processFork
       , ForkApplyAction (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable
import           Formatting (bprint, build, stext, (%))

import           Snowdrop.Block.Chain (FromRef (..), IterationException, ToRef (..),
                                       loadAllHeadersToNoThrow, loadHeadersFrom)
import           Snowdrop.Block.Configuration (BlkConfiguration (..), unBIV)
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (BlockHeader, BlockRef, CurrentBlockRef (..), OSParams,
                                       PrevBlockRef (..), RawBlk)
import           Snowdrop.Util (HasReview, NewestFirst, OldestFirst (..), VerRes (..),
                                newestFirstFContainer, throwLocalError, toOldestFirst,
                                verResToEither)

-- | Validate header chain integrity
blkHeadersConsistent
    :: forall blkType .
    ( Eq (BlockRef blkType)
    )
    => BlkConfiguration blkType
    -> OldestFirst NonEmpty (BlockHeader blkType)
    -> Bool
blkHeadersConsistent BlkConfiguration {..} (OldestFirst blks) =
    and $ flip map (zip (toList blks) prevRefs) $
        \(b, nextBlockPrevRef) -> nextBlockPrevRef == getBlockRef b
  where
    prevRefs = tail (map getPrevRef blks)

    getBlockRef, getPrevRef :: BlockHeader blkType -> Maybe (BlockRef blkType)
    getBlockRef = Just . unCurrentBlockRef . bcBlockRef
    getPrevRef  = unPrevBlockRef . bcPrevBlockRef


-- | Exception type for 'verifyFork'.
data ForkVerificationException blkRef
    = HeadersNotConsistent
    -- ^ Chain of blocks is not self-consistent: either integrity verification of one of blocks failed
    -- or one of blocks in the list is not a predcessor of following one.
    | UnknownForkOrigin
    -- ^ Parent of alt chain doesn't belong to our chain.
    | InvalidForkOrigin
    -- ^ First block of alt chain belongs to our chain.
    | CurrentIsBetterThanFork
    -- ^ Current chain is better than fork.
    | InvalidFork Text
    -- ^ Custom fork validation handler completed with negative result.
    | IntegrityVerificationFailed blkRef Text
    deriving Show

instance Buildable blkRef => Buildable (ForkVerificationException blkRef) where
    build HeadersNotConsistent =
        "Chain of headers is not self-consistent: "
          <> "one of blocks in the list is not a predcessor of following one."
    build UnknownForkOrigin = "Parent of alt chain doesn't belong to current chain."
    build InvalidForkOrigin = "First block of alt chain belongs to our chain."
    build CurrentIsBetterThanFork = "Current chain is better than fork."
    build (InvalidFork reason) =
        bprint ("Invalid fork: "%stext) reason
    build (IntegrityVerificationFailed ref reason) =
        bprint ("Integrity verififcation for block "%build%" failed: "%stext%".") ref reason

-- | Function `verifyFork` verifies fork validity
-- (applicability instead of currently adopted blockchain).
--
--  1. checks that fork is a valid consistent chain: each block is valid with respect to integrity
--  check `bcBlkVerify`, each subsequent block contains previous block reference;
--  2. checks that block, which preceedes the first block of fork, an LCA, exists in block storage;
--  3. checks that first block of fork doesn't exist in block storage;
--  4. loads at most `bcMaxForkDepth` blocks from block storage,
--  starting from tip block and ending with block,
--  referencing LCA (if more than `bcMaxForkDepth` need to be loaded, fails);
--  5. evaluates `bcIsBetterThan` for loaded part of currently adopted "best" chain and fork,
--  returns `ApplyFork` iff evaluation results in `True`.
verifyForkStructure
    :: forall blkType chgAccum e m fve .
       ( MonadError e m
       , Eq (BlockRef blkType)
       , fve ~ ForkVerificationException (BlockRef blkType)
       , HasReview e fve
       , HasReview e (IterationException (BlockRef blkType))
       )
    => BlkStateConfiguration chgAccum blkType m
    -> OSParams blkType
    -> OldestFirst NonEmpty (BlockHeader blkType)
    -> m ()
verifyForkStructure bsc@BlkStateConfiguration{..} osParams fork@(OldestFirst altHeaders) = do
    when (not $ blkHeadersConsistent bscVerifyConfig fork) $ throwLocalError @fve HeadersNotConsistent

    let lcaRefMb = unPrevBlockRef $ bcPrevBlockRef bscVerifyConfig $ head altHeaders
    let firstBlockRef = unCurrentBlockRef $ bcBlockRef bscVerifyConfig $ head altHeaders

    -- At the beginning lcaRef doesn't exists since blockchain is empty
    case lcaRefMb of
        Just lcaRef -> unlessM (bscBlockExists lcaRef) $
                          throwLocalError @fve UnknownForkOrigin
        Nothing     -> pure ()
    whenM (bscBlockExists firstBlockRef) $
        throwLocalError @fve InvalidForkOrigin
    curChain <- loadHeadersFrom bsc (bcMaxForkDepth bscVerifyConfig) (Just (FromRef lcaRefMb))
    let curHeaders  = toOldestFirst curChain
    either (throwLocalError @fve . InvalidFork) pure
        $ verResToEither $ bcValidateFork bscVerifyConfig osParams fork
    when (not $ bcIsBetterThan bscVerifyConfig curHeaders fork) $
        throwLocalError @fve CurrentIsBetterThanFork

data ForkApplyAction chgAccum blkType = ForkApplyAction
    { faaRollbackedRefs :: NewestFirst [] (BlockRef blkType)
    , faaRollbackedAcc  :: chgAccum
    , faaApplyBlocks    :: OldestFirst NonEmpty (BlockHeader blkType, chgAccum)
    , faaRefsToPrune    :: NewestFirst [] (BlockRef blkType)
    }


processFork
    :: forall blkType m e chgAccum .
      ( MonadError e m
      , HasReview e (ForkVerificationException (BlockRef blkType))
      , HasReview e (IterationException (BlockRef blkType))
      , Eq (BlockRef blkType)
      )
    => Bool
    -> BlkStateConfiguration chgAccum blkType m
    -> OSParams blkType
    -> OldestFirst NonEmpty (RawBlk blkType)
    -> chgAccum
    -> m (ForkApplyAction chgAccum blkType)
processFork preserveDeepBlocks bsc@BlkStateConfiguration {..} osParams fork acc0 = do

    -- 1. Expand headers from raw blocks of fork.
    forkHeaders <- bscExpandHeaders acc0 fork

    -- 2. Perform structured validation of chain (via verifyFork), which requires access to headers of fork, cur
    verifyForkStructure @_ @chgAccum bsc osParams forkHeaders

    -- 3. Verify integrity of each raw block
    let verifyBlkIntegrity (rawBlk, header) =
          case unBIV (bcBlkVerify bscVerifyConfig) rawBlk of
            VErr reason ->
              let ref = unCurrentBlockRef $ bcBlockRef bscVerifyConfig $ header
               in VErr (IntegrityVerificationFailed ref reason)
            VRes x -> VRes x

    case foldMap verifyBlkIntegrity $ NE.zip (unOldestFirst fork) (unOldestFirst forkHeaders) of
        VErr err -> throwLocalError err
        VRes ()  -> pure ()

    let mRollbackTo = unPrevBlockRef $ bcPrevBlockRef bscVerifyConfig $ head $ unOldestFirst forkHeaders

    -- 4. Retrieve blunds (block + undo pairs) from the storage, apply undos to in-memory accumulator acc0
    (acc1, rollbackedRefs) <- bscInmemRollback acc0 mRollbackTo

    -- 5. Expand raw block bodies of fork, get sequence of transactions tx1, tx2, ..., tx_k.
    -- Compute series of accumulators acc1, .., acc_{k-1}: acc_i := applyChangeSet acc_{i-1} (changeSet tx_i).
    --
    -- Following command generates a list with each element corresponding to a block of fork,
    -- containing pairs of transactions along with chg accumulators for after the tx is applied
    chgAccums <- bscValidatePayloads acc1 fork

    let applyBlocks = NE.zip (unOldestFirst forkHeaders) (unOldestFirst chgAccums)

    refsToPrune <-
      if preserveDeepBlocks
         then pure mempty
         else do
           -- collect refs for the blocks deeper than @maxDepth@ after application of the fork
           let startRef = unPrevBlockRef $ bcPrevBlockRef bscVerifyConfig $ head (unOldestFirst forkHeaders)
               dropNewest = drop $ bcMaxForkDepth bscVerifyConfig - length fork
           allRefs <- loadAllHeadersToNoThrow bsc (ToRef startRef)
           let refs = newestFirstFContainer dropNewest allRefs
           pure $ unCurrentBlockRef . bcBlockRef bscVerifyConfig <$> refs

    pure $ ForkApplyAction rollbackedRefs acc1 (OldestFirst applyBlocks) refsToPrune
