{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

module Snowdrop.Block.Fork
       ( ForkVerResult (..)
       , ForkVerificationException (..)
       , verifyFork
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable
import           Formatting (bprint)

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (BlockHeader, BlockRef, Blund (..), CurrentBlockRef (..),
                                       OSParams, PrevBlockRef (..))
import           Snowdrop.Util

-- | Result of fork verification.
data ForkVerResult blkType
    = ApplyFork
      { fvrToApply    :: OldestFirst NonEmpty (BlockHeader blkType)
      -- ^ Headers of blocks to apply.
      , fvrToRollback :: NewestFirst [] (Blund blkType)
      -- ^ Blocks to rollback.
      , fvrLCA        :: Maybe (BlockRef blkType)
      -- ^ Last block to be remained after rollback and predecessor of first block to apply.
      -- Value @Nothing@ is returned in case of whole blockchain is rolled back.
      }
    -- ^ Fork verification decision is to apply given fork.
    | RejectFork
    -- ^ Fork verification decision is to reject given fork.

-- | Exception type for 'verifyFork'.
data ForkVerificationException blkType
    = BlocksNotConsistent
    -- ^ Chain of blocks is not self-consistent: either integrity berification of one of blocks failed
    -- or one of blocks in the list is not a predcessor of following one.
    | UnkownForkOrigin
    -- ^ Parent of alt chain doesn't belong to our chain.
    | InvalidForkOrigin
    -- ^ First block of alt chain belongs to our chain.
    | TooDeepFork
    -- ^ Given fork is too deep.
    | OriginOfBlockchainReached
    -- ^ Origin of blockchain reached before an LCA is found.
    | BlockDoesntExistInChain blkType
    -- ^ Wrong block reference occurred during chain traversal (of block that is not present in storage).
    deriving Show

instance Buildable (ForkVerificationException blockRef) where
    build _ = bprint ("Non eligible to forge")

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
verifyFork
    :: forall blkType e m .
       ( MonadError e m
       , Eq (BlockRef blkType)
       , HasReview e (ForkVerificationException (BlockRef blkType))
       )
    => BlkStateConfiguration blkType m
    -> OSParams blkType
    -> OldestFirst NonEmpty (BlockHeader blkType)
    -> m (ForkVerResult blkType)
verifyFork BlkStateConfiguration{..} osParams fork@(OldestFirst altHeaders) = do
    let lcaRefMb = unPrevBlockRef $ bcPrevBlockRef bscConfig $ head altHeaders
    let firstBlockRef = unCurrentBlockRef $ bcBlockRef bscConfig $ head altHeaders

    -- At the beginning lcaRef doesn't exists since blockchain is empty
    case lcaRefMb of
        Just lcaRef -> unlessM (bscBlockExists lcaRef) $
                            throwLocalError @(ForkVerificationException (BlockRef blkType)) UnkownForkOrigin
        Nothing     -> pure ()
    whenM (bscBlockExists firstBlockRef) $
        throwLocalError @(ForkVerificationException (BlockRef blkType)) InvalidForkOrigin
    tip <- bscGetTip
    curChain <- loadBlocksFromTo (bcMaxForkDepth bscConfig) lcaRefMb tip
    let altHeaders' :: OldestFirst [] (BlockHeader blkType)
        altHeaders' = oldestFirstFContainer NE.toList fork
    let curHeaders  = fmap buHeader (toOldestFirst curChain)
    pure $
      if bcIsBetterThan bscConfig altHeaders' curHeaders &&
         bcValidateFork bscConfig osParams altHeaders'
      then ApplyFork fork curChain lcaRefMb
      else RejectFork
  where
    loadBlocksFromTo
        :: Int -> Maybe (BlockRef blkType) -> Maybe (BlockRef blkType)
        -> m (NewestFirst [] (Blund blkType))
    loadBlocksFromTo maxForkDepth toMb fromMb
        | toMb == fromMb        = pure $ NewestFirst []
        | maxForkDepth <= 0 = throwLocalError @(ForkVerificationException (BlockRef blkType)) TooDeepFork
        | Just from <- fromMb = bscGetBlund from >>= \case
            Just b  ->
                newestFirstFContainer (b:) <$>
                    loadBlocksFromTo
                      (maxForkDepth - 1)
                      toMb
                      (unPrevBlockRef $ bcPrevBlockRef bscConfig (buHeader b))
            Nothing -> throwLocalError $ BlockDoesntExistInChain from
        | otherwise = throwLocalError @(ForkVerificationException (BlockRef blkType)) OriginOfBlockchainReached
