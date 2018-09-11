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
       , iterateChain
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable
import           Formatting (bprint)

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockRef, Blund (..),
                                       CurrentBlockRef (..), BlockHeader,
                                       RawBlund, OSParams, PrevBlockRef (..))
import           Snowdrop.Util

data ForkVerResult blkType undo
  = ApplyFork
      { fvrToApply    :: OldestFirst NonEmpty (BlockHeader blkType)
      , fvrToRollback :: NewestFirst [] (RawBlund blkType undo)
      , fvrLCA        :: Maybe (BlockRef blkType)
      }
    | RejectFork

data ForkVerificationException blkType
    = BlocksNotConsistent
    -- | Exception will be thrown when parent of alt chain doesn't belong to our chain
    | UnkownForkOrigin
    -- | Exception will be thrown when the first block of alt chain belongs to our chain
    | InvalidForkOrigin
    | TooDeepFork
    | OriginOfBlockchainReached
    | BlockDoesntExistInChain blkType
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
    :: forall blkType e undo m .
       ( MonadError e m
       , Eq (BlockRef blkType)
       , HasException e (ForkVerificationException (BlockRef blkType))
       )
    => BlkStateConfiguration blkType undo m
    -> OSParams blkType
    -> OldestFirst NonEmpty (BlockHeader blkType)
    -> m (ForkVerResult blkType undo)
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
    let curHeaders  = fmap (blkHeader . buBlock) (toOldestFirst curChain)
    pure $
      if bcIsBetterThan bscConfig altHeaders' curHeaders &&
         bcValidateFork bscConfig osParams altHeaders'
      then ApplyFork fork curChain lcaRefMb
      else RejectFork
  where
    loadBlocksFromTo
        :: Int -> Maybe (BlockRef blkType) -> Maybe (BlockRef blkType)
        -> m (NewestFirst [] (RawBlund blkType undo))
    loadBlocksFromTo maxForkDepth toMb fromMb
        | toMb == fromMb        = pure $ NewestFirst []
        | maxForkDepth <= 0 = throwLocalError @(ForkVerificationException (BlockRef blkType)) TooDeepFork
        | Just from <- fromMb = bscGetBlund from >>= \case
            Just b  ->
                newestFirstFContainer (b:) <$>
                    loadBlocksFromTo
                      (maxForkDepth - 1)
                      toMb
                      (unPrevBlockRef $ bcPrevBlockRef bscConfig (blkHeader $ buBlock b))
            Nothing -> throwLocalError $ BlockDoesntExistInChain from
        | otherwise = throwLocalError @(ForkVerificationException (BlockRef blkType)) OriginOfBlockchainReached

iterateChain
    :: forall blkType undo m .
    ( Monad m
    )
    => BlkStateConfiguration blkType undo m
    -> Int
    -> m (NewestFirst [] (RawBlund blkType undo))
iterateChain BlkStateConfiguration{..} maxDepth = bscGetTip >>= loadBlock maxDepth
  where
    loadBlock
        :: Int -> Maybe (BlockRef blkType)
        -> m (NewestFirst [] (RawBlund blkType undo))
    loadBlock _ Nothing = pure $ NewestFirst []
    loadBlock depth (Just blockRef)
        | depth <= 0 = pure $ NewestFirst []
        | otherwise = bscGetBlund blockRef >>= \case
            Nothing -> pure $ NewestFirst []
            Just b  -> newestFirstFContainer (b:) <$>
                loadBlock
                  (depth - 1)
                  (unPrevBlockRef . (bcPrevBlockRef bscConfig) . blkHeader . buBlock $ b)

-- TODO this function may be needed in future

-- | Finds LCA of two containers `cont1`, `cont2`
--   Returns `(lca, cont1', cont2')` such that:
--    * `blkOrigin cont1' == blkOrigin cont2' == lca`
--    * `cont1'` and `cont2'` are heads of `cont1`, `cont2` respectively.
-- findLCA
--     :: (HasBlock blockRef payload bdata1, HasBlock blockRef payload bdata2, Ord blockRef)
--     => Int
--     -> BlockContainerD blockRef bdata1 payload
--     -> BlockContainerD blockRef bdata2 payload
--     -> Maybe (blockRef,
--               BlockContainerM blockRef bdata1 payload,
--               BlockContainerM blockRef bdata2 payload)
-- findLCA maxDepth cont1 cont2 = do
--     (lca, dropTail lca -> cont2) <- lcaM
--     (_, dropTail lca -> cont1) <- find ((==) lca . fst) refs1
--     assertOrigin lca cont1 $
--       assertOrigin lca cont2 $
--       return (lca, cont1, cont2)
--   where
--     assertOrigin origin = assert . maybe True ((== Just origin) . blkOrigin)

--     dropTail tailKey (m, tip)
--         | tip == tailKey = Nothing
--         | otherwise = Just (M.delete tailKey m, tip)

--     lcaM = find (flip S.member refs1Set . fst) refs2
--     NewestFirst (take maxDepth -> refs1) = blkHeads (Just cont1)
--     refs1Set = S.fromList (fst <$> refs1)
--     NewestFirst (take maxDepth -> refs2) = blkHeads (Just cont2)
