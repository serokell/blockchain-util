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
import           Snowdrop.Block.Types (Block (..), Blund (..), CurrentBlockRef (..),
                                       PrevBlockRef (..))
import           Snowdrop.Util

data ForkVerResult header payload rawPayload undo blockRef
    = ApplyFork
      { fvrToApply    :: OldestFirst NonEmpty header
      , fvrToRollback :: NewestFirst [] (Blund header rawPayload undo)
      , fvrLCA        :: Maybe blockRef
      }
    | RejectFork

data ForkVerificationException blockRef
    = BlocksNotConsistent
    -- | Exception will be thrown when parent of alt chain doesn't belong to our chain
    | UnkownForkOrigin
    -- | Exception will be thrown when the first block of alt chain belongs to our chain
    | InvalidForkOrigin
    | TooDeepFork
    | OriginOfBlockchainReached
    | BlockDoesntExistInChain blockRef
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
    :: forall header payload rawBlock rawPayload undo blockRef e m osparams .
    ( MonadError e m
    , Eq blockRef
    , HasException e (ForkVerificationException blockRef)
    )
    => BlkStateConfiguration header payload rawBlock rawPayload undo blockRef m osparams
    -> osparams
    -> OldestFirst NonEmpty header
    -> m (ForkVerResult header payload rawPayload undo blockRef)
verifyFork BlkStateConfiguration{..} osParams fork@(OldestFirst altHeaders) = do
    let lcaRefMb = unPrevBlockRef $ bcPrevBlockRef bscConfig $ head altHeaders
    let firstBlockRef = unCurrentBlockRef $ bcBlockRef bscConfig $ head altHeaders

    -- At the beginning lcaRef doesn't exists since blockchain is empty
    case lcaRefMb of
        Just lcaRef -> unlessM (bscBlockExists lcaRef) $
                            throwLocalError @(ForkVerificationException blockRef) UnkownForkOrigin
        Nothing     -> pure ()
    whenM (bscBlockExists firstBlockRef) $
        throwLocalError @(ForkVerificationException blockRef) InvalidForkOrigin
    tip      <- bscGetTip
    curChain <- loadBlocksFromTo (bcMaxForkDepth bscConfig) lcaRefMb tip
    let altHeaders' :: OldestFirst [] header
        altHeaders' = oldestFirstFContainer NE.toList fork
    let curHeaders  = fmap (blkHeader . buBlock) (toOldestFirst curChain)
    pure $
      if bcIsBetterThan bscConfig altHeaders' curHeaders &&
         bcValidateFork bscConfig osParams altHeaders'
      then ApplyFork fork curChain lcaRefMb
      else RejectFork
  where
    loadBlocksFromTo
        :: Int -> Maybe blockRef -> Maybe blockRef
        -> m (NewestFirst [] (Blund header rawPayload undo))
    loadBlocksFromTo maxForkDepth toMb fromMb
        | toMb == fromMb      = pure $ NewestFirst []
        | maxForkDepth <= 0   = throwLocalError @(ForkVerificationException blockRef) TooDeepFork
        | Just from <- fromMb = bscGetBlund from >>= \case
            Just b  ->
                newestFirstFContainer (b:) <$>
                    loadBlocksFromTo
                      (maxForkDepth - 1)
                      toMb
                      (unPrevBlockRef $ bcPrevBlockRef bscConfig (blkHeader $ buBlock b))
            Nothing -> throwLocalError $ BlockDoesntExistInChain from
        | otherwise =
            throwLocalError @(ForkVerificationException blockRef) OriginOfBlockchainReached

iterateChain
    :: forall header payload rawBlock rawPayload undo blockRef m osparams .
    ( Monad m
    )
    => BlkStateConfiguration header payload rawBlock rawPayload undo blockRef m osparams
    -> Int
    -> m (NewestFirst [] (Blund header rawPayload undo))
iterateChain BlkStateConfiguration{..} maxDepth = bscGetTip >>= loadBlock maxDepth
  where
    loadBlock :: Int -> Maybe blockRef -> m (NewestFirst [] (Blund header rawPayload undo))
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
