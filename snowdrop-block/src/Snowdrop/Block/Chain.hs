{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Block.Chain
       ( wholeChain
       , forkDepthChain
       , loadBlocksFromTo
       , loadHeadersFrom
       , IterationException (..)
       , FromRef (..)
       , ToRef (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (BlockHeader, BlockRef, CurrentBlockRef (..),
                                       PrevBlockRef (..))
import           Snowdrop.Util (HasReview, NewestFirst (..), OldestFirst (..),
                                newestFirstFContainer, throwLocalError, toOldestFirst)

data IterationException blkRef
    = TooDeepFork
    -- ^ Given fork is too deep.
    | OriginOfBlockchainReached
    -- ^ Origin of blockchain reached before an LCA is found.
    | BlockDoesntExistInChain blkRef
    -- ^ Wrong block reference occurred during chain traversal (of block that is not present in storage).

instance Buildable blkRef => Buildable (IterationException blkRef) where
    build TooDeepFork = "Provided fork is too deep."
    build OriginOfBlockchainReached =
        "Origin of blockchain reached before an LCA is found (block storage inconsistency)."
    build (BlockDoesntExistInChain ref) =
        bprint ("Block refernce "%build%" not found (block storage inconsistency).") ref

-- | 'wholeChain' retrieves list of Hashes of whole blockchain in oldest first order.
-- | 'forkDepthChain' retrieves list of Hashes from newest to maxForkDepth in oldest first order.
wholeChain, forkDepthChain
    :: ( MonadError e m
       , Eq (BlockRef blkType)
       , HasReview e (IterationException (BlockRef blkType))
       )
    => BlkStateConfiguration chgAccum blkType m
    -> m (OldestFirst [] (CurrentBlockRef (BlockRef blkType)))
wholeChain     bsConf = nDepthChain bsConf maxBound
forkDepthChain bsConf = nDepthChain bsConf $ bcMaxForkDepth $ bscVerifyConfig bsConf

nDepthChain
    :: ( MonadError e m
       , Eq (BlockRef blkType)
       , HasReview e (IterationException (BlockRef blkType))
       )
    => BlkStateConfiguration chgAccum blkType m
    -> Int
    -> m (OldestFirst [] (CurrentBlockRef (BlockRef blkType)))
nDepthChain bsConf depth =
    toOldestFirst . fmap (bcBlockRef $ bscVerifyConfig bsConf)
      <$> loadHeadersFrom bsConf depth Nothing

newtype FromRef blkRef = FromRef { unFromRef :: Maybe blkRef }
newtype ToRef blkRef = ToRef { unToRef :: Maybe blkRef }

loadHeadersFrom
    :: forall chgAccum blkType e m iterE .
       ( MonadError e m
       , Eq (BlockRef blkType)
       , iterE ~ IterationException (BlockRef blkType)
       , HasReview e iterE
       )
    => BlkStateConfiguration chgAccum blkType m
    -> Int
    -> Maybe (FromRef (BlockRef blkType))
    -> m (NewestFirst [] (BlockHeader blkType))
loadHeadersFrom BlkStateConfiguration {..} depth from = do
    tip <- bscGetTip
    loadBlocksFromTo
        bscGetHeader
        (bcPrevBlockRef bscVerifyConfig)
        depth
        from
        (ToRef tip)

loadBlocksFromTo
    :: forall blkRef blk e m iterE .
       ( MonadError e m
       , Eq blkRef
       , iterE ~ IterationException blkRef
       , HasReview e iterE
       )
    => (blkRef -> m (Maybe blk))
    -> (blk -> PrevBlockRef blkRef)
    -> Int
    -> Maybe (FromRef blkRef)
    -> ToRef blkRef
    -> m (NewestFirst [] blk)
loadBlocksFromTo getHeader getPrevBlkRef maxForkDepth fromMb (ToRef toMb)
    | Just toMb == (unFromRef <$> fromMb) =
        pure $ NewestFirst []
    | maxForkDepth <= 0 = do
        whenJust fromMb $ \_ -> throwLocalError @iterE TooDeepFork
        pure $ NewestFirst []
    | Just to <- toMb = getHeader to >>= \case
        Just b  ->
            newestFirstFContainer (b:) <$>
                loadBlocksFromTo
                  getHeader
                  getPrevBlkRef
                  (maxForkDepth - 1)
                  fromMb
                  (ToRef $ unPrevBlockRef $ getPrevBlkRef b)
        Nothing -> throwLocalError $ BlockDoesntExistInChain to
    | otherwise = do
        whenJust fromMb $ \_ -> throwLocalError @iterE OriginOfBlockchainReached
        pure $ NewestFirst []

