{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Block.Chain
       ( wholeChain
       , forkDepthChain
       , loadBlocksFromTo
       , loadHeadersFrom
       , loadAllHeadersToNoThrow
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
  deriving Show

instance Buildable blkRef => Buildable (IterationException blkRef) where
    build TooDeepFork = "Provided fork is too deep."
    build OriginOfBlockchainReached =
        "Origin of blockchain reached before an LCA is found (block storage inconsistency)."
    build (BlockDoesntExistInChain ref) =
        bprint ("Block reference "%build%" not found (block storage inconsistency).") ref

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
      <$> loadHeadersFrom' False bsConf depth Nothing

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
loadHeadersFrom = loadHeadersFrom' True

loadAllHeadersToNoThrow
    :: forall chgAccum blkType e m iterE .
       ( MonadError e m
       , Eq (BlockRef blkType)
       , iterE ~ IterationException (BlockRef blkType)
       , HasReview e iterE
       )
    => BlkStateConfiguration chgAccum blkType m
    -> ToRef (BlockRef blkType)
    -> m (NewestFirst [] (BlockHeader blkType))
loadAllHeadersToNoThrow BlkStateConfiguration {..} to =
    loadBlocksFromTo'
        False
        bscGetHeader
        (bcPrevBlockRef bscVerifyConfig)
        maxBound
        Nothing
        to

loadHeadersFrom'
    :: forall chgAccum blkType e m iterE .
       ( MonadError e m
       , Eq (BlockRef blkType)
       , iterE ~ IterationException (BlockRef blkType)
       , HasReview e iterE
       )
    => Bool
    -> BlkStateConfiguration chgAccum blkType m
    -> Int
    -> Maybe (FromRef (BlockRef blkType))
    -> m (NewestFirst [] (BlockHeader blkType))
loadHeadersFrom' throwOnNotFound BlkStateConfiguration {..} depth from = do
    tip <- bscGetTip
    loadBlocksFromTo'
        throwOnNotFound
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
loadBlocksFromTo = loadBlocksFromTo' True

loadBlocksFromTo'
    :: forall blkRef blk e m iterE .
       ( MonadError e m
       , Eq blkRef
       , iterE ~ IterationException blkRef
       , HasReview e iterE
       )
    => Bool
    -> (blkRef -> m (Maybe blk))
    -> (blk -> PrevBlockRef blkRef)
    -> Int
    -> Maybe (FromRef blkRef)
    -> ToRef blkRef
    -> m (NewestFirst [] blk)
loadBlocksFromTo' throwOnNotFound getHeader getPrevBlkRef maxForkDepth fromMb (ToRef toMb)
    | Just toMb == fmap unFromRef fromMb =
        pure mempty
    | maxForkDepth <= 0 = do
        whenJust fromMb $ \_ -> throwLocalError @iterE TooDeepFork
        pure mempty
    | Just to <- toMb = getHeader to >>= \case
        Just b  ->
            newestFirstFContainer (b:) <$>
                loadBlocksFromTo'
                  throwOnNotFound
                  getHeader
                  getPrevBlkRef
                  (maxForkDepth - 1)
                  fromMb
                  (ToRef $ unPrevBlockRef $ getPrevBlkRef b)
        Nothing -> if throwOnNotFound
                      then throwLocalError $ BlockDoesntExistInChain to
                      else pure mempty
    | otherwise = do
        whenJust fromMb $ \_ -> throwLocalError @iterE OriginOfBlockchainReached
        pure mempty

