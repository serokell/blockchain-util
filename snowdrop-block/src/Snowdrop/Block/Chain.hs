{-# LANGUAGE AllowAmbiguousTypes #-}
module Snowdrop.Block.Chain
       ( iterateChain
       , wholeChain
       , forkDepthChain
       , nDepthChain
       , nDepthChainNE
       ) where

import           Universum

import           Snowdrop.Block.Configuration (BlkConfiguration (..), getCurrentBlockRef)
import           Snowdrop.Block.State (BlundComponent)
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockHeader, BlockRef, BlockUndo, Blund (..),
                                       CurrentBlockRef (..), PrevBlockRef (..), RawBlund,
                                       RawPayload)
import           Snowdrop.Core (ChgAccum, ERoComp, QueryERo, queryOne)
import           Snowdrop.Util (NewestFirst (..), OldestFirst (..), newestFirstFContainer,
                                toOldestFirst)

-- | Load up to @maxDepth@ blocks from the currently adopted block sequence.
iterateChain
    :: forall blkType conf xs m .
    ( QueryERo xs (BlundComponent blkType)
    , m ~ ERoComp conf xs
    )
    => BlkStateConfiguration (ChgAccum conf) blkType m
    -> Int -- ^ Max depth of block sequence to load.
    -> m (NewestFirst [] (Blund blkType))
iterateChain BlkStateConfiguration{..} maxDepth = bscGetTip >>= loadBlock maxDepth
  where
    loadBlock
        :: Int -> Maybe (BlockRef blkType)
        -> m (NewestFirst [] (Blund blkType))
    loadBlock _ Nothing = pure $ NewestFirst []
    loadBlock depth (Just blockRef)
        | depth <= 0 = pure $ NewestFirst []
        | otherwise = queryOne @(BlundComponent blkType) @_ @conf blockRef >>= \case
            Nothing -> pure $ NewestFirst [] -- TODO throw exception
            Just b  -> newestFirstFContainer (b:) <$>
                loadBlock
                  (depth - 1)
                  (unPrevBlockRef . (bcPrevBlockRef bscVerifyConfig) . blkHeader . buBlock $ b)

-- | 'wholeChain' retrieves list of Hashes of whole blockchain in oldest first order.
-- | 'forkDepthChain' retrieves list of Hashes from newest to maxForkDepth in oldest first order.
wholeChain, forkDepthChain
    :: forall blkType conf xs m .
    ( QueryERo xs (BlundComponent blkType)
    , m ~ ERoComp conf xs
    )
    => BlkStateConfiguration (ChgAccum conf) blkType m
    -> m (OldestFirst [] (CurrentBlockRef blkType))
wholeChain     bsConf = nDepthChain @_ @conf bsConf maxBound
forkDepthChain bsConf = nDepthChain @_ @conf bsConf $ bcMaxForkDepth $ bscVerifyConfig bsConf

nDepthChain
    :: forall blkType conf xs m .
    ( QueryERo xs (BlundComponent blkType)
    , m ~ ERoComp conf xs
    )
    => BlkStateConfiguration (ChgAccum conf) blkType m
    -> Int
    -> m (OldestFirst [] (CurrentBlockRef blkType))
nDepthChain bsConf depth = toOldestFirst . fmap (getCurrentBlockRef $ bscVerifyConfig bsConf) <$> iterateChain @_ @conf bsConf depth

-- | Retrieves 'depth' number of Hashes in oldest first order.
nDepthChainNE
    :: forall blkType conf xs m .
    ( QueryERo xs (BlundComponent blkType)
    , m ~ ERoComp conf xs
    )
    => BlkStateConfiguration (ChgAccum conf) blkType m
    -> Int
    -> m (Maybe (OldestFirst NonEmpty (Blund (BlockHeader blkType) (RawPayload blkType) (BlockUndo blkType))))
nDepthChainNE bsConf depth = toOldestFirstNE . toOldestFirst <$> iterateChain @_ @conf bsConf depth
  where
    toOldestFirstNE :: OldestFirst [] a -> Maybe (OldestFirst NonEmpty a)
    toOldestFirstNE (OldestFirst [])       = Nothing
    toOldestFirstNE (OldestFirst (x : xs)) = Just $ OldestFirst $ (x :| xs)
