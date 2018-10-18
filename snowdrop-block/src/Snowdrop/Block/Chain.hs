module Snowdrop.Block.Chain
       ( iterateChain
       , wholeChain
       , forkDepthChain
       , nDepthChain
       , nDepthChainNE
       ) where

import           Universum

import           Snowdrop.Block.Configuration (BlkConfiguration (..), getCurrentBlockRef)
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (BlockRef, Blund (..), CurrentBlockRef (..),
                                       PrevBlockRef (..))
import           Snowdrop.Util (NewestFirst (..), OldestFirst (..), newestFirstFContainer,
                                toOldestFirst)

-- | Load up to @maxDepth@ blocks from the currently adopted block sequence.
iterateChain
    :: forall blkType m .
    ( Monad m
    )
    => BlkStateConfiguration blkType m
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
        | otherwise = bscGetBlund blockRef >>= \case
            Nothing -> pure $ NewestFirst [] -- TODO throw exception
            Just b  -> newestFirstFContainer (b:) <$>
                loadBlock
                  (depth - 1)
                  (unPrevBlockRef . (bcPrevBlockRef bscConfig) . buHeader $ b)

-- | 'wholeChain' retrieves list of Hashes of whole blockchain in oldest first order.
-- | 'forkDepthChain' retrieves list of Hashes from newest to maxForkDepth in oldest first order.
wholeChain, forkDepthChain
    :: Monad m
    => BlkStateConfiguration blkType m
    -> m (OldestFirst [] (CurrentBlockRef (BlockRef blkType)))
wholeChain     bsConf = nDepthChain bsConf maxBound
forkDepthChain bsConf = nDepthChain bsConf $ bcMaxForkDepth $ bscConfig bsConf

nDepthChain
    :: Monad m
    => BlkStateConfiguration blkType m
    -> Int
    -> m (OldestFirst [] (CurrentBlockRef (BlockRef blkType)))
nDepthChain bsConf depth = toOldestFirst . fmap (getCurrentBlockRef $ bscConfig bsConf) <$> iterateChain bsConf depth

-- | Retrieves 'depth' number of Hashes in oldest first order.
nDepthChainNE
    :: Monad m
    => BlkStateConfiguration blkType m
    -> Int
    -> m (Maybe (OldestFirst NonEmpty (Blund blkType)))
nDepthChainNE bsConf depth = toOldestFirstNE . toOldestFirst <$> iterateChain bsConf depth
  where
    toOldestFirstNE :: OldestFirst [] a -> Maybe (OldestFirst NonEmpty a)
    toOldestFirstNE (OldestFirst [])       = Nothing
    toOldestFirstNE (OldestFirst (x : xs)) = Just $ OldestFirst $ (x :| xs)
