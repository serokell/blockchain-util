module Snowdrop.Block.Demo
       ( blockSync
       ) where

import           Universum

import           Data.Default (Default)

import           Snowdrop.Block.Configuration (BlkConfiguration (..), getCurrentBlockRef,
                                               getPreviousBlockRef)
import           Snowdrop.Block.Fork (ForkVerificationException (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (BlockRef, BlockUndo, CurrentBlockRef (..), PrevBlockRef (..),
                                       RawBlk, RawBlund)
import           Snowdrop.Core (DbAccessU, ERwComp)
import           Snowdrop.Util


-- | 'blockSync' contains the logic of fork resolution on client.
-- Client calls 'blockSync' on list of hashes of blocks to check
-- whether server contains the same list of hashes or not
blockSync
  :: forall blkType e xs blockChgAccum ctx erwcomp.
    ( Default blockChgAccum
    , HasException e (ForkVerificationException (BlockRef blkType))
    , HasGetter (RawBlund blkType) (RawBlk blkType)
    , Eq (BlockRef blkType)
    , erwcomp ~ ERwComp e (DbAccessU blockChgAccum (BlockUndo blkType) xs) ctx blockChgAccum
    )
    => BlkStateConfiguration blkType erwcomp
    -> OldestFirst [] (BlockRef blkType)
    -> erwcomp (OldestFirst [] (RawBlk blkType))
blockSync config hashes = do
    tipBlockRefM <- bscGetTip config
    case tipBlockRefM of
        Nothing          -> pure (OldestFirst [])
        Just tipBlockRef ->
            loop (bcMaxForkDepth $ bscConfig config) tipBlockRef $
            OldestFirst []
    where
      -- | Loop is an iteration on result 'bscGetTip' called on config.
      -- Loop returns a list of blocks on server (if exists) by a given hash and
      -- list of hashes of desired blocks.
      loop
          :: Int
          -> BlockRef blkType
          -> OldestFirst [] (RawBlk blkType)
          -> erwcomp (OldestFirst [] (RawBlk blkType))
      loop depth from acc =
          case depth <= 0 of
              True -> pure acc
              False -> do
                  sBlundM <- bscGetBlund config from
                  case sBlundM of
                      Nothing     -> throwLocalError $ BlockDoesntExistInChain from
                      Just sBlund -> findLCA depth sBlund acc
      -- | 'findLCA' determines Lowest Common Ancestor by a given block with undo
      -- and list of blocks and returns to client blocks with respect to
      -- LCA.
      findLCA
          :: Int
          -> RawBlund blkType
          -> OldestFirst [] (RawBlk blkType)
          -> erwcomp (OldestFirst [] (RawBlk blkType))
      findLCA depth sBlund acc =
          let hashesList = unOldestFirst hashes in
          if elem cur hashesList
              then pure acc
              else case prev of
                  Nothing      -> pure $ OldestFirst nextAcc
                  Just prevRef -> loop (depth - 1) prevRef $ OldestFirst nextAcc
        where
          CurrentBlockRef cur =
              getCurrentBlockRef (bscConfig config) sBlund
          PrevBlockRef prev   =
              getPreviousBlockRef (bscConfig config) sBlund
          nextAcc =
              (gett @(RawBlund blkType) @(RawBlk blkType)) sBlund : unOldestFirst acc
