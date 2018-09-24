{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Snowdrop.Execution.CreateBlock
       ( createBlock
       ) where

import           Universum

import           Data.Default (Default (..))
import           Loot.Base.HasLens (HasLens')
import           Loot.Log.Rio (LoggingIO)

import           Snowdrop.Block (BlkStateConfiguration (..), Block (..),
                                 BlockHeader, BlockRef, Blund (..), RawBlk, RawBlund)
import           Snowdrop.Execution.State (TipKey (..), TipValue (..), TipComponent, BlundComponent)
import           Snowdrop.Core (ERoComp, DbAccess, DbAccessM, convertEffect, queryOne)
import           Snowdrop.Execution.DbActions.Types (DbModifyActions (..), DbActions, DbAccessActionsM, daaAccessM)
import           Snowdrop.Execution.IOExecutor (runERoCompIO)
import           Snowdrop.Util (HasGetter (..), ExecM)

createBlock
    :: forall blkType e xs proof undo chgAccum
       ctx header rawTx blockDbComps blockChgAccum
       erocomp dbModifyActions.
    ( Default blockChgAccum
    , HasGetter (BlockHeader blkType) Int
    , HasGetter (header, [rawTx]) (RawBlk blkType)
    , erocomp ~ ERoComp e xs ctx
    , dbModifyActions ~ DbModifyActions chgAccum undo xs ExecM proof
    , blockDbComps ~ '[TipComponent blkType, BlundComponent blkType]
    , DbActions (DbAccessM blockChgAccum blockDbComps) (DbAccessActionsM chgAccum xs) blockChgAccum ExecM
    , Show e
    , Typeable e
    , MonadIO (ERoComp e xs ctx)
    , HasLens' ctx LoggingIO
    , Ord (BlockRef blkType)
    , Show (BlockRef blkType)
    )
    => BlkStateConfiguration blkType erocomp
    -> dbModifyActions
    -> header
    -> [rawTx]
    -> erocomp (RawBlk blkType)
createBlock BlkStateConfiguration{..} modification blockHeader rawTxs = do
    let blockDBA = daaAccessM $ dmaAccess modification
    (tipMb, diffMb) <- runERoCompIO @e blockDBA def $
      convertEffect @_ @_ @(DbAccess blockDbComps) @(DbAccessM blockChgAccum blockDbComps) $ do
        tipMb <- fmap unTipValue <$> queryOne @(TipComponent blkType) TipKey
        let getDiff (blund :: RawBlund blkType) = 1 + (gett @(BlockHeader blkType) @Int $ blkHeader $ buBlock blund)
        (,) tipMb <$> maybe (pure $ Just 1) (fmap (fmap getDiff) . queryOne @(BlundComponent blkType)) tipMb
    case diffMb of
        Just _ -> pure $ gett @((header, [rawTx])) @(RawBlk blkType) (blockHeader, rawTxs)
        Nothing   -> maybe (error "tip should be Just") (\tip -> error $ "Block for tip not found: " <> show tip) tipMb
