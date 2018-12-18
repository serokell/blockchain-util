{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module Snowdrop.Block.Exec.Application
    ( rollback
    , faaToChangeSet
    , loadBlunds
    ) where

import           Universum

import           Data.Default (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Data.Vinyl (Rec (..))

import           Snowdrop.Block (BlkConfiguration (..), BlkStateConfiguration (..), BlockHeader,
                                 BlockRef, CurrentBlockRef (..), ForkApplyAction (..), FromRef,
                                 IterationException, PrevBlockRef (..), RawBlk, ToRef (..),
                                 loadBlocksFromTo)
import           Snowdrop.Block.Exec.Storage (Blund (..), BlundComponent, TipComponent, TipKey (..),
                                              TipValue (..))
import           Snowdrop.Core (CSMappendException, ChgAccum, ChgAccumCtx, Ctx, ERoCompU,
                                HChangeSetEl (..), HUpCastableChSet, HasBException, HasBExceptions,
                                QueryERo, Undo, ValueOp (..), computeUndo, convertEffect,
                                modifyAccumOne, modifyAccumUndo, queryOne, withAccum)
import           Snowdrop.Hetero (hupcast)
import           Snowdrop.Util (HasGetter (..), HasLens (..), HasReview (..), NewestFirst (..),
                                OldestFirst (..), logInfo)

-- | Transforms ForkApplyAction into a change to underlying block storage.
--
-- Invariants:
--    * @length fork == length faaApplyBlocks@
faaToChangeSet :: forall conf blkUndo blkType xs
    . ( HasBException conf CSMappendException
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HasGetter (Undo conf) blkUndo
      , HUpCastableChSet '[TipComponent blkType, BlundComponent blkType blkUndo] xs
      , Ord (BlockRef blkType)
      )
    => Bool
    -> BlkConfiguration blkType
    -> OldestFirst NonEmpty (RawBlk blkType)
    -> ForkApplyAction (ChgAccum conf) blkType
    -> ERoCompU conf xs (ChgAccum conf)
faaToChangeSet preserveDeepBlocks cfg fork ForkApplyAction{..} = do
    undo0 <- withAccum faaRollbackedAcc $ computeUndo @xs @conf acc0
    undoRest <- flip traverse (zip (acc0 : caRest) caRest) $ \(prevAcc, acc) ->
        withAccum prevAcc $ computeUndo @xs @conf acc

    logInfo $ fromString $ "faaRefsToPrune: " <> show (length faaRefsToPrune)
    let undos = undo0 :| undoRest
        blunds = NE.zipWith Blund (unOldestFirst fork) (gett <$> undos)
        newBlundPairs = zip (unCurrentBlockRef . bcBlockRef cfg <$> takeNewest headers) (New <$> takeNewest blunds)
        remBlundPairs = (,Rem) <$> unNewestFirst (faaRollbackedRefs <> faaRefsToPrune)
        -- Invariant: rollbackedRefs ∩ newBlundRefs = ∅
        blundChg = M.fromList $ newBlundPairs <> remBlundPairs
        blkChg =
          hupcast @_ @'[TipComponent blkType, BlundComponent blkType blkUndo] $
              HChangeSetEl tipChg :& HChangeSetEl blundChg :& RNil

    withAccum (last chgAccums) $ convertEffect $ modifyAccumOne @conf @xs blkChg
  where
    (headers, chgAccums) = NE.unzip $ unOldestFirst faaApplyBlocks
    acc0 :| caRest = chgAccums

    -- If @preserveDeepBlocks@ is on, it's simply @NE.toList@
    -- Otherwise it leaves only @maxDepth@ last elements from the given list
    -- (or whole list if length of the list is less than @maxDepth@)
    takeNewest :: NonEmpty a -> [a]
    takeNewest = bool (NE.drop $ length fork - bcMaxForkDepth cfg) NE.toList preserveDeepBlocks

    tipChg = M.singleton TipKey (newTipChgCons newTipValue)
      where
        newTipChgCons = maybe (bool Upd New $ null faaRollbackedRefs)
                              (const Upd) $ unPrevBlockRef $ bcPrevBlockRef cfg (head headers)
        newTipValue = TipValue $ unCurrentBlockRef $ bcBlockRef cfg (last headers)

rollback
    :: forall conf blkUndo blkType xs .
      ( HasBExceptions conf
          [ IterationException (BlockRef blkType)
          , CSMappendException
          ]
      , HUpCastableChSet '[TipComponent blkType, BlundComponent blkType blkUndo] xs
      , QueryERo xs (TipComponent blkType)
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HasReview (Undo conf) blkUndo
      , QueryERo xs (BlundComponent blkType blkUndo)
      , Default (ChgAccum conf)
      , HasGetter (RawBlk blkType) (BlockHeader blkType)
      )
    => Int
    -> BlkStateConfiguration (ChgAccum conf) blkType (ERoCompU conf xs)
    -> ERoCompU conf xs (ChgAccum conf, NewestFirst [] (RawBlk blkType))
rollback rollbackBy bsConf = do
    blunds <- loadBlunds @conf bsConf rollbackBy Nothing
    case nonEmpty $ unNewestFirst blunds of
      Nothing -> pure (def, NewestFirst def)
      Just blundsNE -> do
        let tipChg =
              case unPrevBlockRef $ getPreviousBlockRef $ last blundsNE of
                  Nothing     -> Rem
                  Just newTip -> Upd (TipValue newTip)
            refs = unCurrentBlockRef . getCurrentBlockRef <$> blundsNE
            undos = inj . buUndo @_ @blkUndo <$> blunds
            blkChg =
              hupcast @_ @'[TipComponent blkType, BlundComponent blkType blkUndo] $
                (HChangeSetEl $ M.singleton TipKey tipChg)
                :& (HChangeSetEl $ M.fromList $ (,Rem) <$> toList refs)
                :& RNil
        acc0 <- convertEffect $ modifyAccumOne @conf @xs blkChg
        acc' <- withAccum acc0 $ modifyAccumUndo @_ @conf undos
        pure (acc', buRawBlk <$> blunds)
  where
    getCurrentBlockRef = bcBlockRef bc . gett . buRawBlk
    getPreviousBlockRef = bcPrevBlockRef bc . gett . buRawBlk
    bc = bscVerifyConfig bsConf

loadBlunds
  :: forall conf blkUndo blkType xs .
      ( HasBException conf (IterationException (BlockRef blkType))
      , Eq (BlockRef blkType)
      , HasGetter (RawBlk blkType) (BlockHeader blkType)
      , QueryERo xs (BlundComponent blkType blkUndo)
      )
    => BlkStateConfiguration (ChgAccum conf) blkType (ERoCompU conf xs)
    -> Int
    -> Maybe (FromRef (BlockRef blkType))
    -> ERoCompU conf xs (NewestFirst [] (Blund (RawBlk blkType) blkUndo))
loadBlunds bsConf maxDepth from = do
    tip <- bscGetTip bsConf
    loadBlocksFromTo
        (convertEffect . queryOne @(BlundComponent blkType blkUndo) @xs @conf)
        getPreviousBlockRef
        maxDepth
        from
        (ToRef tip)
  where
    bc = bscVerifyConfig bsConf
    getPreviousBlockRef = bcPrevBlockRef bc . gett . buRawBlk

