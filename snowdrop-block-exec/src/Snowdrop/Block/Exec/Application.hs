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
import           Snowdrop.Block.Exec.Storage (BlockUndo, Blund (..), BlundComponent, TipComponent,
                                              TipKey (..), TipValue (..))
import           Snowdrop.Core (CSMappendException, ChgAccum, ChgAccumCtx (..), Ctx, ERoCompU,
                                HChangeSetEl (..), HUpCastableChSet, HasBException, HasBExceptions,
                                QueryERo, Undo, ValueOp (..), computeUndo, convertEffect,
                                modifyAccumOne, modifyAccumUndo, queryOne, withAccum)
import           Snowdrop.Hetero (hupcast)
import           Snowdrop.Util (HasGetter (..), HasLens (..), HasReview (..), NewestFirst (..),
                                OldestFirst (..))

faaToChangeSet :: forall blkType conf xs
    . ( HasBException conf CSMappendException
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HasGetter (Undo conf) (BlockUndo blkType)
      , HUpCastableChSet '[TipComponent blkType, BlundComponent blkType] xs
      , Ord (BlockRef blkType)
      )
    => BlkConfiguration blkType
    -> OldestFirst NonEmpty (RawBlk blkType)
    -> ForkApplyAction (ChgAccum conf) blkType
    -> ERoCompU conf xs (ChgAccum conf)
faaToChangeSet cfg fork ForkApplyAction{..} = do
    undo0 <- withAccum @conf faaRollbackedAcc $ computeUndo @xs @conf acc0
    undoRest <- flip traverse (zip (acc0 : caRest) caRest) $ \(prevAcc, acc) ->
        withAccum @conf prevAcc $ computeUndo @xs @conf acc

    let undos = undo0 :| undoRest
        blunds = NE.zipWith (Blund @blkType) (unOldestFirst fork) (gett <$> undos)
        newBlundPairs = toList $ NE.zip (unCurrentBlockRef . bcBlockRef cfg <$> headers) (New <$> blunds)
        remBlundPairs = (,Rem) <$> unNewestFirst faaRollbackedRefs
        -- Invariant: rollbackedRefs ∩ newBlundRefs = ∅
        blundChg = M.fromList $ newBlundPairs <> remBlundPairs
        newTipChgCons = maybe (bool Upd New $ null faaRollbackedRefs)
                              (const Upd) $ unPrevBlockRef $ bcPrevBlockRef cfg (head headers)
        newTipValue = TipValue $ unCurrentBlockRef $ bcBlockRef cfg (last headers)
        tipChg = M.singleton TipKey (newTipChgCons newTipValue)
        blkChg =
          hupcast @_ @'[TipComponent blkType, BlundComponent blkType] $
              HChangeSetEl tipChg :& HChangeSetEl blundChg :& RNil

    withAccum @conf (last chgAccums) $ convertEffect $ modifyAccumOne @xs @conf blkChg
  where
    (headers, chgAccums) = NE.unzip $ unOldestFirst faaApplyBlocks
    acc0 :| caRest = chgAccums


rollback
    :: forall blkType conf xs .
      ( HasBExceptions conf
          [ IterationException (BlockRef blkType)
          , CSMappendException
          ]
      , HUpCastableChSet '[TipComponent blkType, BlundComponent blkType] xs
      , QueryERo xs (TipComponent blkType)
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      , HasReview (Undo conf) (BlockUndo blkType)
      , QueryERo xs (BlundComponent blkType)
      , Default (ChgAccum conf)
      , HasGetter (RawBlk blkType) (BlockHeader blkType)
      , HasReview (Undo conf) (BlockUndo blkType)
      )
    => Int
    -> BlkStateConfiguration (ChgAccum conf) blkType (ERoCompU conf xs)
    -> ERoCompU conf xs (ChgAccum conf, NewestFirst [] (RawBlk blkType))
rollback rollbackBy bsConf = do
    blunds <- loadBlunds bsConf rollbackBy Nothing
    case nonEmpty $ unNewestFirst blunds of
      Nothing -> pure (def, NewestFirst def)
      Just blundsNE -> do
        let tipChg =
              case unPrevBlockRef $ getPreviousBlockRef $ last blundsNE of
                  Nothing     -> Rem
                  Just newTip -> Upd (TipValue newTip)
            refs = unCurrentBlockRef . getCurrentBlockRef <$> blundsNE
            undos = inj . buUndo @blkType <$> blunds
            blkChg =
              hupcast @_ @'[TipComponent blkType, BlundComponent blkType] $
                (HChangeSetEl $ M.singleton TipKey tipChg)
                :& (HChangeSetEl $ M.fromList $ (,Rem) <$> toList refs)
                :& RNil
        acc0 <- convertEffect $ modifyAccumOne @xs @conf blkChg
        acc' <- withAccum @conf acc0 $ modifyAccumUndo undos
        pure (acc', buRawBlk <$> blunds)
  where
    getCurrentBlockRef = bcBlockRef bc . gett . buRawBlk
    getPreviousBlockRef = bcPrevBlockRef bc . gett . buRawBlk
    bc = bscVerifyConfig bsConf

loadBlunds
    :: forall blkType conf xs .
      ( HasBException conf (IterationException (BlockRef blkType))
      , Eq (BlockRef blkType)
      , HasGetter (RawBlk blkType) (BlockHeader blkType)
      , QueryERo xs (BlundComponent blkType)
      )
    => BlkStateConfiguration (ChgAccum conf) blkType (ERoCompU conf xs)
    -> Int
    -> Maybe (FromRef (BlockRef blkType))
    -> ERoCompU conf xs (NewestFirst [] (Blund blkType))
loadBlunds bsConf maxDepth from = do
    tip <- bscGetTip bsConf
    loadBlocksFromTo
        (convertEffect . queryOne @(BlundComponent blkType) @xs @conf)
        getPreviousBlockRef
        maxDepth
        from
        (ToRef tip)
  where
    bc = bscVerifyConfig bsConf
    getPreviousBlockRef = bcPrevBlockRef bc . gett . buRawBlk

