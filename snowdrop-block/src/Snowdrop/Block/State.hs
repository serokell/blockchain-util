{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module Snowdrop.Block.State
       ( BlockStateException (..)
       , TipKey (..)
       , TipValue (..)
       , inmemoryBlkStateConfiguration
       ) where

import           Universum

import           Data.Default (Default)
import qualified Data.Map as M
import qualified Data.Text.Buildable

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockHeader, BlockRef, BlockUndo, Blund (..),
                                       CurrentBlockRef (..), Payload, RawBlk, RawPayload)
import           Snowdrop.Core (CSMappendException (..), ChangeSet (..), ChgAccum,
                                ChgAccumCtx (CAInitialized), DbAccessU, ERwComp, HasKeyValue,
                                IdSumPrefixed (..), SomeTx, StatePException (..), StateTx (..),
                                Validator, ValueOp (..), applySomeTx, computeUndo, convertEffect,
                                getCAOrDefault, liftERoComp, modifyAccum, modifyAccumOne,
                                modifyAccumUndo, queryOne, queryOneExists, runValidator)
import           Snowdrop.Execution (ProofNExp (..), RestrictCtx, RestrictionInOutException,
                                     expandUnionRawTxs)
import           Snowdrop.Util

data BlockStateException id
    = TipNotFound
    deriving (Show)

instance Buildable (BlockStateException id) where
    build TipNotFound = "Tip not found"

data TipKey = TipKey
  deriving (Eq, Ord, Show, Generic)

instance Buildable TipKey where
    build TipKey = "tip"

{-
instance Buildable blockRef => Buildable (BlockRef blockRef) where
    build br = bprint ("block ref "%build) br
-}

data TipValue blockRef = TipValue {unTipValue :: Maybe blockRef}
    deriving (Eq, Ord, Show, Generic)

-- | An implementation of `BlkStateConfiguration` on top of `ERwComp`.
-- It uniformly accesses state and block storage (via `DataAccess` interface).
inmemoryBlkStateConfiguration
  :: forall blkType e id value rawTx txtypes ctx .
    ( HasKeyValue id value TipKey (TipValue (BlockRef blkType))
    , HasKeyValue id value (BlockRef blkType) (Blund (BlockHeader blkType) (RawPayload blkType) (BlockUndo blkType))
    , HasExceptions e
        [ StatePException
        , BlockStateException id
        , CSMappendException id
        , RestrictionInOutException
        ]
    , Ord id
    , Ord (BlockRef blkType)
    , IdSumPrefixed id
    , HasLens ctx (ChgAccumCtx ctx)
    , HasLens ctx RestrictCtx
    , HasGetter (RawBlk blkType) [rawTx]
    , HasGetter (Payload blkType) [SomeTx id value (RContains txtypes)]
    , Default (ChgAccum ctx)
    )
    => BlkConfiguration blkType
    -> Validator e id value ctx txtypes
    -> (rawTx -> SomeData (ProofNExp e id value ctx rawTx) (RContains txtypes))
    -> (RawBlk blkType -> [SomeTx id value (RContains txtypes)] -> Block (BlockHeader blkType) (Payload blkType))
    -> BlkStateConfiguration blkType (ERwComp e (DbAccessU (ChgAccum ctx) (BlockUndo blkType) id value) ctx (ChgAccum ctx))
inmemoryBlkStateConfiguration cfg validator mkProof mkBlock =
    BlkStateConfiguration {
      bscConfig = cfg
    , bscExpand = \rawBlock -> do
          blkPayload <- liftERoComp $ expandUnionRawTxs mkProof (gett rawBlock)
          pure (mkBlock rawBlock blkPayload)
    , bscApplyPayload = \(gett @_ @[SomeTx id value (RContains txtypes)] -> txs) -> do
          (newSt, undo) <- liftERoComp $ do
              curAcc <- asks (getCAOrDefault @ctx . gett)
              OldestFirst accs <-
                  convertEffect $ modifyAccum (OldestFirst $ map (applySomeTx txBody) txs)
              let preAccs = init (curAcc :| accs)
                  lastAcc = last (curAcc :| accs)
              convertEffect $ mconcat $ flip map (zip preAccs txs) $ \(acc, tx) ->
                  local ( lensFor .~ CAInitialized @ctx acc ) $ applySomeTx (runValidator validator) tx
              (lastAcc,) <$> computeUndo @_ @id @value lastAcc
          undo <$ modify (flip sett newSt)
    , bscApplyUndo = liftERoComp . modifyAccumUndo @_ @id @value . pure >=> modify . flip sett
    , bscStoreBlund = \blund -> do
          let blockRef = unCurrentBlockRef $ bcBlockRef cfg (blkHeader $ buBlock blund)
          let (chg :: ChangeSet id value) = ChangeSet $ M.singleton (inj $ blockRef) (New $ inj blund)
          liftERoComp (modifyAccumOne chg) >>= modify . flip sett
    , bscRemoveBlund = \blockRef -> do
          let (chg :: ChangeSet id value) = ChangeSet $ M.singleton (inj blockRef) Rem
          liftERoComp (modifyAccumOne chg) >>= modify . flip sett
    , bscGetBlund = liftERoComp . queryOne @id @_ @value
    , bscBlockExists = liftERoComp . queryOneExists @id @_ @value
    , bscGetTip = liftERoComp (queryOne @id @_ @value TipKey)
                    >>= maybe (throwLocalError @(BlockStateException id) TipNotFound) (pure . unTipValue)
    , bscSetTip = \newTip' -> do
          let newTip = inj $ TipValue newTip'
          let tipChg = \cons -> ChangeSet $ M.singleton (inj TipKey) (cons newTip)
          oldTipMb <- liftERoComp $ queryOne @id @_ @value TipKey
          let (chg :: ChangeSet id value) = tipChg $ maybe New (const Upd) oldTipMb
          liftERoComp (modifyAccumOne chg) >>= modify . flip sett
    }
