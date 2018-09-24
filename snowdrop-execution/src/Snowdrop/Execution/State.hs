{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Execution.State
       ( TipComponent
       , BlundComponent

       , TipKey (..)
       , TipValue (..)
       , inmemoryBlkStateConfiguration

       , BlkProcConstr
       ) where

import           Universum

import           Data.Default (Default)
import qualified Data.Map as M
import qualified Data.Text.Buildable

import           Snowdrop.Block (BlkConfiguration (..), BlkStateConfiguration (..),
                                 Block (..), BlockHeader, BlockRef, BlockUndo, Blund (..),
                                 CurrentBlockRef (..), Payload, RawBlk, RawPayload)
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..), DbAccessU,
                                ERoComp, ERwComp, HChangeSet, HUpCastableChSet, MappendHChSet,
                                QueryERo, SomeTx, StatePException (..), StateTx (..), TxComponents,
                                UnitedTxType, UpCastableERoM, Validator, ValueOp (..), applySomeTx,
                                computeUndo, convertEffect, getCAOrDefault, hChangeSetFromMap,
                                liftERoComp, modifyAccum, modifyAccumOne, modifyAccumUndo, queryOne,
                                queryOneExists, runValidator, upcastEffERoComp, upcastEffERoCompM)
import           Snowdrop.Execution.Expand (ExpandRawTxsMode, ExpandableTx, ProofNExp (..),
                                            UnionSeqExpandersInps, expandUnionRawTxs)
import           Snowdrop.Util

data TipComponent blkType
data BlundComponent blkType
type instance HKeyVal (TipComponent blkType) = '(TipKey, TipValue (BlockRef blkType))
type instance HKeyVal (BlundComponent blkType)  =
      '(BlockRef blkType, Blund (BlockHeader blkType) (RawPayload blkType) (BlockUndo blkType))

data TipKey = TipKey
  deriving (Eq, Ord, Show, Generic)

instance Buildable TipKey where
    build TipKey = "tip"

data TipValue blockRef = TipValue {unTipValue :: blockRef}
    deriving (Eq, Ord, Show, Generic)

class ( RContains txtypes txtype
      , HUpCastableChSet (TxComponents txtype) xs
      , UpCastableERoM (TxComponents txtype) xs
      ) => BlkProcConstr txtypes xs txtype
instance ( RContains txtypes txtype
         , HUpCastableChSet (TxComponents txtype) xs
         , UpCastableERoM (TxComponents txtype) xs
         ) => BlkProcConstr txtypes xs txtype

type BlkProcComponents blkType (txtypes :: [*]) (c :: * -> Constraint)
    = UnionTypes [TipComponent blkType, BlundComponent blkType]
                 (TxComponents (UnitedTxType txtypes))

mapTxs_
  :: forall e xs txtypes ctx c .
    ( HasLens ctx (ChgAccumCtx ctx)
    , c ~ BlkProcConstr txtypes xs
    )
  => (forall txtype . c txtype => StateTx txtype -> ERoComp e (TxComponents txtype) ctx ())
  -> [(ChgAccum ctx, SomeTx c)]
  -> ERoComp e xs ctx ()
mapTxs_ handleTxDo txsWithAccs =
    mconcat $
    flip map txsWithAccs $ \(acc, tx) ->
    local ( lensFor .~ CAInitialized @ctx acc ) $
    handleTx tx
  where
    handleTx =
      applySomeTx $ \(stx :: StateTx txtype) ->
          upcastEffERoComp @(TxComponents txtype) @xs (handleTxDo stx)

-- | An implementation of `BlkStateConfiguration` on top of `ERwComp`.
-- It uniformly accesses state and block storage (via `DataAccess` interface).
inmemoryBlkStateConfiguration
  :: forall blkType rawTx txtypes e ctx xs c m .
    ( xs ~ BlkProcComponents blkType txtypes c
    , c ~ BlkProcConstr txtypes xs
    , m ~ ERwComp e (DbAccessU (ChgAccum ctx) (BlockUndo blkType) xs) ctx (ChgAccum ctx)
    , HasExceptions e
        [ StatePException
        , CSMappendException
        ]
    , Payload blkType ~ [SomeTx c]
    , Ord (BlockHeader blkType)
    , HasLens (ChgAccum ctx) (ChgAccum ctx)
    , HasLens ctx (ChgAccumCtx ctx)
    , HasGetter (RawBlk blkType) [rawTx]
    , HasGetter (Payload blkType) [SomeTx (BlkProcConstr txtypes xs)]
    , Default (ChgAccum ctx)
    , ExpandRawTxsMode e ctx txtypes
    , MappendHChSet xs

    , QueryERo xs (TipComponent blkType)
    , QueryERo xs (BlundComponent blkType)
    , HUpCastableChSet '[TipComponent blkType] xs
    , HUpCastableChSet '[BlundComponent blkType] xs
    , UpCastableERoM (UnionSeqExpandersInps txtypes) xs
    , Default (HChangeSet xs)
    )
    => BlkConfiguration blkType
    -> Validator e ctx txtypes
    -> (rawTx -> SomeData (ProofNExp e ctx rawTx) (Both (ExpandableTx txtypes) c))
    -> (RawBlk blkType -> [SomeTx c] -> Block (BlockHeader blkType) (Payload blkType))
    -> BlkStateConfiguration blkType m
inmemoryBlkStateConfiguration cfg validator mkProof mkBlock = fix $ \this ->
    BlkStateConfiguration {
      bscConfig = cfg
    , bscExpand = \rawBlock -> do
          blkPayload <- liftERoComp $ upcastEffERoCompM @_ @xs $ expandUnionRawTxs mkProof (gett rawBlock)
          pure (mkBlock rawBlock blkPayload)
    , bscApplyPayload = \(gett @_ @[SomeTx (BlkProcConstr txtypes xs)] -> txs) -> do
          (newSt, undo) <- liftERoComp $ do
              curAcc <- asks (getCAOrDefault @ctx . gett)
              OldestFirst accs <-
                convertEffect $ modifyAccum @_ @xs (OldestFirst $ map (applySomeTx $ hupcast . txBody) txs)
              let preAccs = init (curAcc :| accs)
                  lastAcc = last (curAcc :| accs)
              convertEffect $ mapTxs_ (runValidator validator) (zip preAccs txs)
              (lastAcc,) <$> computeUndo @_ @xs lastAcc
          undo <$ modify (flip sett newSt)
     , bscApplyUndo = liftERoComp . modifyAccumUndo @_ @xs . pure >=> modify . flip sett
     , bscStoreBlund = \blund -> do
           let blockRef = unCurrentBlockRef $ bcBlockRef cfg (blkHeader $ buBlock blund)
           let chg = hChangeSetFromMap @(BlundComponent blkType) $ M.singleton blockRef (New blund)
           applyBlkChg chg
     , bscRemoveBlund = \blockRef -> do
           let chg = hChangeSetFromMap @(BlundComponent blkType) $ M.singleton blockRef Rem
           applyBlkChg chg
     , bscGetBlund = liftERoComp . queryOne @(BlundComponent blkType) @xs
     , bscBlockExists = liftERoComp . queryOneExists @(BlundComponent blkType) @xs
     , bscGetTip = unTipValue <<$>> liftERoComp (queryOne @(TipComponent blkType) @xs TipKey)
     , bscSetTip = \newTip' -> do
           oldTip <- bscGetTip this
           let applyChg tipMod = applyBlkChg $ hChangeSetFromMap @(TipComponent blkType) $ M.singleton TipKey tipMod
           case (newTip', oldTip) of
             (Nothing, Nothing) -> pure ()
             (Just v, Nothing)  -> applyChg $ New (TipValue v)
             (Just v, Just _)   -> applyChg $ Upd (TipValue v)
             (Nothing, Just _)  -> applyChg Rem
    }
    where
      applyBlkChg :: forall t . HUpCastableChSet '[t] xs => HChangeSet '[t] -> m ()
      applyBlkChg chg =
          liftERoComp (modifyAccumOne @_ @xs $ hupcast @_ @'[t] @xs chg)
              >>= modify . flip sett
