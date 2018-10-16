{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Block.State
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
import           Data.Vinyl (Rec)

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockHeader, BlockRef, BlockUndo, Blund (..),
                                       CurrentBlockRef (..), Payload, RawBlk, RawPayload)
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..), Ctx, DbAccessU,
                                ERoComp, ERwComp, HChangeSet, HUpCastableChSet, HasBExceptions,
                                MappendHChSet, QueryERo, SomeTx, StatePException (..), StateTx (..),
                                TxComponents, TxRaw, Undo, UnitedTxType, UpCastableERoM, Validator,
                                ValueOp (..), applySomeTx, computeUndo, convertEffect,
                                getCAOrDefault, hChangeSetFromMap, liftERoComp, modifyAccum,
                                modifyAccumOne, modifyAccumUndo, queryOne, queryOneExists,
                                runValidator, upcastEffERoComp, upcastEffERoCompM)
import           Snowdrop.Execution (ExpandRawTxsMode, ExpandableTx, ProofNExp (..),
                                     UnionSeqExpandersInps, runSeqExpandersSequentially)
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
  :: forall conf xs txtypes c .
    ( HasLens (Ctx conf) (ChgAccumCtx conf)
    , c ~ BlkProcConstr txtypes xs
    )
  => (forall txtype . c txtype => StateTx txtype -> ERoComp conf (TxComponents txtype) ())
  -> [(ChgAccum conf, SomeTx c)]
  -> ERoComp conf xs ()
mapTxs_ handleTxDo txsWithAccs =
    mconcat $
    flip map txsWithAccs $ \(acc, tx) ->
    local ( lensFor .~ CAInitialized @conf acc ) $
    handleTx tx
  where
    handleTx =
      applySomeTx $ \(stx :: StateTx txtype) ->
          upcastEffERoComp @(TxComponents txtype) @xs @conf (handleTxDo stx)

-- | An implementation of `BlkStateConfiguration` on top of `ERwComp`.
-- It uniformly accesses state and block storage (via `DataAccess` interface).
inmemoryBlkStateConfiguration
  :: forall blkType txtypes conf xs c m .
    ( xs ~ BlkProcComponents blkType txtypes c
    , c ~ BlkProcConstr txtypes xs
    , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
    , HasBExceptions conf
        [ StatePException
        , CSMappendException
        ]
    , Payload blkType ~ [SomeTx c]
    , HasLens (ChgAccum conf) (ChgAccum conf)
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , HasGetter (RawBlk blkType) [SomeData TxRaw (Both (ExpandableTx txtypes) c)]
    , HasGetter (Payload blkType) [SomeTx c]
    , Default (ChgAccum conf)
    , ExpandRawTxsMode conf txtypes
    , MappendHChSet xs

    , QueryERo xs (TipComponent blkType)
    , QueryERo xs (BlundComponent blkType)
    , HUpCastableChSet '[TipComponent blkType] xs
    , HUpCastableChSet '[BlundComponent blkType] xs
    , UpCastableERoM (UnionSeqExpandersInps txtypes) xs
    , Default (HChangeSet xs)
    , HasGetter (Undo conf) (BlockUndo blkType)
    , HasReview (Undo conf) (BlockUndo blkType)
    )
    => BlkConfiguration blkType
    -> Validator conf txtypes
    -> Rec (ProofNExp conf) txtypes
    -> (RawBlk blkType -> [SomeTx c] -> Block (BlockHeader blkType) [SomeTx c])
    -> BlkStateConfiguration blkType m
inmemoryBlkStateConfiguration cfg validator exps mkBlock = fix $ \this ->
    BlkStateConfiguration {
      bscConfig = cfg
    , bscExpand = \rawBlock -> do
          blkPayload <-
              liftERoComp $ upcastEffERoCompM @_ @xs $
                  runSeqExpandersSequentially exps (gett rawBlock)
          pure (mkBlock rawBlock blkPayload)
    , bscApplyPayload = \(gett @_ @[SomeTx (BlkProcConstr txtypes xs)] -> txs) -> do
          (newSt, undo) <- liftERoComp $ do
              curAcc <- asks (getCAOrDefault @conf . gett)
              OldestFirst accs <-
                  convertEffect @conf $ modifyAccum @xs @conf (OldestFirst $ map (applySomeTx $ hupcast . txBody) txs)
              let preAccs = init (curAcc :| accs)
                  lastAcc = last (curAcc :| accs)
              convertEffect @conf $ mapTxs_ @conf (runValidator validator) (zip preAccs txs)
              (lastAcc,) . gett <$> computeUndo @xs @conf lastAcc
          undo <$ modify (flip sett newSt)
     , bscApplyUndo = liftERoComp . modifyAccumUndo @xs @conf . pure . inj >=> modify . flip sett
     , bscStoreBlund = \blund -> do
           let blockRef = unCurrentBlockRef $ bcBlockRef cfg (blkHeader $ buBlock blund)
           let chg = hChangeSetFromMap @(BlundComponent blkType) $ M.singleton blockRef (New blund)
           applyBlkChg chg
     , bscRemoveBlund = \blockRef -> do
           let chg = hChangeSetFromMap @(BlundComponent blkType) $ M.singleton blockRef Rem
           applyBlkChg chg
     , bscGetBlund = liftERoComp . queryOne @(BlundComponent blkType) @xs @conf
     , bscBlockExists = liftERoComp . queryOneExists @(BlundComponent blkType) @xs @conf
     , bscGetTip = unTipValue <<$>> liftERoComp (queryOne @(TipComponent blkType) @xs @conf TipKey)
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
          liftERoComp (modifyAccumOne @xs @conf $ hupcast @_ @'[t] @xs chg)
              >>= modify . flip sett
