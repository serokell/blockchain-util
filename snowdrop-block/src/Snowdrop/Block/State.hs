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
       , OpenBlockRawTxType
       , CloseBlockRawTxType

       , MempoolReasonableTx
       ) where

import           Universum

import           Data.Default (Default)
import qualified Data.Map as M
import qualified Data.Text.Buildable
import           Data.Union (UElem, USubset, Union, absurdUnion, ulift, union, urelax)
import           Data.Vinyl (Rec)
import           Data.Vinyl.TypeLevel (RImage, RIndex)

import           Snowdrop.Block.Application (CloseBlockRawTx (..), OpenBlockRawTx (..))
import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (BlockExpandedTx, BlockHeader, BlockRawTx, BlockRef,
                                       BlockUndo, Blund (..), CurrentBlockRef (..))
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..), Ctx, DbAccessU,
                                ERoComp, ERwComp, HChangeSet, HUpCastableChSet, HasBExceptions,
                                MappendHChSet, QueryERo, SomeTx, StatePException (..), StateTx (..),
                                TxComponents, TxRaw (..), TxRawImpl, Undo, UnitedTxType,
                                UpCastableERoM, Validator, ValueOp (..), applySomeTx, computeUndo,
                                convertEffect, getCAOrDefault, hChangeSetFromMap, liftERoComp,
                                modifyAccum, modifyAccumOne, modifyAccumUndo, queryOne,
                                queryOneExists, runValidator, upcastEffERoComp, upcastEffERoCompM)
import           Snowdrop.Execution (DbComponents, ExpandRawTxsMode, ExpandableTx, MempoolTx,
                                     ProofNExp (..), UnionSeqExpandersInps,
                                     runSeqExpandersSequentially)
import           Snowdrop.Util

data OpenBlockRawTxType blkType
data CloseBlockRawTxType blkType

instance UElem (OpenBlockRawTxType blkType) ts (RIndex (OpenBlockRawTxType blkType) ts)
  => HasReview (Union TxRaw ts) (OpenBlockRawTx blkType) where
    inj = inj . TxRaw @(OpenBlockRawTxType blkType)

instance UElem (CloseBlockRawTxType blkType) ts (RIndex (CloseBlockRawTxType blkType) ts)
  => HasReview (Union TxRaw ts) (CloseBlockRawTx blkType) where
    inj = inj . TxRaw @(CloseBlockRawTxType blkType)

type instance TxRawImpl (OpenBlockRawTxType blkType) = OpenBlockRawTx blkType
type instance TxRawImpl (CloseBlockRawTxType blkType) = CloseBlockRawTx blkType

type BlockRawTxs blkType = '[OpenBlockRawTxType blkType, CloseBlockRawTxType blkType]

instance
  BlockHeader blkType1 ~ BlockHeader blkType2
  => HasGetter (Union TxRaw ( BlockRawTxs blkType1 )) (Union TxRaw ( BlockRawTxs blkType2 )) where
    gett =
      union (union absurdUnion
        (ulift . TxRaw @(CloseBlockRawTxType blkType2) . CloseBlockRawTx . unCloseBlockRawTx . unTxRaw) )
      (ulift . TxRaw @(OpenBlockRawTxType blkType2) . OpenBlockRawTx . unOpenBlockRawTx . unTxRaw)

instance
  ( HasGetter (Union TxRaw (t2 : t3 : t4)) (Union TxRaw (t2' : t3' : t4'))
  , UElem t2' (t1 : t2' : t3' : t4') (RIndex t2' (t1 : t2' : t3' : t4'))
  , UElem t3' (t1 : t2' : t3' : t4') (RIndex t3' (t1 : t2' : t3' : t4'))
  , USubset t4' (t1 : t2' : t3' : t4') (RImage t4' (t1 : t2' : t3' : t4'))
  )
  => HasGetter (Union TxRaw (t1 : t2 : t3 : t4)) (Union TxRaw (t1 : t2' : t3' : t4')) where
    gett = union (urelax . gett @_ @(Union TxRaw (t2' : t3' : t4'))) ulift

type MempoolReasonableTx txtypes conf =
    CList '[ MempoolTx txtypes (DbComponents conf)
           , BlkProcConstr txtypes (DbComponents conf)
           , ExpandableTx txtypes
           ]

data TipComponent blkType
data BlundComponent blkType
type instance HKeyVal (TipComponent blkType) = '(TipKey, TipValue (BlockRef blkType))
type instance HKeyVal (BlundComponent blkType)  =
      '(BlockRef blkType, Blund blkType)

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
    , BlockExpandedTx blkType ~ SomeTx c
    , HasLens (ChgAccum conf) (ChgAccum conf)
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , HasGetter (BlockRawTx blkType) (SomeData TxRaw (Both (ExpandableTx txtypes) c))
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
    -> BlkStateConfiguration blkType m
inmemoryBlkStateConfiguration cfg validator exps = fix $ \this ->
    BlkStateConfiguration {
      bscConfig = cfg
    , bscExpand = liftERoComp . upcastEffERoCompM @_ @xs . runSeqExpandersSequentially exps . map gett
    , bscApplyPayload = \txs -> do
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
           let blockRef = unCurrentBlockRef $ bcBlockRef cfg (buHeader blund)
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
