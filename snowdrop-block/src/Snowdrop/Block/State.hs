{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Block.State
       ( BlockStateException (..)
       , TipKey (..)
       , TipValue (..)
       , inmemoryBlkStateConfiguration
       ) where

import           Universum

import           Data.Default (def)
import           Data.Default (Default)
import qualified Data.Map as M
import qualified Data.Text.Buildable

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockHeader, BlockRef, BlockUndo, Blund (..),
                                       CurrentBlockRef (..), Payload, RawBlk, RawPayload)
import           Snowdrop.Core (AvlRevisions, CSMappendException (..), ChgAccum, ChgAccumCtx,
                                ChgAccumModifier (..), ERwComp, HChangeSet, HUpCastableChSet,
                                MappendHChSet, QueryERo, SomeTx, StateModificationException,
                                StatePException (..), StateTx (..), TxComponents, Undo (..),
                                UnitedTxType, UpCastableERo, Validator, ValueOp (..), applySomeTx,
                                hChangeSetFromMap, liftERoComp, mappendChangeSet,
                                modifyRwCompChgAccum, queryOne, queryOneExists, runValidator,
                                upcastEffERoComp, usingSomeTx)
import           Snowdrop.Execution (ExpandRawTxsMode, ExpandableTx, ProofNExp (..),
                                     UnionSeqExpandersInps, expandUnionRawTxs)
import           Snowdrop.Util

data TipMap blkType
data BlundMap blkType (txtypes :: [*]) (c :: * -> Constraint)
type instance HKeyVal (TipMap blkType) = '(TipKey, TipValue (BlockRef blkType))
type instance HKeyVal (BlundMap blkType (txtypes :: [*]) c) =
      '(BlockRef blkType, Blund (BlockHeader blkType) [SomeTx c] (BlockUndo blkType))

data BlockStateException
    = TipNotFound
    deriving (Show)

instance Buildable BlockStateException where
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

class ( RContains txtypes txtype
      , HUpCastableChSet (TxComponents txtype) xs
      , UpCastableERo (TxComponents txtype) xs
      ) => BlkProcConstr txtypes xs txtype
instance ( RContains txtypes txtype
         , HUpCastableChSet (TxComponents txtype) xs
         , UpCastableERo (TxComponents txtype) xs
         ) => BlkProcConstr txtypes xs txtype

class ExpandableTx txtypes (BlkProcConstr txtypes xs) txtype => BlkProcessingMode txtypes xs txtype
instance ExpandableTx txtypes (BlkProcConstr txtypes xs) txtype => BlkProcessingMode txtypes xs txtype

type BlkProcComponents blkType (txtypes :: [*]) (c :: * -> Constraint)
    = UnionTypes [TipMap blkType, BlundMap blkType txtypes c]
                 (TxComponents (UnitedTxType txtypes))

-- | An implementation of `BlkStateConfiguration` on top of `ERwComp`.
-- It uniformly accesses state and block storage (via `DataAccess` interface).
inmemoryBlkStateConfiguration
  :: forall blkType rawTx txtypes e ctx xs c .
    ( xs ~ BlkProcComponents blkType txtypes c
    , c ~ BlkProcessingMode txtypes xs
    , HasExceptions e
        [ StatePException
        , BlockStateException
        , StateModificationException
        , CSMappendException
        ]
    , BlockUndo blkType ~ Undo xs
    , RawPayload blkType ~ [SomeTx c] -- whaat??
    , Ord (BlockHeader blkType)
    , HasLens (ChgAccum ctx) (ChgAccum ctx)
    , HasLens ctx (ChgAccumCtx ctx)
    , HasGetter (RawBlk blkType) [rawTx]
    , HasGetter (Payload blkType) [SomeTx (BlkProcessingMode txtypes xs)]
    , Default (ChgAccum ctx)
    , ExpandRawTxsMode e ctx txtypes
    , MappendHChSet xs

    , QueryERo xs (TipMap blkType)
    , QueryERo xs (BlundMap blkType txtypes c)
    , HUpCastableChSet '[TipMap blkType] xs
    , HUpCastableChSet '[BlundMap blkType txtypes c] xs
    , UpCastableERo (UnionSeqExpandersInps txtypes) xs
    , Default (HChangeSet xs)
    , Default (AvlRevisions xs)
    )
    => BlkConfiguration blkType
    -> Validator e ctx txtypes
    -> (rawTx -> SomeData (ProofNExp e ctx rawTx) (ExpandableTx txtypes (BlkProcConstr txtypes xs)))
    -> (RawBlk blkType -> [SomeTx (BlkProcConstr txtypes xs)] -> Block (BlockHeader blkType) (Payload blkType))
    -> BlkStateConfiguration blkType (ERwComp e ctx (ChgAccum ctx) xs)
inmemoryBlkStateConfiguration cfg validator mkProof mkBlock = fix $ \this ->
    BlkStateConfiguration {
      bscConfig = cfg
    , bscExpand = \rawBlock -> do
          blkPayload <- liftERoComp $ upcastEffERoComp $ expandUnionRawTxs mkProof (gett rawBlock)
          pure (mkBlock rawBlock blkPayload)
    , bscApplyPayload = \(gett @_ @[SomeTx (BlkProcessingMode txtypes xs)] -> txs) -> do
          undos <- forM txs $ \smtx -> usingSomeTx smtx $ \tx -> do
                      liftERoComp $ upcastEffERoComp $ runValidator validator tx
                      applySomeTx (modifyRwCompChgAccum . CAMChange . hupcast . txBody) smtx
          let mergeUndos (Undo cs1 _) (Undo cs2 sn2) = flip Undo sn2 <$> mappendChangeSet cs1 cs2
          case reverse undos of
              []     -> pure $ Undo def def
              f:rest -> either throwLocalError pure $ foldM mergeUndos f rest
    , bscApplyUndo = void . modifyRwCompChgAccum . CAMRevert
    , bscStoreBlund = \blund -> do
          let blockRef = unCurrentBlockRef $ bcBlockRef cfg (blkHeader $ buBlock blund)
          let chg = hChangeSetFromMap @(BlundMap blkType txtypes c) $ M.singleton blockRef (New blund)
          void $ modifyRwCompChgAccum $ CAMChange $ hupcast chg
    , bscRemoveBlund = \blockRef -> do
          let chg = hChangeSetFromMap @(BlundMap blkType txtypes c) $ M.singleton blockRef Rem
          void $ modifyRwCompChgAccum $ CAMRevert $ Undo (hupcast chg) def
    , bscGetBlund = liftERoComp . queryOne @(BlundMap blkType txtypes c)
    , bscBlockExists = liftERoComp . queryOneExists @(BlundMap blkType txtypes c)
    , bscGetTip = do
          tipMb <- liftERoComp (queryOne @(TipMap blkType) TipKey)
          maybe (throwLocalError @BlockStateException TipNotFound) (pure . unTipValue) tipMb
    , bscSetTip = \newTip' -> do
          void $ bscGetTip this -- to check existence
          let chg = hChangeSetFromMap @(TipMap blkType) $ M.singleton TipKey (Upd (TipValue newTip'))
          -- TODO check that tip corresponds to blund storage
          void $ modifyRwCompChgAccum $ CAMChange $ hupcast chg
    }
