{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Block.Exec.StateConfiguration
       ( inmemoryBlkStateConfiguration
       , BlkProcConstr
       ) where

import           Universum

import           Data.Default (Default)
import qualified Data.Map as M
import           Data.Vinyl (Rec)

import           Snowdrop.Block (BlkConfiguration (..), BlkStateConfiguration (..), BlockExpandedTx,
                                 BlockRawTx, BlockUndo, Blund (..), CurrentBlockRef (..))
import           Snowdrop.Block.Exec.RawTx ()
import           Snowdrop.Block.Exec.Storage (BlundComponent, TipComponent, TipKey (..),
                                              TipValue (..))
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..), Ctx, DbAccessU,
                                ERoComp, ERwComp, ExpandRawTxsMode, ExpandableTx, HChangeSet,
                                HUpCastableChSet, HasBExceptions, ProofNExp (..),
                                QueryERo, SomeTx, StatePException (..), StateTx (..), TxComponents,
                                TxRaw (..), Undo, UnionSeqExpandersInps, UnitedTxType,
                                UpCastableERoM, Validator, ValueOp (..), applySomeTx, computeUndo,
                                convertEffect, getCAOrDefault, hChangeSetFromMap, liftERoComp,
                                modifyAccum, modifyAccumOne, modifyAccumUndo, queryOne,
                                queryOneExists, runSeqExpandersSequentially, runValidator,
                                upcastEffERoComp, upcastEffERoCompM)
import           Snowdrop.Hetero (Both, ExnHKeyConstr, RContains, SomeData, UnionTypes, hupcast)
import           Snowdrop.Util (HasGetter (..), HasLens (..), HasReview (..), OldestFirst (..))

instance Hashable bRef => Hashable (TipValue bRef)

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
    , ExnHKeyConstr xs
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
