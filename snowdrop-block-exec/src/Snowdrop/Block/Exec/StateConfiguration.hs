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
import qualified Data.Text.Buildable
import           Data.Vinyl (Rec)

import           Snowdrop.Block (BlkConfiguration (..), BlkStateConfiguration (..), BlockExpandedTx,
                                 BlockRawTx, BlockUndo, Blund (..), CurrentBlockRef (..))
import           Snowdrop.Block.Exec.RawTx ()
import           Snowdrop.Block.Exec.Storage (BlundComponent, TipComponent, TipKey (..),
                                              TipValue (..))
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..), Ctx, DbAccessU,
                                ERoComp, ERwComp, ExpandableTx, HChangeSet, HasBExceptions,
                                MappendHChSet, ProofNExp (..), QueryERo, SomeTx,
                                StatePException (..), StateTx (..), SumChangeSet, TxComponents,
                                TxRaw, UnionSeqExpandersInps, UnitedTxType, UpCastableERoM,
                                Validator, applySomeTx, convertEffect, liftERoComp, modifyAccumOne,
                                queryOne, queryOneExists, runSeqExpandersSequentially, runValidator,
                                upcastEffERoComp, upcastEffERoCompM)
import           Snowdrop.Hetero (Both, RContains, SomeData, UnionTypes, hupcast)
import           Snowdrop.Util (HasGetter (..), HasLens (..), HasReview (..), OldestFirst (..))

class ( RContains txtypes txtype
      , UpCastableERoM (TxComponents txtype) xs
      ) => BlkProcConstr txtypes xs txtype
instance ( RContains txtypes txtype
         , UpCastableERoM (TxComponents txtype) xs
         ) => BlkProcConstr txtypes xs txtype

type BlkProcComponents blkType (txtypes :: [*])
    = UnionTypes [TipComponent blkType, BlundComponent blkType]
                 (TxComponents (UnitedTxType txtypes))

mapTx_
  :: forall conf xs txtypes c .
    ( HasLens (Ctx conf) (ChgAccumCtx conf)
    , c ~ BlkProcConstr txtypes xs
    )
  => (forall txtype . c txtype => StateTx txtype -> ERoComp conf (TxComponents txtype) ())
  -> (ChgAccum conf, SomeTx c)
  -> ERoComp conf xs ()
mapTx_ handleTxDo (acc, tx) = local (lensFor .~ CAInitialized @conf acc) $ handleTx tx
  where
    handleTx =
      applySomeTx $ \(stx :: StateTx txtype) ->
          upcastEffERoComp @(TxComponents txtype) @xs @conf (handleTxDo stx)

-- | An implementation of `BlkStateConfiguration` on top of `ERwComp`.
-- It uniformly accesses state and block storage (via `DataAccess` interface).
inmemoryBlkStateConfiguration
  :: forall blkType txtypes conf xs c m .
    ( xs ~ BlkProcComponents blkType txtypes
    , c ~ BlkProcConstr txtypes xs
    , m ~ ERwComp conf (DbAccessU conf xs) (ChgAccum conf)
    , Tx blkType ~ SomeTx (BlkProcConstr txtypes xs)
    , HasBExceptions conf
        [ StatePException
        , CSMappendException
        ]
    , Default (ChgAccum conf)
    , Default (SumChangeSet (UnionSeqExpandersInps txtypes))
    , Default (HChangeSet (UnionSeqExpandersInps txtypes))
    , HasGetter (BlockUndo blkType) (HChangeSet xs)
    , HasGetter (RawBlk blkType) (BlockHeader blkType)
    , HasGetter (RawBlk blkType) [SomeData TxRaw (Both (ExpandableTx txtypes) c)]
    , HasLens (ChgAccum conf) (ChgAccum conf)
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , MappendHChSet (UnionSeqExpandersInps txtypes)
    , QueryERo xs (BlundComponent blkType)
    , QueryERo xs (TipComponent blkType)
    , UpCastableERoM (UnionSeqExpandersInps txtypes) xs
    )
    => BlkConfiguration blkType
    -> Validator conf txtypes
    -> Rec (ProofNExp conf) txtypes
    -> BlkStateConfiguration (ChgAccum conf) blkType m
inmemoryBlkStateConfiguration cfg validator exps =
    BlkStateConfiguration {
      bscVerifyConfig = cfg
    , bscExpandHeaders = \_ rawBlocks -> do
      pure $ OldestFirst $ gett <$> rawBlocks
    , bscExpandPayloads = \chgAccum rawBlks -> let
        comp = OldestFirst <$> traverse (runSeqExpandersSequentially exps . gett) rawBlks
        in liftERoComp $ upcastEffERoCompM @_ @xs $ local (lensFor @(Ctx conf) @(ChgAccumCtx conf) .~ CAInitialized @conf chgAccum) comp
    , bscValidateTx = \chgAccum tx -> liftERoComp $ mapTx_ @conf (runValidator validator) (chgAccum, tx)
    , bscGetHeader = \blockRef -> liftERoComp $ do
        blund <- queryOne @(BlundComponent blkType) @xs @conf $ blockRef
        pure $ blkHeader. buBlock <$> blund
    , bscInmemRollback = \blockRef -> liftERoComp $ do
        blockUndo <- convertEffect @conf $ queryOne @(BlundComponent blkType) @xs @conf blockRef >>= maybe (throwLocalError BlockRefNotFound) (pure . buUndo)
        modifyAccumOne @_ @conf $ gett @_ @(HChangeSet xs) blockUndo
     , bscBlockExists = liftERoComp . queryOneExists @(BlundComponent blkType) @xs @conf
     , bscGetTip = unTipValue <<$>> liftERoComp (queryOne @(TipComponent blkType) @xs @conf TipKey)
    }
