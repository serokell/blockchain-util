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
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import           Data.Union (USubset, Union, urelax)
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.TypeLevel (AllConstrained, RImage)

import           Snowdrop.Block (BlkConfiguration (..), BlkStateConfiguration (..), BlockHeader,
                                 BlockRef, ForkVerificationException, FromRef (..),
                                 IterationException, RawBlk, ToRef (..), loadBlocksFromTo,
                                 unCurrentBlockRef)
import           Snowdrop.Block.Exec.RawTx (CloseBlockRawTx (..), CloseBlockRawTxType,
                                            OpenBlockRawTx (..), OpenBlockRawTxType)
import           Snowdrop.Block.Exec.Storage (BlockUndo, Blund (..), BlundComponent, TipComponent,
                                              TipKey (..), TipValue (..))
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..), Ctx, ERoComp,
                                ERoCompU, ExpandableTx, HChangeSet, HasBExceptions, MappendHChSet,
                                ProofNExp (..), QueryERo, SomeTx, StateTx (..), TxComponents, TxRaw,
                                Undo, UnionSeqExpandersInps, UnitedTxType, UpCastableERoM,
                                Validator, applySomeTx, convertEffect, modifyAccumUndo, queryOne,
                                queryOneExists, runSeqExpandersSequentially, runValidator,
                                upcastEffERoComp, upcastEffERoCompM, withAccum)
import           Snowdrop.Hetero (Both, NotElem, RContains, SomeData, UnionTypes)
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
  :: forall blkType txtypes conf xs c openBlockTxType closeBlockTxType txtypes' .
    ( xs ~ BlkProcComponents blkType txtypes'
    , c ~ Both (ExpandableTx txtypes') (BlkProcConstr txtypes' xs)
    , openBlockTxType ~ OpenBlockRawTxType (BlockHeader blkType)
    , closeBlockTxType ~ CloseBlockRawTxType (BlockHeader blkType)
    , txtypes' ~ (openBlockTxType ': closeBlockTxType ': txtypes)
    , HasBExceptions conf '[ CSMappendException
                           , ForkVerificationException (BlockRef blkType)
                           , IterationException (BlockRef blkType)
                           ]
    , NotElem openBlockTxType txtypes'
    , NotElem closeBlockTxType txtypes'
    , Default (ChgAccum conf)
    , HasReview (Undo conf) (BlockUndo blkType)
    , HasGetter (RawBlk blkType) (BlockHeader blkType)
    , HasGetter (RawBlk blkType) [Union TxRaw txtypes]
    , HasLens (ChgAccum conf) (ChgAccum conf)
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , QueryERo xs (BlundComponent blkType)
    , QueryERo xs (TipComponent blkType)
    , MappendHChSet (UnionSeqExpandersInps txtypes')
    , UpCastableERoM (UnionSeqExpandersInps txtypes') xs
    , Default (HChangeSet (UnionSeqExpandersInps txtypes'))
    , AllConstrained c txtypes'
    , HasGetter (Union TxRaw txtypes) (SomeData TxRaw c)
    , USubset txtypes txtypes' (RImage txtypes txtypes')
    )
    => BlkConfiguration blkType
    -> Validator conf txtypes'
    -> Rec (ProofNExp conf) txtypes'
    -> BlkStateConfiguration (ChgAccum conf) blkType (ERoCompU conf xs)
inmemoryBlkStateConfiguration cfg validator expander = fix $ \this ->
    BlkStateConfiguration {
      bscVerifyConfig = cfg
    , bscExpandHeaders = \_ rawBlocks -> pure $ gett <$> rawBlocks
    , bscValidatePayloads = \acc0 rawBlks -> do
        let rawTxs = toRawTxs <$> rawBlks
            lengths = unOldestFirst $ length <$> rawTxs
            indicies = cummulativeSums lengths
            flatRawTxs = gett @_ @(SomeData TxRaw c) <$> mconcat (toList $ unOldestFirst rawTxs)
        OldestFirst txsWithAccs <-
            convertEffect @conf $
              upcastEffERoCompM @_ @xs $
              withAccum @conf acc0 $
              runSeqExpandersSequentially expander flatRawTxs
        let txsWithPreAccs = zip accs (fst <$> txsWithAccs)
            accs = acc0 : (snd <$> txsWithAccs)
        convertEffect @conf $ mapM_ (mapTx_ @conf (runValidator validator)) txsWithPreAccs
        pure $ OldestFirst $ pickElements (NE.zip lengths indicies) accs
    , bscGetHeader = \blockRef -> convertEffect @conf $ do
        blund <- queryOne @(BlundComponent blkType) @xs @conf $ blockRef
        pure $ gett . buRawBlk <$> blund
    --  sequentially rollback every block from the tip down to blockRef
    , bscInmemRollback = \acc0 mBlockRef -> do
        mTip <- bscGetTip this
        blunds <- getBlunds mBlockRef mTip
        let refs = unCurrentBlockRef . bcBlockRef cfg . gett . buRawBlk <$> blunds
        fmap (, refs) $ withAccum @conf acc0 $
          modifyAccumUndo @xs @conf $ inj . buUndo <$> blunds
    , bscBlockExists = convertEffect @conf . queryOneExists @(BlundComponent blkType) @xs @conf
    , bscGetTip = unTipValue <<$>> convertEffect @conf (queryOne @(TipComponent blkType) @xs @conf TipKey)
    }
    where
      getBlunds fromRef toRef =
          loadBlocksFromTo
              (convertEffect @conf . queryOne @(BlundComponent blkType) @xs @conf)
              (bcPrevBlockRef cfg . gett @(RawBlk blkType) . buRawBlk)
              (bcMaxForkDepth cfg)
              (Just $ FromRef fromRef)
              (ToRef toRef)

      drop' (toDrop, i) chgAccs =
        case drop toDrop chgAccs of
          ca : caRest -> (ca, ca : caRest)
          _ -> error $
                  "inmemoryBlkStateConfiguration:"
                    <> " runSeqExpandersSequentially returned list"
                    <> " with no chgAccum for tx #" <> show i

      pickElements :: NonEmpty (Int, Int) -> [ChgAccum conf] -> NonEmpty (ChgAccum conf)
      pickElements (i0 :| iRest) chgAccs = fstChgAcc :| pickElementsDo iRest chgAccsRest
        where
          (fstChgAcc, chgAccsRest) = drop' i0 chgAccs

      pickElementsDo :: [(Int, Int)] -> [ChgAccum conf] -> [ChgAccum conf]
      pickElementsDo (i : iRest) chgAccs = chgAcc : pickElementsDo iRest chgAccsRest
        where
          (chgAcc, chgAccsRest) = drop' i chgAccs
      pickElementsDo [] _ = []

      -- | Functions transforms list of ints to list of cummulative sums
      -- E.g. cummulativeSums [0, 1, 0, 7, 4] == [0, 1, 1, 8, 12]
      cummulativeSums :: NonEmpty Int -> NonEmpty Int
      cummulativeSums (f :| rest) = NE.reverse $ snd $
          foldl' (\(s, ls) i -> (s + i, s + i <| ls)) (f, f :| []) rest

      toRawTxs :: RawBlk blkType -> [Union TxRaw txtypes']
      toRawTxs rawBlk =
          [inj $ OpenBlockRawTx header]
            ++ (map urelax rawTxs)
            ++ [inj $ CloseBlockRawTx header]
        where
          rawTxs = gett @_ @[Union TxRaw txtypes] rawBlk
          header = gett @_ @(BlockHeader blkType) rawBlk
