{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Block.Exec.StateConfiguration
       ( inmemoryBlkStateConfiguration
       , BlkProcConstr
       ) where

import           Universum

import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import           Data.Union (USubset, Union, ulift, urelax)
import           Data.Vinyl.TypeLevel (AllConstrained, RImage)

import           Snowdrop.Block (BlkConfiguration (..), BlkStateConfiguration (..), BlockHeader,
                                 BlockRef, ForkVerificationException, FromRef (..),
                                 IterationException, RawBlk, ToRef (..), loadBlocksFromTo,
                                 unCurrentBlockRef)
import           Snowdrop.Block.Exec.RawTx (CloseBlockRawTx (..), CloseBlockRawTxType,
                                            OpenBlockRawTx (..), OpenBlockRawTxType)
import           Snowdrop.Block.Exec.Storage (Blund (..), BlundComponent, TipComponent, TipKey (..),
                                              TipValue (..))
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx, Ctx, ERoCompU,
                                ExpandableTx, HasBExceptions, QueryERo, SeqExpanders, TxComponents,
                                TxRaw (..), Undo, UnionSeqExpandersInps, UnionSeqExpandersOuts,
                                UpCastableERoM, convertEffect, modifyAccumUndo, queryOne,
                                queryOneExists, runSeqExpandersSequentially, upcastEffERoCompM,
                                withAccum)
import           Snowdrop.Hetero (Both, NotIntersect, RContains, SomeData, UnionTypes)
import           Snowdrop.Util (HasGetter (..), HasLens (..), HasReview (..), OldestFirst (..))

class ( RContains txtypes txtype
      , UpCastableERoM (TxComponents txtype) xs
      ) => BlkProcConstr txtypes xs txtype
instance ( RContains txtypes txtype
         , UpCastableERoM (TxComponents txtype) xs
         ) => BlkProcConstr txtypes xs txtype

type BlkProcComponents blkType blkUndo (txtypes :: [*])
    = UnionTypes [TipComponent blkType, BlundComponent blkType blkUndo]
                 (UnionSeqExpandersOuts txtypes)

-- | An implementation of `BlkStateConfiguration` on top of `ERwComp`.
-- It uniformly accesses state and block storage (via `DataAccess` interface).
inmemoryBlkStateConfiguration
  :: forall blkType blkUndo txtypes conf openExpRestr closeExpRestr xs c openBlockTxType closeBlockTxType txtypes' .
    ( xs ~ BlkProcComponents blkType blkUndo txtypes'
    , c ~ Both (ExpandableTx txtypes') (BlkProcConstr txtypes' xs)
    , openBlockTxType ~ OpenBlockRawTxType (BlockHeader blkType) openExpRestr
    , closeBlockTxType ~ CloseBlockRawTxType (BlockHeader blkType) closeExpRestr
    , txtypes' ~ (openBlockTxType ': closeBlockTxType ': txtypes)
    , HasBExceptions conf '[ CSMappendException
                           , ForkVerificationException (BlockRef blkType)
                           , IterationException (BlockRef blkType)
                           ]
    , NotIntersect '[ openBlockTxType, closeBlockTxType ] txtypes
    , HasReview (Undo conf) blkUndo
    , HasGetter (RawBlk blkType) (BlockHeader blkType)
    , Element (RawBlk blkType) ~ Union TxRaw txtypes
    , Container (RawBlk blkType)
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , QueryERo xs (BlundComponent blkType blkUndo)
    , QueryERo xs (TipComponent blkType)
    , UpCastableERoM (UnionSeqExpandersInps txtypes') xs
    , AllConstrained c txtypes'
    , HasGetter (Union TxRaw txtypes) (SomeData TxRaw c)
    , USubset txtypes txtypes' (RImage txtypes txtypes')
    )
    => BlkConfiguration blkType
    -> SeqExpanders conf txtypes'
    -> BlkStateConfiguration (ChgAccum conf) blkType (ERoCompU conf xs)
inmemoryBlkStateConfiguration cfg  expander = fix $ \this ->
    BlkStateConfiguration {
      bscVerifyConfig = cfg
    , bscExpandHeaders = \_ rawBlocks -> pure $ gett <$> rawBlocks
    , bscValidatePayloads = \acc0 rawBlks -> do
        let rawTxs = toRawTxs <$> rawBlks
            lengths = unOldestFirst $ length <$> rawTxs
            indicies = cummulativeSums lengths
            flatRawTxs = gett @_ @(SomeData TxRaw c) <$> mconcat (toList $ unOldestFirst rawTxs)
        OldestFirst txsWithAccs <-
            convertEffect $
              upcastEffERoCompM @_ @xs @conf $
              withAccum acc0 $
              runSeqExpandersSequentially expander flatRawTxs
        let accs = acc0 : (snd <$> txsWithAccs)
        pure $ OldestFirst $ pickElements (NE.zip lengths indicies) accs
    , bscGetHeader = \blockRef -> convertEffect $ do
        blund <- queryOne @(BlundComponent blkType blkUndo) @xs @conf $ blockRef
        pure $ gett . buRawBlk <$> blund
    --  sequentially rollback every block from the tip down to blockRef
    , bscInmemRollback = \acc0 mBlockRef -> do
        mTip <- bscGetTip this
        blunds <- getBlunds mBlockRef mTip
        let refs = unCurrentBlockRef . bcBlockRef cfg . gett . buRawBlk <$> blunds
        fmap (, refs) $ withAccum acc0 $
          modifyAccumUndo @xs @conf $ inj . buUndo <$> blunds
    , bscBlockExists = convertEffect . queryOneExists @(BlundComponent blkType blkUndo) @xs @conf
    , bscGetTip = unTipValue <<$>> convertEffect (queryOne @(TipComponent blkType) @xs @conf TipKey)
    }
    where
      getBlunds fromRef toRef =
          loadBlocksFromTo
              (convertEffect . queryOne @(BlundComponent blkType blkUndo) @xs @conf)
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
          [ulift $ TxRaw @openBlockTxType $ OpenBlockRawTx header]
            ++ (map urelax rawTxs)
            ++ [ulift $ TxRaw @closeBlockTxType $ CloseBlockRawTx header]
        where
          rawTxs = toList rawBlk
          header = gett @_ @(BlockHeader blkType) rawBlk
