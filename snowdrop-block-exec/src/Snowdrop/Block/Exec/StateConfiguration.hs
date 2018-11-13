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

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), BlockHeader, BlockRef, BlockUndo, Blund (..),
                                       PrevBlockRef (..), RawBlk, RawBlund, RawPayload, Tx)
import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx (..), Ctx, DbAccessU,
                                ERoComp, ERwComp, HChangeSet, HasBExceptions, MappendHChSet,
                                QueryERo, SomeTx, StatePException (..), StateTx (..), TxComponents,
                                TxRaw, Undo, UnitedTxType, UpCastableERoM, Validator, applySomeTx,
                                liftERoComp, modifyAccumUndo, queryOne, queryOneExists,
                                runValidator, upcastEffERoComp, upcastEffERoCompM)
import           Snowdrop.Execution (ExpandableTx, ProofNExp (..), UnionSeqExpandersInps,
                                     runSeqExpandersSequentially)
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
    , Default (HChangeSet (UnionSeqExpandersInps txtypes))
    , HasGetter (BlockUndo blkType) (Undo conf)
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
inmemoryBlkStateConfiguration cfg validator exps = fix $ \this ->
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
    , bscInmemRollback = \blockRef -> do
          blunds <- bscGetTip this >>= maybe (throwLocalError BlockRefNotFound) (liftERoComp . getBlunds blockRef)
          liftERoComp $ modifyAccumUndo @xs @conf $ NewestFirst (gett . buUndo <$> blunds)
    , bscBlockExists = liftERoComp . queryOneExists @(BlundComponent blkType) @xs @conf
    , bscGetTip = unTipValue <<$>> liftERoComp (queryOne @(TipComponent blkType) @xs @conf TipKey)
    }
    where
      getBlunds :: BlockRef blkType -> BlockRef blkType -> ERoComp conf xs [RawBlund blkType]
      getBlunds to from
        | to == from = pure []
        | otherwise = do
            blundM <- queryOne @(BlundComponent blkType) @xs @conf from
            (blund, prev) <- getPrev blundM
            liftA2 (:) (pure blund) (getBlunds to prev)
            where
              getPrev blundM = maybe (throwLocalError BlockRefNotFound) pure $ do
                blund <- blundM
                prev <- unPrevBlockRef . bcPrevBlockRef cfg $ blkHeader . buBlock $ blund
                pure (blund, prev)
