{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Block.State
       ( BlockStateException (..)
       , TipKey (..)
       , BlockRef (..)
       , TipValue (..)
       , inmemoryBlkStateConfiguration
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.Default (def)
import qualified Data.Map as M
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))
import           Snowdrop.Block.Types (Block (..), Blund (..), CurrentBlockRef (..))
import           Snowdrop.Core (CSMappendException (..), ChangeSet (..), ChgAccum, ChgAccumCtx,
                                ChgAccumModifier (..), ERwComp, HasKeyValue, IdSumPrefixed (..),
                                SomeTx, StateModificationException, StatePException (..),
                                StateTx (..), Undo (..), Validator, ValueOp (..), applySomeTx,
                                mappendChangeSet, modifyRwCompChgAccum, queryOne, queryOneExists,
                                runValidator, liftERoComp')
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

newtype BlockRef blockRef = BlockRef {unBlockRef :: blockRef}
    deriving (Eq, Ord, Show)

instance Buildable blockRef => Buildable (BlockRef blockRef) where
    build (BlockRef br) = bprint ("block ref "%build) br

data TipValue blockRef = TipValue {unTipValue :: Maybe blockRef}
    deriving (Eq, Ord, Show, Generic)

-- FIXME: I don't sure about this approach because it complicates usage of
-- inmemoryBlkStateConfiguration function (we need to tonvert that
-- WithConstraint part back and forth). The problems here is that we want to
-- attach a constraint to BlkStateConfiguration like "Hey, I work only with
-- transactions from that given set". Alternative way is to attach that
-- particular constraint to BlkStateConfiguration, BlkConfiguration,
-- BlockIntegrityVerifier and Block data types. It will simplify composition in
-- cases when we use the same set of transactions throught our application. We
-- also can make constraint configurable making it possible to use completely
-- different payload inside of a Block.

-- | An implementation of `BlkStateConfiguration` on top of `ERwComp`.
-- It uniformly accesses state and block storage (via `DataAccess` interface).
inmemoryBlkStateConfiguration
  :: forall header rawPayload blockRef e id txtypes value ctx payload .
    ( HasKeyValue id value TipKey (TipValue blockRef)
    , HasKeyValue id value (BlockRef blockRef) (Blund header rawPayload (Undo id value))
    , HasExceptions e
        [ StatePException
        , BlockStateException id
        , StateModificationException id
        , CSMappendException id
        ]
    , Ord id
    , Ord (BlockRef blockRef)
    , IdSumPrefixed id
    , HasLens ctx (ChgAccumCtx ctx)
    )
    => BlkConfiguration header (WithConstraint (HasGetterOf [SomeTx id value (RContains txtypes)]) payload) blockRef
    -> Validator e id value ctx txtypes
    -> BlkStateConfiguration header (WithConstraint (HasGetterOf [SomeTx id value (RContains txtypes)]) payload) rawPayload (Undo id value) blockRef (ERwComp e id value ctx (ChgAccum ctx))
inmemoryBlkStateConfiguration cfg validator =
    BlkStateConfiguration {
      bscConfig = cfg
    , bscApplyPayload = \txsC -> do
          undos <- do
              applyWithConstraintFlip txsC $ \txs ->
                  forM (gettOf txs :: [SomeTx id value (RContains txtypes)]) $ \tx -> do
                      liftERoComp' $ applySomeTx (runValidator validator) tx
                      modifyRwCompChgAccum (CAMChange $ applySomeTx txBody tx)
          let mergeUndos (Undo cs1 sn1) (Undo cs2 _) = flip Undo sn1 <$> mappendChangeSet cs1 cs2
          case undos of
            []     -> pure $ Undo def BS.empty
            f:rest -> either throwLocalError pure $ foldM mergeUndos f rest
    , bscApplyUndo = void . modifyRwCompChgAccum . CAMRevert
    , bscStoreBlund = \blund -> do
          let blockRef = unCurrentBlockRef $ bcBlockRef cfg (blkHeader $ buBlock blund)
          let chg = ChangeSet $ M.singleton (inj $ BlockRef blockRef) (New $ inj blund)
          void $ modifyRwCompChgAccum $ CAMChange chg
    , bscRemoveBlund = \blockRef ->
        void $ modifyRwCompChgAccum $ CAMRevert $ Undo (ChangeSet $ M.singleton (inj $ BlockRef blockRef) Rem) BS.empty
    , bscGetBlund = liftERoComp' . queryOne . BlockRef
    , bscBlockExists = liftERoComp' . queryOneExists . BlockRef
    , bscGetTip = liftERoComp' (queryOne TipKey)
                    >>= maybe (throwLocalError @(BlockStateException id) TipNotFound) (pure . unTipValue)
    , bscSetTip = \newTip' -> do
          let newTip = inj $ TipValue newTip'
          let tipChg = \cons -> ChangeSet $ M.singleton (inj TipKey) (cons newTip)
          oldTipMb <- liftERoComp' $ queryOne TipKey
          -- TODO check that tip corresponds to blund storage
          void . modifyRwCompChgAccum . CAMChange . tipChg $ maybe New (const Upd) oldTipMb
    }
