{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE Rank2Types #-}

module Snowdrop.Execution.Mempool.Logic
       ( evictMempool
       , processTxAndInsertToMempool
       , createBlockDbModifyAction
       , normalizeMempool
       , Rejected (..)
       ) where

import           Universum

import           Control.Lens ((%=))
import           Control.Monad.Except (catchError)
import           Data.Default (Default (..))

import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx, IdSumPrefixed (..),
                                StatePException, Undo, liftERoComp)
import           Snowdrop.Execution.DbActions (DbModifyActions (..))
import           Snowdrop.Execution.IOExecutor (IOCtx)
import           Snowdrop.Execution.Mempool.Core (Mempool, MempoolConfig (..), MempoolState (..),
                                                  RwActionWithMempool, actionWithMempool, msTxsL)
import           Snowdrop.Util

---------------------------
-- Logic
---------------------------

evictMempool :: Default (ChgAccum ctx) => RwActionWithMempool e id value rawtx ctx [(rawtx, Undo id value)]
evictMempool = gets msTxs <* put def

processTxAndInsertToMempool
    :: ( HasException e StatePException, Show e, Typeable e
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => MempoolConfig e id proof value ctx rawtx
    -> rawtx
    -> RwActionWithMempool e id value rawtx ctx ()
processTxAndInsertToMempool MempoolConfig{..} rawtx = do
    tx <- liftERoComp (mcExpandTx rawtx)
    undo <- mcProcessTx tx
    msTxsL %= (++ [(rawtx, undo)])

newtype Rejected rawtx = Rejected {getRejected :: [rawtx]}

normalizeMempool
    :: ( HasException e StatePException, Show e, Typeable e
       , Default (ChgAccum ctx)
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => MempoolConfig e id proof value ctx rawtx
    -> RwActionWithMempool e id value rawtx ctx (Rejected rawtx)
normalizeMempool MempoolConfig{..} = do
    txs <- map fst <$> evictMempool
    (rejected, _) <- processAll txs
    pure rejected
  where
    processOne rawtx = do
        tx <- liftERoComp (mcExpandTx rawtx)
        (rawtx,) <$> mcProcessTx tx

    processAll txs = fmap (first Rejected . partitionEithers) $ do
        forM txs $ \rawtx ->
            (Right <$> processOne rawtx)
              `catchError` (\_ -> pure $ Left rawtx)

---------------------------------
-- Helpers (work in MonadIO)
---------------------------------

createBlockDbModifyAction
    :: ( HasExceptions e [CSMappendException id, StatePException], Show e, Typeable e
       , Default chgAccum
       , IdSumPrefixed id, Ord id
       )
    => MempoolConfig e id proof value (IOCtx chgAccum id value) rawtx
    -> Mempool id value chgAccum rawtx
    -> DbModifyActions chgAccum id value ExecM a
    -> DbModifyActions chgAccum id value ExecM (a, Rejected rawtx)
createBlockDbModifyAction cfg mem dbM = DbModifyActions (dmaAccessActions dbM) $ \chg -> do
    res <- dmaApply dbM chg
    (res,) <$> actionWithMempool mem (dmaAccessActions dbM) (normalizeMempool cfg)
