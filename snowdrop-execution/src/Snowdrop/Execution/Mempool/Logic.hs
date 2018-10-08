{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Execution.Mempool.Logic
       ( evictMempool
       , processTxAndInsertToMempool
       , normalizeMempool
       , Rejected (..)

       , createBlockDbModifyAction
       ) where

import           Universum

import           Control.Lens ((%=))
import           Control.Monad.Except (catchError)
import           Data.Default (Default (..))

import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx, SomeTxWithUndo,
                                StatePException, upcastEffERwComp)
import           Snowdrop.Execution.DbActions (DbModifyActions (..))
import           Snowdrop.Execution.IOExecutor (IOCtx)
import           Snowdrop.Execution.Mempool.Core (Mempool, MempoolConfig (..), MempoolState (..),
                                                  MempoolTx, RwMempoolAct, StateTxHandler (..),
                                                  actionWithMempool, msTxsL)
import           Snowdrop.Util

---------------------------
-- Logic
---------------------------

evictMempool
    :: Default (ChgAccum ctx)
    => RwMempoolAct e ctx c xs rawtx [(rawtx, SomeTxWithUndo c)]
evictMempool = gets msTxs <* put def

processTxAndInsertToMempool
    :: ( HasException e StatePException, Show e, Typeable e
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => MempoolConfig e ctx (Both (MempoolTx txtypes xs) c) rawtx
    -> rawtx
    -> RwMempoolAct e ctx (Both (MempoolTx txtypes xs) c) xs rawtx ()
processTxAndInsertToMempool MempoolConfig{..} rawtx = usingSomeData (mcProcessTx rawtx) $
  \(StateTxHandler handler) -> do
      txNundo <- upcastEffERwComp handler
      msTxsL %= (++ [(rawtx, SomeData txNundo)])

newtype Rejected rawtx = Rejected { getRejected :: [rawtx] }

normalizeMempool
    :: forall e ctx rawtx txtypes xs c .
       ( HasException e StatePException, Show e, Typeable e
       , Default (ChgAccum ctx)
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => MempoolConfig e ctx (Both (MempoolTx txtypes xs) c) rawtx
    -> RwMempoolAct e ctx (Both (MempoolTx txtypes xs) c) xs rawtx (Rejected rawtx)
normalizeMempool MempoolConfig{..} = do
    txs <- map fst <$> evictMempool
    (rejected, _) <- processAll txs
    pure rejected
  where
    processOne :: rawtx -> RwMempoolAct e ctx (Both (MempoolTx txtypes xs) c) xs rawtx ()
    processOne rawtx = usingSomeData (mcProcessTx rawtx) $ \(StateTxHandler handler) ->
        void (upcastEffERwComp handler)

    processAll :: [rawtx] -> RwMempoolAct e ctx (Both (MempoolTx txtypes xs) c) xs rawtx (Rejected rawtx, [()])
    processAll txs = fmap (first Rejected . partitionEithers) $ do
        forM txs $ \rawtx ->
            (Right <$> processOne rawtx)
              `catchError` (\_ -> pure $ Left rawtx)

---------------------------------
-- Helpers (works in MonadIO)
---------------------------------

createBlockDbModifyAction
    :: ( HasExceptions e [CSMappendException, StatePException], Show e, Typeable e
       , Default chgAccum
       )
    => MempoolConfig e (IOCtx chgAccum xs) (Both (MempoolTx txtypes xs) c) rawtx
    -> Mempool (Both (MempoolTx txtypes xs) c) chgAccum rawtx
    -> DbModifyActions chgAccum xs ExecM a
    -> DbModifyActions chgAccum xs ExecM (a, Rejected rawtx)
createBlockDbModifyAction cfg mem dbM = DbModifyActions (dmaAccessActions dbM) $ \chg -> do
    res <- dmaApply dbM chg
    (res,) <$> actionWithMempool mem (dmaAccessActions dbM) (normalizeMempool cfg)
