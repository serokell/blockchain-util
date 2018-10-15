{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Execution.Mempool.Logic
       ( evictMempool
       , processTxAndInsertToMempool
       , normalizeMempool
       , Rejected (..)
       , logInfoMempool
       ) where

import           Universum

import           Control.Lens ((%=))
import           Control.Monad.Except (catchError)
import           Data.Default (Default (..))
import           Formatting (sformat, (%))

import           Snowdrop.Core (BException, ChgAccum, ChgAccumCtx, Ctx, HasBException,
                                StatePException, upcastEffERwCompM)
import           Snowdrop.Execution.Mempool.Core (Mempool, MempoolConfig (..), MempoolTx,
                                                  RwMempoolAct, StateTxHandler (..), getMempoolTxs,
                                                  msTxsL)
import           Snowdrop.Util

---------------------------
-- Logic
---------------------------

evictMempool
    :: Default (ChgAccum conf)
    => RwMempoolAct conf xs rawtx [rawtx]
evictMempool = gets (view msTxsL) <* put def

processTxAndInsertToMempool
    :: ( HasBException conf StatePException
       , Show (BException conf), Typeable (BException conf)
       , HasLens (Ctx conf) (ChgAccumCtx conf)
       )
    => MempoolConfig conf (Both (MempoolTx txtypes xs) c) rawtx
    -> rawtx
    -> RwMempoolAct conf xs rawtx ()
processTxAndInsertToMempool MempoolConfig{..} rawtx = usingSomeData (mcProcessTx rawtx) $
  \(StateTxHandler handler) -> do
      _tx <- upcastEffERwCompM handler -- expanded tx is not preserved, handler is used only to validate tx
      msTxsL %= (++ [rawtx])

newtype Rejected rawtx = Rejected { getRejected :: [rawtx] }

normalizeMempool
    :: forall conf rawtx txtypes xs c .
       ( HasBException conf StatePException
       , Show (BException conf), Typeable (BException conf)
       , Default (ChgAccum conf)
       , HasLens (Ctx conf) (ChgAccumCtx conf)
       )
    => MempoolConfig conf (Both (MempoolTx txtypes xs) c) rawtx
    -> RwMempoolAct conf xs rawtx (Rejected rawtx)
normalizeMempool MempoolConfig{..} = do
    txs <- evictMempool
    rejected <- processAll txs
    pure rejected
  where
    processOne :: rawtx -> RwMempoolAct conf xs rawtx ()
    processOne rawtx = usingSomeData (mcProcessTx rawtx) $ \(StateTxHandler handler) ->
        void (upcastEffERwCompM handler)

    processAll :: [rawtx] -> RwMempoolAct conf xs rawtx (Rejected rawtx)
    processAll txs = fmap (Rejected . fst . partitionEithers) $ do
        forM txs $ \rawtx ->
            (Right <$> processOne rawtx)
              `catchError` (\_ -> pure $ Left rawtx)

logInfoMempool
    :: ( Default (ChgAccum conf)
       , DBuildable rawtx
       )
    => Mempool conf rawtx
    -> ExecM ()
logInfoMempool mempool = do
    txs <- getMempoolTxs mempool
    let fmtLine dp = newlineF dp%"* "%dlater dbuild (indented dp)
    logDoc logInfo $ \dp ->
        sformat ("Mempool:\n"%bareListF "" (fmtLine dp)) txs
