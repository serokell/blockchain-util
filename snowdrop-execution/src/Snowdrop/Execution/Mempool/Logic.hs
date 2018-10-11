{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Execution.Mempool.Logic
       ( evictMempool
       , processTxAndInsertToMempool
       , normalizeMempool
       , Rejected (..)
       ) where

import           Universum

import           Control.Lens ((%=))
import           Control.Monad.Except (catchError)
import           Data.Default (Default (..))

import           Snowdrop.Core (BException, ChgAccum, ChgAccumCtx, Ctx, HasBException,
                                StatePException, upcastEffERwCompM)
import           Snowdrop.Execution.Mempool.Core (MempoolConfig (..), MempoolState (..), MempoolTx,
                                                  RwMempoolAct, StateTxHandler (..), msTxsL)
import           Snowdrop.Util

---------------------------
-- Logic
---------------------------

evictMempool
    :: Default (ChgAccum conf)
    => RwMempoolAct conf xs rawtx [rawtx]
evictMempool = gets msTxs <* put def

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
