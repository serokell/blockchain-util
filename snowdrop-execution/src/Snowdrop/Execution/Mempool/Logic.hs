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

import           Snowdrop.Core (ChgAccum, ChgAccumCtx, StatePException, TxComponents,
                                UpCastableERoM, upcastEffERwCompM)
import           Snowdrop.Execution.Mempool.Core (MempoolConfig (..), MempoolState (..),
                                                  RwMempoolAct, StateTxHandler (..), msTxsL)
import           Snowdrop.Util

---------------------------
-- Logic
---------------------------

evictMempool
    :: Default (ChgAccum ctx)
    => RwMempoolAct e xs ctx rawtx [rawtx]
evictMempool = gets msTxs <* put def

class UpCastableERoM (TxComponents txtype) xs => MempoolMode xs txtype
instance UpCastableERoM (TxComponents txtype) xs => MempoolMode xs txtype

processTxAndInsertToMempool
    :: ( HasException e StatePException, Show e, Typeable e
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => MempoolConfig e ctx (Both (MempoolMode xs) c) rawtx
    -> rawtx
    -> RwMempoolAct e xs ctx rawtx ()
processTxAndInsertToMempool MempoolConfig{..} rawtx = usingSomeData (mcProcessTx rawtx) $
  \(StateTxHandler handler) -> do
      _tx <- upcastEffERwCompM handler -- expanded tx is not preserved, handler is used only to validate tx
      msTxsL %= (++ [rawtx])

newtype Rejected rawtx = Rejected { getRejected :: [rawtx] }

normalizeMempool
    :: forall e ctx rawtx xs c .
       ( HasException e StatePException, Show e, Typeable e
       , Default (ChgAccum ctx)
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => MempoolConfig e ctx (Both (MempoolMode xs) c) rawtx
    -> RwMempoolAct e xs ctx rawtx (Rejected rawtx)
normalizeMempool MempoolConfig{..} = do
    txs <- evictMempool
    (rejected, _) <- processAll txs
    pure rejected
  where
    processOne :: rawtx -> RwMempoolAct e xs ctx rawtx ()
    processOne rawtx = usingSomeData (mcProcessTx rawtx) $ \(StateTxHandler handler) ->
        void (upcastEffERwCompM handler)

    processAll :: [rawtx] -> RwMempoolAct e xs ctx rawtx (Rejected rawtx, [()])
    processAll txs = fmap (first Rejected . partitionEithers) $ do
        forM txs $ \rawtx ->
            (Right <$> processOne rawtx)
              `catchError` (\_ -> pure $ Left rawtx)
