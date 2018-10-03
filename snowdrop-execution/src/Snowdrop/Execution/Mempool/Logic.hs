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

import           Snowdrop.Core (ChgAccum, ChgAccumM, StatePException, liftERoComp)
import           Snowdrop.Execution.Mempool.Core (MempoolConfig (..), MempoolState (..),
                                                  RwActionWithMempool, msTxsL)
import           Snowdrop.Util

---------------------------
-- Logic
---------------------------

evictMempool
    :: Default (ChgAccum ctx)
    => RwActionWithMempool e id value rawtx ctx [rawtx]
evictMempool = gets msTxs <* put def

processTxAndInsertToMempool
    :: ( HasException e StatePException, Show e, Typeable e
       , HasLens ctx (ChgAccumM (ChgAccum ctx))
       )
    => MempoolConfig e id txtype value ctx rawtx
    -> rawtx
    -> RwActionWithMempool e id value rawtx ctx ()
processTxAndInsertToMempool MempoolConfig{..} rawtx = do
    tx <- liftERoComp (mcExpandTx rawtx)
    () <- mcProcessTx tx
    msTxsL %= (++ [rawtx])

newtype Rejected rawtx = Rejected { getRejected :: [rawtx] }

normalizeMempool
    :: ( HasException e StatePException, Show e, Typeable e
       , Default (ChgAccum ctx)
       , HasLens ctx (ChgAccumM (ChgAccum ctx))
       )
    => MempoolConfig e id txtype value ctx rawtx
    -> RwActionWithMempool e id value rawtx ctx (Rejected rawtx)
normalizeMempool MempoolConfig{..} = do
    txs <- evictMempool
    (rejected, _) <- processAll txs
    pure rejected
  where
    processOne rawtx = do
        tx <- liftERoComp (mcExpandTx rawtx)
        rawtx <$ mcProcessTx tx

    processAll txs = fmap (first Rejected . partitionEithers) $ do
        forM txs $ \rawtx ->
            (Right <$> processOne rawtx)
              `catchError` (\_ -> pure $ Left rawtx)
