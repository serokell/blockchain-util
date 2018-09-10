{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

-- This module is not located in Excecution, as stated in the task, because the
-- import of snowdrop-block lead to import cycle.
module Snowdrop.Block.Demo
    ( client
    -- , ClientConf (..)
    -- , ServerActions (..)
    -- , PauseSync (..)
    -- , TxSource (..)
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async.Lifted (race_)
import           Data.Default (Default (def))
import           Data.Time.Clock (getCurrentTime)
import           Fmt (format)
import           Formatting (build, sformat, (%))

import qualified Data.Set as S

import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))

import           Snowdrop.Block.Types (CurrentBlockRef (..), BlockHeader, RawPayload, BlockRef, RawBlund,
                                       RawBlk, OSParams, Time, RawTxTF, ExceptionTF, Block (..), Blund (..))
import           Snowdrop.Block.Application (tryApplyFork, BlockApplicationException)
import           Snowdrop.Block.Fork (ForkVerificationException, iterateChain)
import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Core (ERwComp, StatePException (..))
-- In case Demo.hs will end up in Execution
-- import           Snowdrop.Execution.IOExecutor (BaseMException, runERwCompIO)
-- import           Snowdrop.Execution.DbActions (ClientMode (..), DbModifyActions (..))
-- import           Snowdrop.Execution.Mempool (Mempool, actionWithMempool, normalizeMempool, processTxAndInsertToMempool)
--
import           Snowdrop.Execution (BaseMException, MempoolConfig (..), runERwCompIO, ClientMode (..), IOCtx,
                                                DbModifyActions (..), Mempool, actionWithMempool,
                                                normalizeMempool, processTxAndInsertToMempool)
-- import           Snowdrop.Impl.Signature (SimpleSigScheme)
-- import           Snowdrop.Impl.Simple.Configuration (DemoConfiguration (..), Exceptions, IOCtx, Ids,
--                                                      SHeader, SPayload, SUndo, Values)
-- import           Snowdrop.Impl.Simple.Node.Common (ComputationCtx (..), NodeConf (..),
--                                                    forkDepthChain, mempoolConfig,
--                                                    runCompositeAction, wholeChain)
-- import           Snowdrop.Impl.Simple.Raw (RawTx)
-- import           Snowdrop.Impl.Simple.StateConfiguration (blkStateConfig)
-- import           Snowdrop.Impl.Simple.Types (Coin, Hash, RawBlock (..), RawBlockBody, OSParams,
--                                              OSParamsBuilder (..))
import           Snowdrop.Util

newtype TxSource blkType = TxSource
    { unTxSource :: MVar (RawTxTF blkType)
    }

newtype PauseSync = PauseSync
    { unPauseSync :: MVar Bool
    }

------------------------------------------
-- Logic
------------------------------------------

-- | Actions used by client to communicate with server part
data ServerActions proof blkType = ServerActions
    { saSubmitTx  :: (RawTxTF blkType) -> ExecM ()
    , saBlockSync :: [(BlockRef blkType)] -> ExecM [RawBlk blkType]
    }

-- | Configuration for client
data ClientConf stateChgAccum blockChgAccum proof ids values sigScheme blkType = ClientConf
    { ccNodeConf      :: ClientMode proof -> NodeConf stateChgAccum blockChgAccum () ids values sigScheme
    -- ^ This method takes ClientMode as param in order to allow client methods to
    -- be executed either with proof of block (containing necessary data for all
    -- requests that will happen) or function to load neccessary parts of state on demand
    , ccServerActions :: ServerActions proof blkType
    , ccMempool       :: Mempool ids values stateChgAccum (RawTxTF blkType)
    , ccIncomingTx    :: TxSource blkType
    , ccPauseSync     :: PauseSync
    , ccSyncDelay     :: Int -- TODO: Use Time data types
    -- ^ If true pause block sync on client
    }

data NodeConf stateChgAccum blockChgAccum proof ids values sigScheme = NodeConf
    { -- TODO in real application we probably need RawBlund in storage
      -- to reply to others nodes.
      nsStateDBActions :: DbModifyActions stateChgAccum ids values ExecM proof
    , nsBlockDBActions :: DbModifyActions blockChgAccum ids values ExecM ()
    , nsStId           :: PublicKey sigScheme
    }

-- TODO: remove it from the snowdrop
-- TODO: Is it general enough?
data DemoConfiguration =
    DemoConfiguration
      { enableFee          :: Bool
      , enableAvlP         :: Bool
      , logConfigPath      :: Maybe FilePath
      , blockCreationDelay :: Int
      , clientSyncDelay    :: Int
      , dropTxsOnRollback  :: Bool
      }

-- TODO: remove it from the snowdrop
newtype OsParamsBuilder blkType = OsParamsBuilder { unOSParamsBuilder :: Time -> OSParams blkType }

-- TODO: remove it from snowdrop
data ComputationCtx stateChgAccum blockChgAccum compositeChgAccum proof blkType ids values sigScheme = ComputationCtx
    { demoConf :: DemoConfiguration
    , nodeConf :: NodeConf stateChgAccum blockChgAccum proof ids values sigScheme
    , bsConf   :: BlkStateConfiguration blkType (ERwComp (ExceptionTF blkType) ids values (IOCtx compositeChgAccum ids values) compositeChgAccum)
    , osParams :: OSParams blkType
    }

client
    :: forall stateChgAccum blockChgAccum proof blkType ids values sigScheme .
    ( Default stateChgAccum
    , Default blockChgAccum
    , DBuildable (RawTxTF blkType)
    , Buildable (RawTxTF blkType)
    , Buildable (BlockRef blkType)
    , Typeable (ExceptionTF blkType)
    , Buildable (ExceptionTF blkType)
    , Show (ExceptionTF blkType)
    , Eq (BlockRef blkType)
    , HasGetter (RawBlk blkType) (RawPayload blkType)
    , HasGetter (RawBlk blkType) (BlockHeader blkType)
    , HasReview (ExceptionTF blkType) StatePException
    , HasReview
        (ExceptionTF blkType)
        (ForkVerificationException (BlockRef blkType))
    , HasReview
        (ExceptionTF blkType)
        (BlockApplicationException (BlockRef blkType))
    )
    => DemoConfiguration
    -> OsParamsBuilder blkType
    -> ClientConf stateChgAccum blockChgAccum proof ids values sigScheme blkType
    -> BlkStateConfiguration blkType (ERwComp (ExceptionTF blkType) ids values (IOCtx blockChgAccum ids values) blockChgAccum)
    -- TODO: Move `MempoolConfig` to some config. Same ids values for state and block -ChgAccum
    -> MempoolConfig (ExceptionTF blkType) ids proof values (IOCtx stateChgAccum ids values) (RawTxTF blkType)
    -> ExecM ()
client demoConfig osParamsBuilder ClientConf {..} bsConf mempoolCfg =
    race_ blockApplyThread txRcvThread
  where

    clientLog = modifyLogName (const "Client")

    stateDba = dmaAccessActions $ nsStateDBActions $ ccNodeConf $ RemoteMode
    blockDba = dmaAccessActions $ nsBlockDBActions $ ccNodeConf $ RemoteMode

    txRcvThread :: ExecM ()
    txRcvThread = clientLog $ forever $ do
        tx <- liftIO $ takeMVar $ unTxSource ccIncomingTx
        let logErr e = logError $ format "Validation of tx failed: {}" e
        do
            actionWithMempool ccMempool stateDba (processTxAndInsertToMempool mempoolCfg tx)
            logDoc logInfo $ \dp ->
                sformat ("Submitting tx to server: "%(indented dp & newlineF%%docF)) tx
            saSubmitTx ccServerActions tx
          `catch` (\(e :: BaseMException (ExceptionTF blkType)) -> logErr e)

    blockApplyThread :: ExecM ()
    blockApplyThread = clientLog $ do
        logInfo $ format "Sync will be attempted every {} seconds" delay
        blockApplyThreadLoop
      where
        delay = clientSyncDelay demoConfig

        blockApplyThreadLoop = forever $ do
            liftIO $ threadDelay $ delay * 1000 * 1000
            paused <- readMVar $ unPauseSync ccPauseSync
            modifyLogName (<> "Syncing") $
                if paused then do
                    logInfo "====== Client synchronisation paused ======"
                else do
                  hashes    <- fst <$> runERwCompIO blockDba def (forkDepthChain bsConf)
                  rawBlocks <- saBlockSync ccServerActions (unCurrentBlockRef <$> unOldestFirst hashes)
                  logDoc logInfo (const $ sformat ("Asked for  : " %listF ", " build) (unCurrentBlockRef <$> unOldestFirst hashes))
                  osParams <- liftIO $ unOSParamsBuilder osParamsBuilder <$> getCurrentTime
                  case rawBlocks of
                    [] -> logInfo $ "====== Nothing to synchronise ======"
                    (x:xs) -> let
                        neRawBlocks :: OldestFirst NonEmpty (RawBlk blkType)
                        neRawBlocks = OldestFirst (x :| xs)
                      in do
                            void $ runCompositeAction
                                    demoConf -- TODO: try to get rid of this
                                    osParams
                                    (ccNodeConf RemoteMode)
                                    (undoAndApplyBlocks neRawBlocks)
                            void $ actionWithMempool ccMempool
                                                    (dmaAccessActions $ nsStateDBActions $ ccNodeConf RemoteMode)
                                                    (normalizeMempool mempoolCfg)

        undoAndApplyBlocks
            :: OldestFirst NonEmpty (RawBlk blkType)
            -> ComputationCtx stateChgAccum blockChgAccum compositeChgAccum proof blkType ids values sigScheme
            -> ERwComp (ExceptionTF blkType) ids values (IOCtx compositeChgAccum ids values) compositeChgAccum Bool
        undoAndApplyBlocks neBlocks ComputationCtx{..} =
                tryApplyFork bsConf osParams neBlocks


-- TODO: get rid of code duplication
-- | 'wholeChain' retrieves list of Hashes of whole blockchain in oldest first order.
-- | 'forkDepthChain' retrieves list of Hashes from newest to maxForkDepth in oldest first order.
forkDepthChain
    :: Monad m
    => BlkStateConfiguration blkType m
    -> m (OldestFirst [] (CurrentBlockRef blkType))
forkDepthChain bsConf = nDepthChain bsConf $ bcMaxForkDepth $ bscConfig bsConf

-- TODO: get rid of code duplication
nDepthChain
    :: Monad m
    => BlkStateConfiguration blkType m
    -> Int
    -> m (OldestFirst [] (CurrentBlockRef blkType))
nDepthChain bsConf depth = toOldestFirst . fmap (getCurrentBlockRef $ bscConfig bsConf) <$> iterateChain bsConf depth

-- TODO: get rid of code duplication
getCurrentBlockRef :: BlkConfiguration blkType -> RawBlund blkType -> CurrentBlockRef blkType
getCurrentBlockRef BlkConfiguration{..} = bcBlockRef . blkHeader . buBlock

-- TODO: remove from snowdrop and find a better place here
runCompositeAction = undefined
--     :: forall blockChgAccum stateChgAccum proof a.
--     ( Default blockChgAccum
--     , Default stateChgAccum
--     )
--     => DemoConfiguration
--     -> OsParams
--     -> NodeConf stateChgAccum blockChgAccum proof
--     -> (forall ps compositeChgAccum .
--        (compositeChgAccum ~ (CompositeChgAccum blockChgAccum stateChgAccum ps))
--        => ComputationCtx stateChgAccum blockChgAccum compositeChgAccum proof
--        -> ERwComp Exceptions Ids Values (IOCtx compositeChgAccum) compositeChgAccum a
--        )
--     -> ExecM (a, proof)
-- runCompositeAction demoConf osParams nc@NodeConf{..} f = do
--     (blockCS, stateCS, a) <- reify blockPrefixes $ \(_ :: Proxy ps) ->
--       let actions = constructCompositeActions @ps blockDba stateDba
--           rwComp = f (ComputationCtx demoConf nc bsConf osParams)
--        in runERwCompIO actions def rwComp >>=
--               \(a, (CompositeChgAccum blockCS_ stateCS_)) -> pure (blockCS_, stateCS_, a)

--     proof <- dmaApply nsStateDBActions stateCS
--     void $ dmaApply nsBlockDBActions blockCS
--     pure (a, proof)
--    where
--     blockPrefixes = S.fromList [tipPrefix, blockPrefix]
--     stateDba = dmaAccessActions nsStateDBActions
--     blockDba = dmaAccessActions nsBlockDBActions

--     bsConf :: Default chgAccum => BlkStateConfiguration BDemo (ERwComp Exceptions Ids Values (IOCtx chgAccum) chgAccum)
--     bsConf = blkStateConfig demoConf nsStId

-- TODO:
-- Make type families: ids values sigScheme