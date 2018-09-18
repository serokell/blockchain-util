{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- This module is not located in Excecution, as stated in the task, because the
-- import of snowdrop-block lead to import cycle.
module Snowdrop.Block.Demo
    ( client
    , forkDepthChain
    , nDepthChain
    , getCurrentBlockRef
    , runCompositeAction
    , ClientConf (..)
    , ServerActions (..)
    , PauseSync (..)
    , TxSource (..)

    , ComputationCtx (..)
    , NodeConf (..)
    , OsParamsBuilder (..)
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Data.Reflection (reify, Reifies)
import           Control.Exception (SomeException)
import           Control.Concurrent.Async.Lifted (race_)
import           Data.Default (Default (def))
import           Data.Time.Clock (getCurrentTime)
import           Fmt (format)
import           Formatting (build, sformat, (%))
import           Data.Time.Units (toMicroseconds, Second)

import qualified Data.Set as S

import           Snowdrop.Block.StateConfiguration (BlkStateConfiguration (..))

import           Snowdrop.Block.Types (CurrentBlockRef (..), BlockHeader, RawPayload, BlockRef, RawBlund,
                                       RawBlk, OSParams, Time, Block (..), Blund (..))
import           Snowdrop.Block.Application (tryApplyFork, BlockApplicationException)
import           Snowdrop.Block.Fork (ForkVerificationException, iterateChain)
import           Snowdrop.Block.Configuration (BlkConfiguration (..))
import           Snowdrop.Core (ERwComp, StatePException (..), Prefix (..), IdSumPrefixed)

import           Snowdrop.Execution (DbAccessActions, CompositeChgAccum (..), BaseMException, MempoolConfig (..), runERwCompIO, ClientMode (..), IOCtx,
                                                DbModifyActions (..), Mempool, actionWithMempool, constructCompositeActions,
                                                normalizeMempool, processTxAndInsertToMempool)

import           Snowdrop.Util

-- TODO: Do we still need this?
newtype TxSource blkType rawTx = TxSource
    { unTxSource :: MVar rawTx
    }

newtype PauseSync = PauseSync
    { unPauseSync :: MVar Bool
    }

------------------------------------------
-- Logic
------------------------------------------

-- | Actions used by client to communicate with server part
data ServerActions blkType rawTx = ServerActions
    { saSubmitTx  :: rawTx -> ExecM ()
    , saBlockSync :: [(BlockRef blkType)] -> ExecM [RawBlk blkType]
    }

-- | Configuration for client
data ClientConf stateChgAccum blockChgAccum proof ids values sigScheme blkType rawTx = ClientConf
    { ccNodeConf      :: ClientMode rawTx -> NodeConf stateChgAccum blockChgAccum proof ids values sigScheme
    -- ^ This method takes ClientMode as param in order to allow client methods to
    -- be executed either with proof of block (containing necessary data for all
    -- requests that will happen) or function to load neccessary parts of state on demand
    , ccServerActions :: ServerActions blkType rawTx
    , ccMempool       :: Mempool ids values stateChgAccum rawTx
    , ccIncomingTx    :: TxSource blkType rawTx -- TODO: Do we still need this?
    , ccPauseSync     :: PauseSync
    , ccSyncDelay     :: Second
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
-- TODO: Is it general enough? (needed for ComputationCtx which is needed for runCompositeAction)
-- data DemoConfiguration =
--     DemoConfiguration
--       { enableFee          :: Bool
--       , enableAvlP         :: Bool
--       , logConfigPath      :: Maybe FilePath
--       , blockCreationDelay :: Int
--       , clientSyncDelay    :: Int
--       , dropTxsOnRollback  :: Bool
--       }

-- TODO: remove it from the snowdrop
newtype OsParamsBuilder blkType = OsParamsBuilder { unOSParamsBuilder :: Time -> OSParams blkType }

-- TODO: remove it from snowdrop
data ComputationCtx stateChgAccum blockChgAccum compositeChgAccum proof blkType ids values sigScheme e = ComputationCtx
    { nodeConf :: NodeConf stateChgAccum blockChgAccum proof ids values sigScheme
    , bsConf   :: BlkStateConfiguration blkType (ERwComp e ids values (IOCtx compositeChgAccum ids values) compositeChgAccum)
    , osParams :: OSParams blkType
    }

client
    :: forall stateChgAccum blockChgAccum compositeChgAccum proof txtypes blkType ids values sigScheme rawTx e ps.
    ( Default stateChgAccum
    , Default blockChgAccum
    , DBuildable rawTx
    , Buildable rawTx
    , Buildable (BlockRef blkType)
    , Typeable e
    , Buildable e
    , Show e
    , Eq (BlockRef blkType)
    , HasGetter (RawBlk blkType) (RawPayload blkType)
    , HasGetter (RawBlk blkType) (BlockHeader blkType)
    , IdSumPrefixed ids
    , Ord ids
    , HasExceptions e
        [ StatePException
        , (ForkVerificationException (BlockRef blkType))
        , (BlockApplicationException (BlockRef blkType))
        ]
    , compositeChgAccum ~ (CompositeChgAccum blockChgAccum stateChgAccum ps)
    , Reifies ps (Set Prefix)
    )
    => OsParamsBuilder blkType
    -> ClientConf stateChgAccum blockChgAccum proof ids values sigScheme blkType rawTx
    -- TODO: Try to use one BlkStateConfiguration.
    -> BlkStateConfiguration blkType (ERwComp e ids values (IOCtx compositeChgAccum ids values) compositeChgAccum)
    -> BlkStateConfiguration blkType (ERwComp e ids values (IOCtx blockChgAccum ids values) blockChgAccum)
    -- @id: have no idea what (RContains txtypes) is.
    -> MempoolConfig e ids (RContains txtypes) values (IOCtx stateChgAccum ids values) rawTx
    -> ExecM rawTx
    -> ([RawBlk blkType] -> ExecM ())
    -> ExecM ()
client osParamsBuilder ClientConf {..} bsConfCompositeChgAccum bsConfBlockChgAccum mempoolCfg rcvTx onSync =
    race_ blockApplyThread txRcvThread
  where
    clientLog = modifyLogName (const "Client")

    stateDba = dmaAccessActions $ nsStateDBActions $ ccNodeConf $ RemoteMode
    blockDba = dmaAccessActions $ nsBlockDBActions $ ccNodeConf $ RemoteMode

    txRcvThread :: ExecM ()
    txRcvThread = clientLog $ forever $ do
        tx <- rcvTx
        let logErr e = logError $ format "Validation of tx failed: {}" e
        do
            actionWithMempool ccMempool stateDba (processTxAndInsertToMempool mempoolCfg tx)
            logDoc logInfo $ \dp ->
                sformat ("Submitting tx to server: "%(indented dp & newlineF%%docF)) tx
            saSubmitTx ccServerActions tx
          `catch` (\(e :: BaseMException e) -> logErr e)

    blockApplyThread :: ExecM ()
    blockApplyThread = clientLog $ do
        logInfo $ format "Sync will be attempted every {} seconds" (show @Text ccSyncDelay)
        blockApplyThreadLoop
      where
        blockApplyThreadLoop = forever $ do
            liftIO $ threadDelay $ fromInteger $ toMicroseconds ccSyncDelay
            paused <- readMVar $ unPauseSync ccPauseSync
            newBlocks <- modifyLogName (<> "Syncing") $
                if paused then do
                    logInfo "====== Client synchronisation paused ======"
                    return [] -- TODO: Should onSync be excecuted in that case?
                else do
                  hashes    <- fst <$> runERwCompIO blockDba def (forkDepthChain bsConfBlockChgAccum)
                  rawBlocks <- saBlockSync ccServerActions (unCurrentBlockRef <$> unOldestFirst hashes)
                  logDoc logInfo (const $ sformat ("Asked for  : " %listF ", " build) (unCurrentBlockRef <$> unOldestFirst hashes))
                  osParams <- liftIO $ unOSParamsBuilder osParamsBuilder <$> getCurrentTime
                  case rawBlocks of
                    [] -> (logInfo $ "====== Nothing to synchronise ======") >> (return rawBlocks)
                    (x:xs) -> let
                        neRawBlocks :: OldestFirst NonEmpty (RawBlk blkType)
                        neRawBlocks = OldestFirst (x :| xs)
                      in do
                            void $ runCompositeAction
                                    bsConfCompositeChgAccum
                                    osParams
                                    (ccNodeConf RemoteMode)
                                    (undoAndApplyBlocks neRawBlocks)
                            void $ actionWithMempool ccMempool
                                                    (dmaAccessActions $ nsStateDBActions $ ccNodeConf RemoteMode)
                                                    (normalizeMempool mempoolCfg)
                            return rawBlocks

            catch
                (onSync newBlocks)
                -- TODO: Use more concrete exceptions?
                (\(e :: SomeException) ->
                    logError $ format "onSync finished with exception: {}" (show @Text e))

        undoAndApplyBlocks
            :: OldestFirst NonEmpty (RawBlk blkType)
            -> ComputationCtx stateChgAccum blockChgAccum compositeChgAccum proof blkType ids values sigScheme e
            -> ERwComp e ids values (IOCtx compositeChgAccum ids values) compositeChgAccum Bool
        undoAndApplyBlocks neBlocks ComputationCtx {..} =
                tryApplyFork bsConf osParams neBlocks


-- TODO: get rid of code duplication
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

-- TODO: get rid of code duplication (after SD-132 merge)
getCurrentBlockRef :: BlkConfiguration blkType -> RawBlund blkType -> CurrentBlockRef blkType
getCurrentBlockRef BlkConfiguration{..} = bcBlockRef . blkHeader . buBlock

-- TODO: Problem: ComputationCtx need to carry bsConf with compositeChgAccum but for
-- line `runERwCompIO blockDba def (forkDepthChain bsConf_for_block_dba)` bsConf with blockChgAccum.
-- Do we really need two distinct chgAccoums?
runCompositeAction
    :: forall blockChgAccum stateChgAccum compositeChgAccum proof a e ids values blkType sigScheme ps.
    ( Default blockChgAccum
    , Default stateChgAccum
    , compositeChgAccum ~ (CompositeChgAccum blockChgAccum stateChgAccum ps)
    , HasReview e StatePException
    , Typeable e
    , Show e
    , IdSumPrefixed ids
    , Ord ids
    , Reifies ps (Set Prefix)
    )
    => BlkStateConfiguration blkType (ERwComp e ids values (IOCtx compositeChgAccum ids values) compositeChgAccum)
    -> OSParams blkType
    -> NodeConf stateChgAccum blockChgAccum proof ids values sigScheme
    -> (  ComputationCtx stateChgAccum blockChgAccum compositeChgAccum proof blkType ids values sigScheme e
       -> ERwComp e ids values (IOCtx compositeChgAccum ids values) compositeChgAccum a
       )
    -> ExecM (a, proof)
runCompositeAction bsConf osParams nc@NodeConf{..} f = do
    (blockCS, stateCS, a) <- reify blockPrefixes $ \(_ :: Proxy s) ->
      let
          actions :: DbAccessActions (CompositeChgAccum blockChgAccum stateChgAccum ps) ids values ExecM
          actions = constructCompositeActions @ps blockDba stateDba

          rwComp :: ERwComp e ids values (IOCtx compositeChgAccum ids values) compositeChgAccum a
          rwComp = f (ComputationCtx nc bsConf osParams)

       in runERwCompIO actions def rwComp >>=
              \(a, (CompositeChgAccum blockCS_ stateCS_)) -> pure (blockCS_, stateCS_, a)

    proof <- dmaApply nsStateDBActions stateCS
    void $ dmaApply nsBlockDBActions blockCS
    pure (a, proof)
   where
    tipPrefix :: Prefix
    tipPrefix = Prefix 1

    blockPrefix :: Prefix
    blockPrefix = Prefix 2

    blockPrefixes = S.fromList [tipPrefix, blockPrefix]
    stateDba = dmaAccessActions nsStateDBActions
    blockDba = dmaAccessActions nsBlockDBActions
