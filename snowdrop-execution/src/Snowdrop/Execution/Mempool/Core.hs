{-# LANGUAGE DataKinds #-}

module Snowdrop.Execution.Mempool.Core
       ( Mempool
       , MempoolConfig (..)
       , MempoolState (..)
       , msTxsL
       , RwActionWithMempool
       , defaultMempoolConfig
       , createMempool
       , getMempoolTxs
       , actionWithMempool
       ) where

import           Universum

import           Control.Lens (lens)
import           Data.Default (Default (..))

import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx,
                                ChgAccumModifier (..), ERoComp, ERwComp, SomeTx,
                                StateModificationException, StatePException, StateTx (..), Undo,
                                Validator, applySomeTx, liftERoComp, modifyRwCompChgAccum,
                                runValidator)
import           Snowdrop.Execution.DbActions (DbAccessActions)
import           Snowdrop.Execution.IOExecutor (IOCtx, runERwCompIO)
import           Snowdrop.Util

---------------------------
-- Core part
---------------------------


type ExpanderRawTx e id c value ctx rawtx =
    rawtx -> ERoComp e id value ctx (SomeTx id value c)

type RwActionWithMempool e id value rawtx ctx a =
    ERwComp e id value ctx (MempoolState id value (ChgAccum ctx) rawtx) a

type ProcessStateTx e id c value rawtx ctx =
    SomeTx id value c -> RwActionWithMempool e id value rawtx ctx (Undo id value)

data MempoolConfig e id c value ctx rawtx = MempoolConfig
    { mcExpandTx  :: ExpanderRawTx e id c value ctx rawtx
    , mcProcessTx :: ProcessStateTx e id c value rawtx ctx
    }

data MempoolState id value chgAccum rawtx = MempoolState
    { msTxs      :: [(rawtx, Undo id value)]
    , msChgAccum :: chgAccum
    }

data Versioned t = Versioned
    { vsData    :: t
    , vsVersion :: Int
    }

instance HasGetter (MempoolState id value chgAccum rawtx) chgAccum where
    gett = msChgAccum

instance HasLens (MempoolState id value chgAccum rawtx) chgAccum where
    sett s x = s {msChgAccum = x}

msTxsL :: Lens' (MempoolState id value chgAccum rawtx) [(rawtx, Undo id value)]
msTxsL = lens msTxs (\s x -> s {msTxs = x})

instance Default chgAccum => Default (MempoolState id value chgAccum rawtx) where
    def = MempoolState def def

instance Default chgAccum => Default (Versioned (MempoolState id value chgAccum rawtx)) where
    def = Versioned def 0

newtype Mempool id value chgAccum rawtx
    = Mempool { mempoolState :: TVar (Versioned (MempoolState id value chgAccum rawtx)) }

defaultMempoolConfig
    :: forall e id value ctx c txtypes rawtx .
       ( HasExceptions e [
             StateModificationException id
           , StatePException
           , CSMappendException id
           ]
       , Ord id
       , HasLens ctx (ChgAccumCtx ctx)
       )
    => ExpanderRawTx e id (Both c (RContains txtypes)) value ctx rawtx
    -> Validator e id value ctx txtypes
    -> MempoolConfig e id (Both c (RContains txtypes)) value ctx rawtx
defaultMempoolConfig expander validator = MempoolConfig {
      mcProcessTx = applySomeTx $ \tx -> do
        liftERoComp $ runValidator validator tx
        modifyRwCompChgAccum (CAMChange $ txBody tx)
    , mcExpandTx = expander
    }

actionWithMempool
    :: ( Show e, Typeable e, Default chgAccum
       , HasReview e StatePException
       )
    => Mempool id value chgAccum rawtx
    -> DbAccessActions chgAccum id value ExecM
    -> RwActionWithMempool e id value rawtx (IOCtx chgAccum id value) a
    -> ExecM a
actionWithMempool mem@Mempool{..} dbActs callback = do
    Versioned{vsVersion=version,..} <- liftIO $ atomically $ readTVar mempoolState
    (res, newState) <- runERwCompIO dbActs vsData callback
    modified <- liftIO $ atomically $ do
        stLast <- readTVar mempoolState
        if version == vsVersion stLast then
            True <$ writeTVar mempoolState (Versioned newState (version + 1))
        else
            pure False
    if modified then pure res
    else actionWithMempool mem dbActs callback

createMempool :: (Default chgAccum, MonadIO m) => m (Mempool id value chgAccum rawtx)
createMempool = Mempool <$> atomically (newTVar def)

getMempoolTxs :: (Default chgAccum, MonadIO m) => Mempool id value chgAccum rawtx -> m [(rawtx, Undo id value)]
getMempoolTxs Mempool{..} = msTxs . vsData <$> atomically (readTVar mempoolState)

