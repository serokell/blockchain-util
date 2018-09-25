{-# LANGUAGE DataKinds #-}

module Snowdrop.Execution.Mempool.Core
       ( Mempool
       , MempoolConfig (..)
       , MempoolState (..)
       , ExpanderRawTx
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

import           Snowdrop.Core (CSMappendException (..), ChgAccum, ChgAccumCtx, ConvertEffect,
                                DbAccessM, ERoCompM, ERwComp, SomeTx, StatePException, StateTx (..),
                                Validator, applySomeTx, convertERwComp, convertEffect, liftERoComp,
                                modifyAccumOne, runValidator)
import           Snowdrop.Execution.DbActions (DbActions)
import           Snowdrop.Execution.IOExecutor (IOCtx, runERwCompIO, BaseMIOContstraint)
import           Snowdrop.Util

---------------------------
-- Core part
---------------------------


type ExpanderRawTx e id c value ctx rawtx =
    rawtx -> ERoCompM e id value ctx (SomeTx id value c)

type RwActionWithMempool e id value rawtx ctx a =
    ERwComp e (DbAccessM (ChgAccum ctx) id value) ctx (MempoolState id value (ChgAccum ctx) rawtx) a

type ProcessStateTx e id c value rawtx ctx =
    SomeTx id value c -> RwActionWithMempool e id value rawtx ctx ()

data MempoolConfig e id c value ctx rawtx = MempoolConfig
    { mcExpandTx  :: ExpanderRawTx e id c value ctx rawtx
    , mcProcessTx :: ProcessStateTx e id c value rawtx ctx
    }

data MempoolState id value chgAccum rawtx = MempoolState
    { msTxs      :: [rawtx]
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

msTxsL :: Lens' (MempoolState id value chgAccum rawtx) [rawtx]
msTxsL = lens msTxs (\s x -> s {msTxs = x})

instance Default chgAccum => Default (MempoolState id value chgAccum rawtx) where
    def = MempoolState def def

instance Default chgAccum => Default (Versioned (MempoolState id value chgAccum rawtx)) where
    def = Versioned def 0

newtype Mempool id value chgAccum rawtx = Mempool
    { mempoolState :: TVar (Versioned (MempoolState id value chgAccum rawtx)) }

defaultMempoolConfig
    :: forall e id value ctx txtypes rawtx .
    ( HasExceptions e [
          StatePException
        , CSMappendException id
        ]
    , Ord id
    , HasLens ctx (ChgAccumCtx ctx)
    )
    => ExpanderRawTx e id (RContains txtypes) value ctx rawtx
    -> Validator e id value ctx txtypes
    -> MempoolConfig e id (RContains txtypes) value ctx rawtx
defaultMempoolConfig expander validator = MempoolConfig {
      mcProcessTx = applySomeTx $ \tx -> do
        chgAccum' <- liftERoComp $ do
            () <- convertEffect $ runValidator validator tx
            modifyAccumOne (txBody tx)
        modify (flip sett chgAccum')
    , mcExpandTx = expander
    }

actionWithMempool
  :: forall e da chgAccum id value rawtx daa a ctx m .
       ( Show e, Typeable e, Default chgAccum
       , HasReview e StatePException
       , chgAccum ~ ChgAccum (IOCtx da m)
       , DbActions da daa chgAccum m
       , ConvertEffect e (IOCtx da m) (DbAccessM chgAccum id value) da
       , BaseMIOContstraint ctx m
       , MonadIO m
       )
    => Mempool id value chgAccum rawtx
    -> daa m
    -> RwActionWithMempool e id value rawtx (IOCtx da m) a
    -> m a
actionWithMempool mem@Mempool{..} dbActs callback = do
    Versioned{vsVersion=version,..} <- liftIO $ atomically $ readTVar mempoolState
    (res, newState) <- runERwCompIO dbActs vsData (convertERwComp convertEffect callback)
    modified <- liftIO $ atomically $ do
        stLast <- readTVar mempoolState
        if version == vsVersion stLast then
            True <$ writeTVar mempoolState (Versioned newState (version + 1))
        else
            pure False
    if modified
    then pure res
    else actionWithMempool mem dbActs callback

createMempool :: (Default chgAccum, MonadIO m) => m (Mempool id value chgAccum rawtx)
createMempool = Mempool <$> atomically (newTVar def)

getMempoolTxs
    :: (Default chgAccum, MonadIO m)
    => Mempool id value chgAccum rawtx
    -> m [rawtx]
getMempoolTxs Mempool{..} = msTxs . vsData <$> atomically (readTVar mempoolState)
