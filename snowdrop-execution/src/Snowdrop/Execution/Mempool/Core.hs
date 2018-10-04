{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Execution.Mempool.Core
       ( Mempool
       , MempoolState (..)
       , StateTxHandler (..)
       , MempoolConfig (..)
       , msTxsL
       , RwMempoolAct
       , actionWithMempool
       , createMempool
       , getMempoolTxs

       , MempoolTx
       , defaultMempoolConfig
       ) where

import           Universum

import           Control.Lens (lens)
import           Data.Default (Default (..))

import           Snowdrop.Core (CSMappendException, ChgAccum, ChgAccumCtx, ConvertEffect, DbAccessM,
                                ERwComp, ProofNExp, StatePException, StateTx (..), TxComponents,
                                UpCastableERoM, Validator, convertERwComp, convertEffect,
                                liftERoComp, modifyAccumOne, runValidator)
import           Snowdrop.Execution.DbActions (DbActions)
import           Snowdrop.Execution.Expand (ExpandOneTxMode, expandOneTx)
import           Snowdrop.Execution.IOExecutor (IOCtx, runERwCompIO)
import           Snowdrop.Util

---------------------------
-- Core part
---------------------------

type RwMempoolAct e xs ctx rawtx a =
    ERwComp e (DbAccessM (ChgAccum ctx) xs) ctx (MempoolState (ChgAccum ctx) rawtx) a

newtype StateTxHandler e ctx rawtx txtype = StateTxHandler
    { getStateTxHandler :: RwMempoolAct e (TxComponents txtype) ctx rawtx (StateTx txtype)
    }

type SomeStateTxHandler e ctx c rawtx = SomeData (StateTxHandler e ctx rawtx) c

newtype MempoolConfig e ctx c rawtx =
    MempoolConfig { mcProcessTx :: rawtx -> SomeStateTxHandler e ctx c rawtx }

data MempoolState chgAccum rawtx = MempoolState
    { msTxs      :: [rawtx]
    , msChgAccum :: chgAccum
    }

data Versioned t = Versioned
    { vsData    :: t
    , vsVersion :: Int
    }

newtype Mempool chgAccum rawtx
    = Mempool { mempoolState :: TVar (Versioned (MempoolState chgAccum rawtx)) }

instance HasGetter (MempoolState chgAccum rawtx) chgAccum where
    gett = msChgAccum

instance HasLens (MempoolState chgAccum rawtx) chgAccum where
    sett s x = s {msChgAccum = x}

msTxsL :: Lens' (MempoolState chgAccum rawtx) [rawtx]
msTxsL = lens msTxs (\s x -> s {msTxs = x})

instance Default chgAccum => Default (MempoolState chgAccum rawtx) where
    def = MempoolState def def

instance Default chgAccum => Default (Versioned (MempoolState chgAccum rawtx)) where
    def = Versioned def 0

actionWithMempool
    :: ( Show e, Typeable e, Default chgAccum
       , HasException e StatePException
       , chgAccum ~ ChgAccum (IOCtx da)
       , DbActions da daa chgAccum ExecM
       , ConvertEffect e (IOCtx da) (DbAccessM chgAccum xs) da
       )
    => Mempool chgAccum rawtx
    -> daa ExecM
    -> RwMempoolAct e xs (IOCtx da) rawtx a
    -> ExecM a
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

createMempool :: (Default chgAccum, MonadIO m) => m (Mempool chgAccum rawtx)
createMempool = Mempool <$> atomically (newTVar def)

getMempoolTxs
    :: (Default chgAccum, MonadIO m)
    => Mempool chgAccum rawtx
    -> m [rawtx]
getMempoolTxs Mempool{..} = msTxs . vsData <$> atomically (readTVar mempoolState)

---------------------------
-- Default mempool and constraints
---------------------------

class ( ExpandOneTxMode txtype
      , RContains txtypes txtype
      , UpCastableERoM (TxComponents txtype) xs
      ) => MempoolTx txtypes xs txtype
instance (
        ExpandOneTxMode txtype
      , RContains txtypes txtype
      , UpCastableERoM (TxComponents txtype) xs
      ) => MempoolTx txtypes xs txtype

defaultMempoolConfig
    :: forall e xs ctx (c :: * -> Constraint) txtypes rawtx .
    ( HasExceptions e [
          StatePException
        , CSMappendException
        ]
    , HasLens ctx (ChgAccumCtx ctx)
    )
    => (rawtx -> SomeData (ProofNExp e ctx rawtx) (Both (MempoolTx txtypes xs) c))
    -> Validator e ctx txtypes
    -> MempoolConfig e ctx (Both (MempoolTx txtypes xs) c) rawtx
defaultMempoolConfig expander validator = MempoolConfig handler
  where
    handler :: rawtx -> SomeStateTxHandler e ctx (Both (MempoolTx txtypes xs) c) rawtx
    handler rawtx = usingSomeData (expander rawtx) $ SomeData . StateTxHandler . processTx rawtx

    processTx
        :: forall txtype . MempoolTx txtypes xs txtype
        => rawtx
        -> ProofNExp e ctx rawtx txtype
        -> RwMempoolAct e (TxComponents txtype) ctx rawtx (StateTx txtype)
    processTx rawtx prfNexp = do
        tx@StateTx{..} <- liftERoComp (expandOneTx prfNexp rawtx)
        liftERoComp $ runValidator validator tx
        liftERoComp (modifyAccumOne txBody) >>= modify . flip sett
        pure tx
