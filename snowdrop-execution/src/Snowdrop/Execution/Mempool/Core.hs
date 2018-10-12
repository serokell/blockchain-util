{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Execution.Mempool.Core
       ( Mempool
       , MempoolState
       , StateTxHandler (..)
       , MempoolConfig (..)
       , msTxsL
       , RwMempoolAct
       , actionWithMempool
       , createMempool
       , getMempoolTxs
       , getMempoolChgAccum

       , MempoolTx
       , defaultMempoolConfig
       ) where

import           Universum

import           Control.Lens (lens)
import           Data.Default (Default (..))

import           Snowdrop.Core (BException, CSMappendException, ChgAccum, ChgAccumCtx,
                                ConvertEffect, Ctx, DbAccessM, ERwComp, HasBException,
                                HasBExceptions, ProofNExp, StatePException, StateTx (..),
                                TxComponents, UpCastableERoM, Validator, convertERwComp,
                                convertEffect, liftERoComp, modifyAccumOne, runValidator)
import           Snowdrop.Execution.DbActions (DbActions)
import           Snowdrop.Execution.Expand (ExpandOneTxMode, expandOneTx)
import           Snowdrop.Execution.IOExecutor (IOCtx, IOExecEffect, runERwCompIO)
import           Snowdrop.Util

---------------------------
-- Core part
---------------------------

type RwMempoolAct conf xs rawtx a =
    ERwComp conf (DbAccessM conf xs) (MempoolState conf rawtx) a

newtype StateTxHandler conf rawtx txtype = StateTxHandler
    { getStateTxHandler :: RwMempoolAct conf (TxComponents txtype) rawtx (StateTx txtype)
    }

type SomeStateTxHandler conf c rawtx = SomeData (StateTxHandler conf rawtx) c

newtype MempoolConfig conf c rawtx =
    MempoolConfig { mcProcessTx :: rawtx -> SomeStateTxHandler conf c rawtx }

data MempoolState' chgAccum rawtx = MempoolState
    { msTxs      :: [rawtx]
    , msChgAccum :: chgAccum
    }

type MempoolState conf = MempoolState' (ChgAccum conf)

data Versioned t = Versioned
    { vsData    :: t
    , vsVersion :: Int
    }

newtype Mempool conf rawtx
    = Mempool { mempoolState :: TVar (Versioned (MempoolState conf rawtx)) }

instance HasGetter (MempoolState' chgAccum rawtx) chgAccum where
    gett = msChgAccum

instance HasLens (MempoolState' chgAccum rawtx) chgAccum where
    sett s x = s {msChgAccum = x}

msTxsL :: Lens' (MempoolState' chgAccum rawtx) [rawtx]
msTxsL = lens msTxs (\s x -> s {msTxs = x})

instance Default chgAccum => Default (MempoolState' chgAccum rawtx) where
    def = MempoolState def def

instance Default chgAccum => Default (Versioned (MempoolState' chgAccum rawtx)) where
    def = Versioned def 0

actionWithMempool
    :: forall daa xs conf rawtx a chgAccum .
      ( Show (BException conf), Typeable (BException conf)
      , Default chgAccum
      , HasBException conf StatePException
      , chgAccum ~ ChgAccum conf
      , DbActions (IOExecEffect conf) daa chgAccum ExecM
      , ConvertEffect conf (DbAccessM conf xs) (IOExecEffect conf)
      , Ctx conf ~ IOCtx conf
      )
    => Mempool conf rawtx
    -> daa ExecM
    -> RwMempoolAct conf xs rawtx a
    -> ExecM a
actionWithMempool mem@Mempool{..} dbActs callback = do
    Versioned{vsVersion=version,..} <- liftIO $ atomically $ readTVar mempoolState
    (res, newState) <- runERwCompIO dbActs vsData (convertERwComp (convertEffect @conf) callback)
    modified <- liftIO $ atomically $ do
        stLast <- readTVar mempoolState
        if version == vsVersion stLast then
            True <$ writeTVar mempoolState (Versioned newState (version + 1))
        else
            pure False
    if modified
    then pure res
    else actionWithMempool mem dbActs callback

createMempool :: (Default (ChgAccum conf), MonadIO m) => m (Mempool conf rawtx)
createMempool = Mempool <$> atomically (newTVar def)

getMempoolChgAccum
    :: (Default (ChgAccum conf), MonadIO m)
    => Mempool conf rawtx
    -> m (ChgAccum conf)
getMempoolChgAccum Mempool{..} = msChgAccum . vsData <$> atomically (readTVar mempoolState)

getMempoolTxs
    :: (Default (ChgAccum conf), MonadIO m)
    => Mempool conf rawtx
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
    :: forall conf xs (c :: * -> Constraint) txtypes rawtx .
    ( HasBExceptions conf [
          StatePException
        , CSMappendException
        ]
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    )
    => (rawtx -> SomeData (ProofNExp conf rawtx) (Both (MempoolTx txtypes xs) c))
    -> Validator conf txtypes
    -> MempoolConfig conf (Both (MempoolTx txtypes xs) c) rawtx
defaultMempoolConfig expander validator = MempoolConfig handler
  where
    handler :: rawtx -> SomeStateTxHandler conf (Both (MempoolTx txtypes xs) c) rawtx
    handler rawtx = usingSomeData (expander rawtx) $ SomeData . StateTxHandler . processTx rawtx

    processTx
        :: forall txtype . MempoolTx txtypes xs txtype
        => rawtx
        -> ProofNExp conf rawtx txtype
        -> RwMempoolAct conf (TxComponents txtype) rawtx (StateTx txtype)
    processTx rawtx prfNexp = do
        tx@StateTx{..} <- liftERoComp (expandOneTx prfNexp rawtx)
        liftERoComp $ runValidator validator tx
        liftERoComp (modifyAccumOne @_ @conf txBody) >>= modify . flip sett
        pure tx
