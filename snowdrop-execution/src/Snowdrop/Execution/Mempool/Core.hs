{-# LANGUAGE DataKinds #-}

module Snowdrop.Execution.Mempool.Core
       ( Mempool
       , MempoolState (..)
       , StateTxHandler (..)
       , MempoolConfig (..)
       , msTxsL
       , RwMempoolAct
       -- , defaultMempoolConfig
       , actionWithMempool
       , createMempool
       , getMempoolTxs
       ) where

import           Universum

import           Control.Lens (lens)
import           Data.Default (Default (..))

import           Snowdrop.Core (ChgAccum, ERwComp, SomeTxWithUndo, StatePException,
                                StateTxWithUndo (..), TxComponents)
import           Snowdrop.Execution.DbActions (DbAccessActions)
import           Snowdrop.Execution.IOExecutor (IOCtx, runERwCompIO)
import           Snowdrop.Util

---------------------------
-- Core part
---------------------------

type RwMempoolAct e ctx c xs rawtx a =
    ERwComp e ctx (MempoolState c (ChgAccum ctx) rawtx) xs a

newtype StateTxHandler e ctx c rawtx txtype = StateTxHandler
    { getStateTxHandler :: RwMempoolAct e ctx c (TxComponents txtype) rawtx (StateTxWithUndo txtype)
    }
type SomeStateTxHandler e ctx c rawtx = SomeData (StateTxHandler e ctx c rawtx) c

newtype MempoolConfig e ctx c rawtx =
    MempoolConfig { mcProcessTx :: rawtx -> SomeStateTxHandler e ctx c rawtx }

data MempoolState (c :: * -> Constraint) chgAccum rawtx = MempoolState
    { msTxs      :: [(rawtx, SomeTxWithUndo c)]
    , msChgAccum :: chgAccum
    }

data Versioned t = Versioned
    { vsData    :: t
    , vsVersion :: Int
    }

newtype Mempool c chgAccum rawtx
    = Mempool { mempoolState :: TVar (Versioned (MempoolState c chgAccum rawtx)) }

instance HasGetter (MempoolState c chgAccum rawtx) chgAccum where
    gett = msChgAccum

instance HasLens (MempoolState c chgAccum rawtx) chgAccum where
    sett s x = s {msChgAccum = x}

msTxsL :: Lens' (MempoolState c chgAccum rawtx) [(rawtx, SomeTxWithUndo c)]
msTxsL = lens msTxs (\s x -> s {msTxs = x})

instance Default chgAccum => Default (MempoolState c chgAccum rawtx) where
    def = MempoolState def def

instance Default chgAccum => Default (Versioned (MempoolState c chgAccum rawtx)) where
    def = Versioned def 0

-- TODO this bullshit will be handled in future.
-- defaultMempoolConfig
--     :: forall e xs ctx c txtypes rawtx .
--     ( HasExceptions e [
--           StateModificationException
--         , StatePException
--         , CSMappendException
--         ]
--     , HasLens ctx (ChgAccumCtx ctx)
--     )
--     => (rawtx -> SomeData (ProofNExp e ctx rawtx) (RContains txtypes))
--     -> Validator e ctx txtypes
--     -> MempoolConfig e ctx (RContains txtypes) rawtx
-- defaultMempoolConfig expander validator = MempoolConfig handler
--   where
--     handler :: rawtx -> SomeStateTxHandler e ctx (RContains txtypes) rawtx
--     handler rawtx = SomeData $ StateTxHandler $ usingSomeData (expander rawtx) $ processTx rawtx
--       --   \(ProofNExp (prf, sexp)) -> handler' (ProofNExp (prf, sexp))
--       -- where
--       --   handler'
--       --       :: forall txtype .
--       --          ProofNExp e ctx rawtx txtype
--       --       -> RwMempoolAct e ctx (RContains txtypes) (TxComponents txtype) rawtx (StateTxWithUndo txtype)
--       --   handler' = processTx @txtype rawtx

--     processTx
--         :: forall txtype . RContains txtypes txtype
--         => rawtx
--         -> ProofNExp e ctx rawtx txtype
--         -> RwMempoolAct e ctx (RContains txtypes) (TxComponents txtype) rawtx (StateTxWithUndo txtype)
--     processTx prfNexp rawtx = do
--         tx@StateTx{..} <- liftERoComp (expandOneTx prfNexp rawtx)
--         liftERoComp $ runValidator validator tx
--         StateTxWithUndo tx <$> modifyRwCompChgAccum (CAMChange txBody)

actionWithMempool
    :: ( Show e, Typeable e, Default chgAccum
       , HasException e StatePException
       )
    => Mempool c chgAccum rawtx
    -> DbAccessActions chgAccum xs ExecM
    -> RwMempoolAct e (IOCtx chgAccum xs) c xs rawtx a
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
    if modified
    then pure res
    else actionWithMempool mem dbActs callback

createMempool :: (Default chgAccum, MonadIO m) => m (Mempool c chgAccum rawtx)
createMempool = Mempool <$> atomically (newTVar def)

getMempoolTxs
    :: (Default chgAccum, MonadIO m)
    => Mempool c chgAccum rawtx
    -> m [(rawtx, SomeTxWithUndo c)]
getMempoolTxs Mempool{..} = msTxs . vsData <$> atomically (readTVar mempoolState)
