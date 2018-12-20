{-# LANGUAGE DataKinds #-}

module Snowdrop.Block.Exec.BaseConf where

import           Universum

import           Control.Lens (makeLenses)
import           Data.Default (Default (..))
import           Data.Vinyl.TypeLevel (type (++))

import           Snowdrop.Block (BlkConfiguration, BlockRef, ForkVerificationException,
                                 IterationException)
import           Snowdrop.Core (CSMappendException, StatePException, TxConf, TxConfModifier,
                                applyTxModifier)
import           Snowdrop.Hetero (NotIntersect, UnionTypes)
import           Snowdrop.Util (ExecM)

data BaseConf blkType (txConfEl :: * -> * -> *) (txtypes :: [*]) (errs :: [*]) conf = BaseConf
    { _cTxConf     :: TxConf txConfEl txtypes conf
    , _cBlkConf    :: BlkConfiguration blkType
    , _cPrintState :: ExecM ()
    }

makeLenses ''BaseConf

data BaseConfModifier blkType (txConfEl :: * -> * -> *) (txtypes :: [*]) (errs :: [*]) conf = BaseConfModifier
    { _cTxConfModifier     :: TxConfModifier txConfEl txtypes conf
    , _cBlkConfModifier    :: BlkConfiguration blkType -> BlkConfiguration blkType
    , _cPrintStateModifier :: ExecM ()
    }

makeLenses ''BaseConfModifier

instance Default (BaseConfModifier blkType txConfEl '[] '[] conf) where
    def = BaseConfModifier def identity mempty

type BaseErrors blkRef = '[ForkVerificationException blkRef, IterationException blkRef, StatePException, CSMappendException]

mkBaseConf
    :: BlkConfiguration blkType
    -> ExecM ()
    -> BaseConf blkType txConfEl '[] (BaseErrors (BlockRef blkType)) conf
mkBaseConf = BaseConf def

applyBaseModifier
    :: NotIntersect txs1 txs2
    => BaseConf blkType txConfEl txs1 errs1 conf
    -> BaseConfModifier blkType txConfEl txs2 errs2 conf
    -> BaseConf blkType txConfEl (txs1 ++ txs2) (UnionTypes errs1 errs2) conf
applyBaseModifier (BaseConf txConf blkConf printState) (BaseConfModifier txConfMod blkConfMod printState') =
    BaseConf (txConf `applyTxModifier` txConfMod) (blkConfMod blkConf) (printState <> printState')
