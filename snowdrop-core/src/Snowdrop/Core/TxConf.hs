{-# LANGUAGE DataKinds #-}

module Snowdrop.Core.TxConf where

import           Universum

import           Control.Lens (makeLenses, makeWrapped)
import           Data.Default (Default (..))
import           Data.Vinyl (RMap (rmap), Rec (RNil), (<+>))
import           Data.Vinyl.TypeLevel (type (++))

import           Snowdrop.Core.Expand (SeqExp, SeqExpanders)
import           Snowdrop.Hetero (NotIntersect)

data TxConfEl conf txtype = TxConfEl
    { _cExpander  :: SeqExp conf txtype
    }

makeLenses ''TxConfEl

newtype TxConf txConfEl (txtypes :: [*]) conf = TxConf { unTxConf :: Rec (txConfEl conf) txtypes }
makeWrapped ''TxConf

newtype TxConfModifier txConfEl (txtypes :: [*]) conf = TxConfModifier (Rec (txConfEl conf) txtypes)
makeWrapped ''TxConfModifier

instance Default (TxConf txConfEl '[] conf) where
    def = TxConf RNil

instance Default (TxConfModifier txConfEl '[] conf) where
    def = TxConfModifier RNil

applyTxModifier
    :: NotIntersect ts1 ts2
    => TxConf txConfEl ts1 conf
    -> TxConfModifier txConfEl ts2 conf
    -> TxConf txConfEl (ts1 ++ ts2) conf
applyTxModifier (TxConf els) (TxConfModifier els') = TxConf (els <+> els')

extractExpander :: RMap txs => TxConf TxConfEl txs conf -> SeqExpanders conf txs
extractExpander = rmap _cExpander . unTxConf
