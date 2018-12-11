{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Core.Transaction
       ( TxComponents
       , TxRaw (..)
       , TxRawImpl

       , StateTx (..)
       , DownCastableTx
       , downcastStateTx
       -- , castStateTx

       , SomeTx
       , applySomeTx
       , usingSomeTx

       , UnionExpandersOuts
       , SeqExpanderComponents
       , ExpRestriction (..)
       ) where

import           Universum

import           Data.Union (UElem, Union, ulift)
import           Data.Vinyl.TypeLevel (RIndex)

import           Snowdrop.Core.ChangeSet (HChangeSet)
import           Snowdrop.Hetero (HDownCastable, SomeData (..), UnionTypes, hdowncast)
import           Snowdrop.Util (DBuildable (..), HasReview (..))

------------------------------------------
-- Basic storage: model
------------------------------------------

-- | Determines components of HChangeSet by txtype.
type TxComponents (txtype :: k) = UnionExpandersOuts (SeqExpanderComponents txtype)

type family TxRawImpl (txtype :: k) :: *

newtype TxRaw txtype = TxRaw { unTxRaw :: TxRawImpl txtype }

instance UElem x xs (RIndex x xs) => HasReview (Union TxRaw xs) (TxRaw x) where
    inj = ulift

deriving instance Show (TxRawImpl t) => Show (TxRaw t)
deriving instance Hashable (TxRawImpl t) => Hashable (TxRaw t)
deriving instance DBuildable (TxRawImpl t) => DBuildable (TxRaw t)

-- | Transaction which modifies state.
-- There is also RawTx, which is posted on the blockchain.
-- Ideally, RawStateTx and any action which modifies a state can be converted into StateStateTx.
data StateTx (txtype :: *) = StateTx
    { txBody  :: HChangeSet (TxComponents txtype)
    } deriving (Generic)

instance Hashable (HChangeSet (TxComponents txtype)) => Hashable (StateTx txtype)
deriving instance Eq (HChangeSet (TxComponents txtype)) => Eq (StateTx txtype)
deriving instance Ord (HChangeSet (TxComponents txtype)) => Ord (StateTx txtype)
deriving instance Show (HChangeSet (TxComponents txtype)) => Show (StateTx txtype)

type DownCastableTx txtype1 txtype2 =
    ( HDownCastable (TxComponents txtype1) (TxComponents txtype2)
    )

downcastStateTx
    :: forall txtype1 txtype2 . DownCastableTx txtype1 txtype2
    => StateTx txtype1 -> StateTx txtype2
downcastStateTx StateTx {..} = StateTx (hdowncast txBody)

type SomeTx = SomeData StateTx

applySomeTx :: forall a c . (forall txtype . c txtype => StateTx txtype -> a) -> SomeTx c -> a
applySomeTx f (SomeData x) = f x

usingSomeTx :: forall a c . SomeTx c -> (forall txtype . c txtype => StateTx txtype -> a) -> a
usingSomeTx tx f = applySomeTx f tx

------------------------------------------
-- Restrictions of expanders
------------------------------------------

-- This datatype to be intended to use as kind and constructor of types instead of pair
data ExpRestriction i o = ExRestriction i o -- different type and constructor names to avoid going crazy

-- This type family should be defined for each seq expander like
-- type instance SeqExpanderComponents DlgTx =
--                  '[ ExRestriction '[TxIn] '[UtxoComponent],
--                     ExRestriction '[DlgIssuer, DlgDelegate] '[DlgIssuerComponent, DlgDelegateComponent]
--                   ]
-- this SeqExpander contains two PreExpanders
type family SeqExpanderComponents (txtype :: k) :: [ExpRestriction [*] [*]]

-- These typeclass should be satisfied automatically if everything is correct
type family UnionExpandersOuts (restrictions :: [ExpRestriction [*] [*]]) where
    UnionExpandersOuts '[] = '[]
    UnionExpandersOuts ('ExRestriction _ o ': xs) = UnionTypes o (UnionExpandersOuts xs)
