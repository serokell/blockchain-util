{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Core.Transaction
       ( TxProof
       , TxComponents
       , TxRaw (..)
       , TxRawImpl

       , StateTx (..)
       , DownCastableTx
       , downcastStateTx
       -- , castStateTx

       , SomeTx
       , applySomeTx
       , usingSomeTx
       ) where

import           Universum

import           Snowdrop.Core.ChangeSet (HChangeSet)
import           Snowdrop.Hetero (HDownCastable, SomeData (..), hdowncast)
import           Snowdrop.Util (HasGetter (..))

------------------------------------------
-- Basic storage: model
------------------------------------------

-- | Determines Proof by txtype.
type family TxProof      (txtype :: k) :: *

-- | Determines components of HChangeSet by txtype.
type family TxComponents (txtype :: k) :: [*]

type family TxRawImpl (txtype :: k) :: *

newtype TxRaw txtype = TxRaw { unTxRaw :: TxRawImpl txtype }

deriving instance Hashable (TxRawImpl txtype) => Hashable (TxRaw txtype)

-- | Transaction which modifies state.
-- There is also RawTx, which is posted on the blockchain.
-- Ideally, RawStateTx and any action which modifies a state can be converted into StateStateTx.
data StateTx (txtype :: *) = StateTx
    { txProof :: TxProof txtype
    , txBody  :: HChangeSet (TxComponents txtype)
    } deriving (Generic)

instance (Hashable (HChangeSet (TxComponents txtype)), Hashable (TxProof txtype)) => Hashable (StateTx txtype)
deriving instance (Eq (HChangeSet (TxComponents txtype)), Eq (TxProof txtype)) => Eq (StateTx txtype)
deriving instance (Ord (HChangeSet (TxComponents txtype)), Ord (TxProof txtype)) => Ord (StateTx txtype)
deriving instance (Show (HChangeSet (TxComponents txtype)), Show (TxProof txtype)) => Show (StateTx txtype)

type DownCastableTx txtype1 txtype2 =
    ( HasGetter (TxProof txtype1) (TxProof txtype2)
    , HDownCastable (TxComponents txtype1) (TxComponents txtype2)
    )

downcastStateTx
    :: forall txtype1 txtype2 . DownCastableTx txtype1 txtype2
    => StateTx txtype1 -> StateTx txtype2
downcastStateTx StateTx {..} = StateTx (gett txProof) (hdowncast txBody)

-- castStateTx
--     :: forall id value txtype1 txtype2 .
--        (TxProof txtype1 -> Maybe (TxProof txtype2))
--     -> StateTx id value txtype1 -> Maybe (StateTx id value txtype2)
-- castStateTx castProof StateTx {..} = (\x -> StateTx x txBody) <$> castProof txProof

type SomeTx = SomeData StateTx

applySomeTx :: forall a c . (forall txtype . c txtype => StateTx txtype -> a) -> SomeTx c -> a
applySomeTx f (SomeData x) = f x

usingSomeTx :: forall a c . SomeTx c -> (forall txtype . c txtype => StateTx txtype -> a) -> a
usingSomeTx tx f = applySomeTx f tx

-- class HUpCastableChSet (TxComponents txtype) xs => UpCastableTxBody xs txtype
-- instance HUpCastableChSet (TxComponents txtype) xs => UpCastableTxBody xs txtype

-- mappendSomeTxBody
--     :: forall xs c .
--        HChangeSet xs
--     -> SomeTx c
--     -> Either CSMappendException (HChangeSet xs)
-- mappendSomeTxBody cs = applySomeTx (\StateTx{..} -> cs `mappendChangeSet` gupcast txBody)
