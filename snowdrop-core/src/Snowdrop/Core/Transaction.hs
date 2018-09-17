{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Core.Transaction
       ( TxProof
       , TxComponents

       , StateTx (..)
       , StateTxWithUndo (..)
       , DownCastableTx
       , downcastStateTx
       -- , castStateTx

       , SomeTx
       , SomeTxWithUndo
       , applySomeTx
       ) where

import           Universum

import           Snowdrop.Core.ChangeSet (HChangeSet, Undo)
import           Snowdrop.Util (HDownCastable, hdowncast, HasGetter (..), SomeData (..))

------------------------------------------
-- Basic storage: model
------------------------------------------

-- | Determines Proof by txtype.
type family TxProof      (txtype :: k) :: *

  -- | Determines components of HChangeSet by txtype.
type family TxComponents (txtype :: k) :: [*]

-- | Transaction which modifies state.
-- There is also RawTx, which is posted on the blockchain.
-- Ideally, RawStateTx and any action which modifies a state can be converted into StateStateTx.
data StateTx (txtype :: *) = StateTx
    { txProof :: TxProof txtype
    , txBody  :: HChangeSet (TxComponents txtype)
    } deriving (Generic)

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

data StateTxWithUndo txtype = StateTxWithUndo
    { stateTx :: StateTx txtype
    , txUndo  :: Undo (TxComponents txtype)
    }
type SomeTxWithUndo = SomeData StateTxWithUndo

applySomeTx :: (forall txtype . c txtype => StateTx txtype -> a) -> SomeTx c -> a
applySomeTx f (SomeData x) = f x

-- class UpCastableGMap ValueOp (TxComponents txtype) xs => UpCastableTxBody xs txtype
-- instance UpCastableGMap ValueOp (TxComponents txtype) xs => UpCastableTxBody xs txtype

-- mappendSomeTxBody
--     :: forall xs c .
--        HChangeSet xs
--     -> SomeTx c
--     -> Either CSMappendException (HChangeSet xs)
-- mappendSomeTxBody cs = applySomeTx (\StateTx{..} -> cs `mappendChangeSet` gupcast txBody)
