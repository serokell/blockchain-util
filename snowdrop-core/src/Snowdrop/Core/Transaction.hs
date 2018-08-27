{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE RankNTypes  #-}

module Snowdrop.Core.Transaction
    ( SValue
    , TxProof
    , UnitedTxType
    , HasKeyValue

    -- To suppress warnings
    , Prf (..)

    , StateTx (..)
    , PartialStateTx (..)
    , mkPartialStateTx

    , selectKeyValueCS

    , SomePartialTx
    , SomeTx
    , applySomeTx
    , applySomePartialTx
    , upcastStateTx
    , downcastStateTx
    , castStateTx
    ) where

import           Universum

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Vinyl
import           Control.Lens (to)

import           Snowdrop.Core.ChangeSet.Type (ChangeSet (..))
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Util

------------------------
-- UnitedTxType
------------------------

data UnitedTxType (types :: [*])

type instance TxProof (UnitedTxType '[x]) = TxProof x

newtype Prf x = Prf {unPrf :: TxProof x}

type instance TxProof (UnitedTxType (x ': y ': xs)) = Rec Prf (x ': y ': xs)

instance (prftype ~ TxProof x) => HasGetter (Prf x) prftype where
    gett = unPrf

instance (prftype ~ TxProof x, x ∈ xs) => HasGetter (Rec Prf xs) prftype where
    gett = gett . rget (Proxy @x)
    getterOf = to (gett . rget (Proxy @x)) -- to avoid some troubles with @x@, dunno

------------------------------------------
-- Basic storage: model
------------------------------------------

type family SValue id :: *

type family TxProof (txtype :: k) :: *

type HasKeyValue id val id1 val1 = (HasPrism id id1, HasPrism val val1, SValue id1 ~ val1)

-- | Transaction which modifies state.
-- There is also RawTx, which is posted on the blockchain.
-- Ideally, RawStateTx and any action which modifies a state can be converted into StateStateTx.
data StateTx id value txtype = StateTx
    { txProof :: TxProof txtype
    , txBody  :: ChangeSet id value
    }

type SomeTx id value = SomeData (StateTx id value)

applySomeTx :: (forall txtype . c txtype => StateTx id value txtype -> a) -> SomeTx id value c -> a
applySomeTx f (SomeData x) = f x

upcastStateTx
    :: forall id value txtype1 txtype2 . HasGetter (TxProof txtype1) (TxProof txtype2)
    => StateTx id value txtype1 -> StateTx id value txtype2
upcastStateTx StateTx {..} = StateTx (gett txProof) txBody

downcastStateTx
    :: forall id value txtype1 txtype2 . HasPrism (TxProof txtype1) (TxProof txtype2)
    => StateTx id value txtype1 -> Maybe (StateTx id value txtype2)
downcastStateTx StateTx {..} = (\x -> StateTx x txBody) <$> proj txProof

castStateTx
    :: forall id value txtype1 txtype2 .
       (TxProof txtype1 -> Maybe (TxProof txtype2))
    -> StateTx id value txtype1 -> Maybe (StateTx id value txtype2)
castStateTx castProof StateTx {..} = (\x -> StateTx x txBody) <$> castProof txProof

deriving instance (Eq   (TxProof txtype), Eq   id, Eq   value) => Eq   (StateTx id value txtype)
deriving instance (Ord  (TxProof txtype), Ord  id, Ord  value) => Ord  (StateTx id value txtype)
deriving instance (Show (TxProof txtype), Show id, Show value) => Show (StateTx id value txtype)
deriving instance Generic (StateTx id txtype value)

-- | Transaction which modifies one part of state (with some Prefix).
-- ptxBody contains ids with the same prefix
data PartialStateTx id value txtype = PartialStateTx
    { ptxProof :: TxProof txtype
    , ptxBody  :: ChangeSet id value
    }

type SomePartialTx id value = SomeData (PartialStateTx id value)

applySomePartialTx :: (forall txtype . c txtype => PartialStateTx id value txtype -> a) -> SomePartialTx id value c -> a
applySomePartialTx f (SomeData x) = f x

mkPartialStateTx
    :: forall id1 txtype value . Ord id1
    => (id1 -> Prefix)
    -> TxProof txtype
    -> ChangeSet id1 value
    -> PartialStateTx id1 value txtype
mkPartialStateTx getPref prf cset@(ChangeSet cs) =
    let ks = S.toList $ M.keysSet cs in
    case ks of
        [] -> PartialStateTx prf cset
        ((getPref  -> pref) : _)
          | all ((== pref) . getPref) ks -> PartialStateTx prf cset
          | otherwise -> error "Prefixes of ids aren't the same"

-- Emulation of dependent type
-- Assumption: `txtype` allows one to identify concrete types for `id`, `value`
-- Validators try to do conversion from abstract type to particular types
-- Validator is associated with set of `txType`s
-- There shall be at least one validator for each `txType`

instance (Ord id, HasReview id id1, HasReview value value1, HasReview (TxProof txtype) (TxProof txtype1))
      => HasReview (StateTx id value txtype) (StateTx id1 value1 txtype1) where
    inj StateTx{..} = StateTx (inj txProof) (inj txBody)

instance (Ord id, Ord id1, HasPrism id id1, HasPrism value value1, HasPrism (TxProof txtype) (TxProof txtype1))
      => HasPrism (StateTx id value txtype) (StateTx id1 value1 txtype1) where
    proj StateTx{..} = StateTx <$> (proj txProof) <*> (proj txBody)

selectKeyValueCS
    :: forall id1 value1 id value .
    ( HasKeyValue id value id1 value1
    , IdSumPrefixed id
    , Ord id1
    )
    => ChangeSet id value
    -> Either Prefix (ChangeSet id1 value1)
selectKeyValueCS (ChangeSet cs) = ChangeSet <$> M.foldrWithKey f (Right mempty) cs
  where
    f _ _ e@(Left _) = e
    f i valOp (Right c) = case proj @id @id1 i of
        Nothing  -> Right c
        Just id1 -> case sequenceA $ proj @value @value1 <$> valOp of
            Just nop -> Right $ M.insert id1 nop c
            Nothing  -> Left $ idSumPrefix i
