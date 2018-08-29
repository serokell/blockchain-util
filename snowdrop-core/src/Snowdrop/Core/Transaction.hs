module Snowdrop.Core.Transaction
       ( SValue
       , HasKeyValue

       , StateTxType (..)
       , StateTx (..)
       , PartialStateTx (..)
       , mkPartialStateTx

       , selectKeyValueCS
       ) where

import           Universum

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Formatting (int)

import           Snowdrop.Core.ChangeSet.Type (ChangeSet (..))
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Util

------------------------------------------
-- Basic storage: model
------------------------------------------

type family SValue id :: *

type HasKeyValue id val id1 val1 = (HasPrism id id1, HasPrism val val1, SValue id1 ~ val1)

newtype StateTxType = StateTxType Int
    deriving (Eq, Ord, Show, Generic)

instance Buildable StateTxType where
    build (StateTxType no) = bprint ("<state tx type #"%int%">") no

-- | Transaction which modifies state.
-- There is also RawTx, which is posted on the blockchain.
-- Ideally, RawStateTx and any action which modifies a state can be converted into StateStateTx.
data StateTx id proof value = StateTx
    { txType  :: StateTxType
    , txProof :: proof
    , txBody  :: ChangeSet id value
    } deriving (Eq, Ord, Show, Generic)

-- | Transaction which modifies one part of state (with some Prefix).
-- ptxBody contains ids with the same prefix
data PartialStateTx id proof value = PartialStateTx
    { ptxProof :: proof
    , ptxBody  :: ChangeSet id value
    }

mkPartialStateTx
    :: forall id1 proof value . Ord id1
    => (id1 -> Prefix)
    -> proof
    -> ChangeSet id1 value
    -> PartialStateTx id1 proof value
mkPartialStateTx getPref prf cset@(ChangeSet cs) =
    let ks = S.toList $ M.keysSet cs in
    case ks of
        [] -> PartialStateTx prf cset
        ((getPref  -> pref) : _)
          | all ((== pref) . getPref) ks -> PartialStateTx prf cset
          | otherwise -> error "Prefixes of ids aren't the same"

-- Emulation of dependent type
-- Assumption: `txType` allows one to identify concrete types for `id`, `proof`, `value`
-- Validators try to do conversion from abstract type to particular types
-- Validator is associated with set of `txType`s
-- There shall be at least one validator for each `txType`

instance (Ord id, HasReview id id1, HasReview value value1, HasReview proof proof1)
      => HasReview (StateTx id proof value) (StateTx id1 proof1 value1) where
    inj StateTx{..} = StateTx txType (inj txProof) (inj txBody)

instance (Ord id, Ord id1, HasPrism id id1, HasPrism value value1, HasPrism proof proof1)
      => HasPrism (StateTx id proof value) (StateTx id1 proof1 value1) where
    proj StateTx{..} = StateTx txType <$> (proj txProof)  <*> (proj txBody)

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
