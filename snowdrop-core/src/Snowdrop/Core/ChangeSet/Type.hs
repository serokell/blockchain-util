{-# LANGUAGE DeriveFunctor #-}

module Snowdrop.Core.ChangeSet.Type
    (
      ChangeSet (..)
    , Undo (..)
    , csRemove
    , csNew
    , csUpdate
    , changeSetToMap
    , changeSetToList
    , CSMappendException (..)
    , mappendChangeSet
    , mconcatChangeSets
    , splitByPrefix
    , filterByPrefix
    , filterByPrefixPred
    , filterSetByPrefixPred
    , mapKeysMonotonicCS
    ) where

import           Data.Default (Default (def))
import           Formatting (bprint, build, (%))
import           Universum hiding (head, init, last)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable

import           Snowdrop.Core.ChangeSet.ValueOp (ValueOp (..), ValueOpEx (..))
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Util

newtype ChangeSet id v = ChangeSet {changeSet :: M.Map id (ValueOp v)}
    deriving (Functor, Eq, Ord, Show)

instance (Ord id, HasReview id id1, HasReview value value1)
      => HasReview (ChangeSet id value) (ChangeSet id1 value1) where
    inj (ChangeSet mp) = ChangeSet (inj mp)

instance (Ord id, Ord id1, HasPrism id id1, HasPrism value value1)
      => HasPrism (ChangeSet id value) (ChangeSet id1 value1) where
    proj (ChangeSet mp) = ChangeSet <$> proj mp

data Undo id value = Undo
  { undoChangeSet :: ChangeSet id value
  , undoSnapshot  :: ByteString
  }
  deriving (Show, Eq, Generic)

csRemove :: ChangeSet id v -> Set id
csRemove = M.keysSet . M.filter isRem . changeSet
  where
    isRem Rem = True
    isRem _   = False

csNew :: ChangeSet id v -> Map id v
csNew = M.mapMaybe isNew . changeSet
  where
    isNew (New x) = Just x
    isNew _       = Nothing

csUpdate :: ChangeSet id v -> Map id v
csUpdate = M.mapMaybe isUpd . changeSet
  where
    isUpd (Upd x) = Just x
    isUpd _       = Nothing

changeSetToMap :: ChangeSet id v -> Map id v
changeSetToMap = M.mapMaybe toJust . changeSet
  where
    toJust Rem        = Nothing
    toJust NotExisted = Nothing
    toJust (Upd x)    = Just x
    toJust (New x)    = Just x

changeSetToList :: ChangeSet id v -> [(id, ValueOp v)]
changeSetToList (ChangeSet mp) = M.toList mp

instance Default (ChangeSet id v) where
    def = ChangeSet def

newtype CSMappendException id = CSMappendException id
    deriving (Show, Eq)

instance (Show id, Typeable id) => Exception (CSMappendException id)

instance Buildable id => Buildable (CSMappendException id) where
    build (CSMappendException i) =
        bprint ("Failed to mappend ChangeSets due to conflict for key "%build) i

-- This tricky implementation works for O(min(N, M) * log(max(N, M)))
mappendChangeSet :: Ord id => ChangeSet id v -> ChangeSet id v -> Either (CSMappendException id) (ChangeSet id v)
mappendChangeSet (ChangeSet m1) (ChangeSet m2) =
    if leftAppRight then ChangeSet <$> M.foldrWithKey comb (Right m2) m1
    else ChangeSet <$> M.foldrWithKey comb (Right m1) m2
  where
    leftAppRight = M.size m1 < M.size m2

    comb _ _ e@(Left _) = e
    comb i v (Right m) = case M.lookup i m of
        Nothing -> Right $ M.insert i v m
        Just op ->
            let resOp = if leftAppRight then Op v <> Op op
                        else Op op <> Op v
            in case resOp of
                Err   -> Left $ CSMappendException i
                Op ro -> Right $ M.insert i ro m

mconcatChangeSets :: Ord id => [ChangeSet id v] -> Either (CSMappendException id) (ChangeSet id v)
mconcatChangeSets = foldM mappendChangeSet def

splitByPrefix :: forall id v . (IdSumPrefixed id, Ord id) => ChangeSet id v -> M.Map Prefix (ChangeSet id v)
splitByPrefix (ChangeSet c) = M.foldrWithKey f mempty c
  where
    f i v = M.alter (alterF i v) (idSumPrefix i)

    alterF i csVal Nothing   = Just $ ChangeSet $ M.singleton i csVal
    alterF i csVal (Just cs) = Just $ ChangeSet $ M.insert i csVal $ changeSet cs

filterByPrefix :: IdSumPrefixed id => Prefix -> ChangeSet id v -> ChangeSet id v
filterByPrefix p = filterByPrefixPred (== p)

filterByPrefixPred :: IdSumPrefixed id => (Prefix -> Bool) -> ChangeSet id v -> ChangeSet id v
filterByPrefixPred predicate = ChangeSet . M.filterWithKey (curry (predicate . idSumPrefix . fst)) . changeSet

filterSetByPrefixPred :: IdSumPrefixed id => (Prefix -> Bool) -> Set id -> Set id
filterSetByPrefixPred predicate mp = S.filter (predicate . idSumPrefix) mp

mapKeysMonotonicCS :: Ord id1 => (id -> id1) -> ChangeSet id val -> ChangeSet id1 val
mapKeysMonotonicCS f = ChangeSet . M.mapKeysMonotonic f . changeSet
