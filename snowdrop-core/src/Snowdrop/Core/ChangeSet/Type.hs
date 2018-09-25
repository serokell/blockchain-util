{-# LANGUAGE DeriveFunctor #-}

-- | Description of ChangeSet and basic functions to work with it.

module Snowdrop.Core.ChangeSet.Type
       ( ChangeSet (..)
       , csRemove
       , csNew
       , csUpdate
       , changeSetToMap
       , changeSetToList
       , CSMappendException (..)
       , mappendChangeSet
       , mconcatChangeSets
       , filterByPrefix
       , filterByPrefixPred
       , filterSetByPrefixPred
       , diffChangeSet
       ) where

import           Universum hiding (head, init, last)

import           Data.Default (Default (def))
import           Formatting (bprint, build, (%))

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable

import           Snowdrop.Core.ChangeSet.ValueOp (ValueOp (..), ValueOpEx (..), (<->))
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Util

-- | ChangeSet is a basic datatype. It reflects changes over a key-value storage.
-- ChangeSet holds Map from a key in database to an operation
-- which should be performed over the state by this key.
-- For more information see Snowdrop.Core.ChangeSet.ValueOp.
newtype ChangeSet id v = ChangeSet {changeSet :: M.Map id (ValueOp v)}
    deriving (Functor, Eq, Ord, Show)

instance Default (ChangeSet id v) where
    def = ChangeSet def

instance (Ord id, HasReview id id1, HasReview value value1)
      => HasReview (ChangeSet id value) (ChangeSet id1 value1) where
    inj (ChangeSet mp) = ChangeSet (inj mp)

instance (Ord id, Ord id1, HasPrism id id1, HasPrism value value1)
      => HasPrism (ChangeSet id value) (ChangeSet id1 value1) where
    proj (ChangeSet mp) = ChangeSet <$> proj mp

-- | Returns a set of keys which Rem operation should be applied to.
csRemove :: ChangeSet id v -> Set id
csRemove = M.keysSet . M.filter isRem . changeSet
  where
    isRem Rem = True
    isRem _   = False

-- | Returns a map containg keys which New operation should be applied to
-- and corresponding new values according to ChangeSet.
csNew :: ChangeSet id v -> Map id v
csNew = M.mapMaybe isNew . changeSet
  where
    isNew (New x) = Just x
    isNew _       = Nothing

-- | Returns a map containg keys which Upd operation should be applied to
-- and corresponding new values according to ChangeSet.
csUpdate :: ChangeSet id v -> Map id v
csUpdate = M.mapMaybe isUpd . changeSet
  where
    isUpd (Upd x) = Just x
    isUpd _       = Nothing

-- | Returns map consisted of keys corresponding to New and Upd operations and
-- values from operations themselves.
changeSetToMap :: ChangeSet id v -> Map id v
changeSetToMap = M.mapMaybe toJust . changeSet
  where
    toJust Rem        = Nothing
    toJust NotExisted = Nothing
    toJust (Upd x)    = Just x
    toJust (New x)    = Just x

-- | Like @changeSetToMap@ but returns a list.
changeSetToList :: ChangeSet id v -> [(id, ValueOp v)]
changeSetToList (ChangeSet mp) = M.toList mp

-- | Exception throwing from @mappendChangeSet@.
newtype CSMappendException id = CSMappendException id
    deriving (Show, Eq)

instance (Show id, Typeable id) => Exception (CSMappendException id)

instance Buildable id => Buildable (CSMappendException id) where
    build (CSMappendException i) =
        bprint ("Failed to mappend ChangeSets due to conflict for key "%build) i

-- | Combine @ValueOp@s corresponding the same keys.
-- @CSMappendException@ will be returned if after an aplication of
-- the first ChangeSet to a state it won't be possible to apply the second ChangeSet
-- without violation ValueOp's invariant.
-- Don't apply any changes to state itself.
-- This implementation works for O(min(N, M) * log(max(N, M)))
mappendChangeSet
    :: Ord id
    => ChangeSet id v
    -> ChangeSet id v
    -> Either (CSMappendException id) (ChangeSet id v)
mappendChangeSet (ChangeSet m1) (ChangeSet m2) = if leftAppRight
    then ChangeSet <$> M.foldrWithKey comb (Right m2) m1
    else ChangeSet <$> M.foldrWithKey comb (Right m1) m2
  where
    leftAppRight = M.size m1 < M.size m2

    comb _ _ e@(Left _) = e
    comb i v (Right m) = case M.lookup i m of
        Nothing -> Right $ M.insert i v m
        Just op ->
            let resOp = if leftAppRight
                        then Op v <> Op op
                        else Op op <> Op v
            in case resOp of
                Err   -> Left $ CSMappendException i
                Op ro -> Right $ M.insert i ro m

-- | Like @mconcatChangeSet@ but for list of ChangeSet.
mconcatChangeSets :: Ord id => [ChangeSet id v] -> Either (CSMappendException id) (ChangeSet id v)
mconcatChangeSets = foldM mappendChangeSet def

-- | Returns only those entries where key's prefix satisfies the predicate.
filterByPrefixPred :: IdSumPrefixed id => (Prefix -> Bool) -> ChangeSet id v -> ChangeSet id v
filterByPrefixPred predicate
    = ChangeSet . M.filterWithKey (curry (predicate . idSumPrefix . fst)) . changeSet

-- | Returns only those entries where key's prefix equals to the passed prefix.
filterByPrefix :: IdSumPrefixed id => Prefix -> ChangeSet id v -> ChangeSet id v
filterByPrefix p = filterByPrefixPred (== p)

-- | Returns only those elements where key's prefix satisfies the predicate.
filterSetByPrefixPred :: IdSumPrefixed id => (Prefix -> Bool) -> Set id -> Set id
filterSetByPrefixPred predicate mp = S.filter (predicate . idSumPrefix) mp


-- | Calculates diff of two changesets, namely
-- @c `diffChangeSet` a = Right b@ iff @a `mappendChangeSet` b = Right c@
diffChangeSet
    :: Ord id
    => ChangeSet id v
    -> ChangeSet id v
    -> Either (CSMappendException id) (ChangeSet id v)
diffChangeSet (ChangeSet c) (ChangeSet a) = do
    let err :: id -> Either (CSMappendException id) a
        err = Left . CSMappendException
        processKey b (k, aV) = do
          bV <- case k `M.lookup` c of
            Just cV -> maybe (err k) pure (cV <-> aV)
            _       -> err k
          pure $ M.insert k bV b
    b' <- foldM processKey mempty (M.toList a)
    pure $ ChangeSet $ (c M.\\ a) <> b'

