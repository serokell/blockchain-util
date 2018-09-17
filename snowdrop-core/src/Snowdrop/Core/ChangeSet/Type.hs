{-# LANGUAGE DeriveFunctor #-}

module Snowdrop.Core.ChangeSet.Type
       ( HChangeSet
       , HChangeSetEl (..)
       , HUpCastableChSet
       , Undo (..)
       , upcastUndo
       , downcastUndo

       , mappendChangeSet
       , mconcatChangeSets
       , csRemove
       , csNew
       , csUpdate
       , hchangeSetElToMap
       , hchangeSetElToList
       , CSMappendException (..)
       ) where

import           Universum hiding (head, init, last)

import           Data.Default (Default (..))
import           Data.Vinyl (Rec (..))

import qualified Data.Map.Strict as M

import           Snowdrop.Core.ChangeSet.ValueOp (ValueOp (..), ValueOpEx (..))
import           Snowdrop.Util

newtype HChangeSetEl t = HChangeSetEl {unHChangeSetEl :: Map (HKey t) (ValueOp (HVal t)) }
type HChangeSet = Rec HChangeSetEl

instance Default (HChangeSetEl t) where
    def = HChangeSetEl M.empty

type HUpCastableChSet = HUpCastable HChangeSetEl

data Undo xs = Undo
    { undoChangeSet :: HChangeSet xs
    , undoSnapshot  :: ByteString
    } deriving (Generic)

deriving instance Show (HChangeSet xs) => Show (Undo xs)
deriving instance Eq (HChangeSet xs) => Eq (Undo xs)

upcastUndo :: HUpCastableChSet xs supxs => Undo xs -> Undo supxs
upcastUndo (Undo cs bs) = Undo (hupcast cs) bs

downcastUndo :: HDownCastable xs res => Undo xs -> Undo res
downcastUndo (Undo cs bs) = Undo (hdowncast cs) bs

csRemove :: HChangeSetEl t -> Set (HKey t)
csRemove = M.keysSet . M.filter isRem . unHChangeSetEl
  where
    isRem Rem = True
    isRem _   = False

csNew :: HChangeSetEl t -> Map (HKey t) (HVal t)
csNew = M.mapMaybe isNew . unHChangeSetEl
  where
    isNew (New x) = Just x
    isNew _       = Nothing

csUpdate :: HChangeSetEl t -> Map (HKey t) (HVal t)
csUpdate = M.mapMaybe isUpd . unHChangeSetEl
  where
    isUpd (Upd x) = Just x
    isUpd _       = Nothing

hchangeSetElToMap :: HChangeSetEl t -> Map (HKey t) (HVal t)
hchangeSetElToMap = M.mapMaybe toJust . unHChangeSetEl
  where
    toJust Rem        = Nothing
    toJust NotExisted = Nothing
    toJust (Upd x)    = Just x
    toJust (New x)    = Just x

hchangeSetElToList :: HChangeSetEl t -> [(HKey t, ValueOp (HVal t))]
hchangeSetElToList = M.toList . unHChangeSetEl

data CSMappendException = forall id . (Show id, Eq id) => CSMappendException id

deriving instance Show CSMappendException

-- instance (Show id, Typeable id) => Exception (CSMappendException id)

-- instance Buildable id => Buildable (CSMappendException id) where
--     build (CSMappendException i) =
--         bprint ("Failed to mappend ChangeSets due to conflict for key "%build) i

mappendChangeSet
    :: RecAll' xs ExnHKey
    => HChangeSet xs
    -> HChangeSet xs
    -> Either CSMappendException (HChangeSet xs)
mappendChangeSet RNil RNil = Right RNil
mappendChangeSet (x :& xs) (y :& ys) = case x `mappendChangeSetEl` y of
    Left e -> Left e
    Right res  -> (res :&) <$> mappendChangeSet xs ys

-- This tricky implementation works for O(min(N, M) * log(max(N, M)))
mappendChangeSetEl
    :: ExnHKey t
    => HChangeSetEl t
    -> HChangeSetEl t
    -> Either CSMappendException (HChangeSetEl t)
mappendChangeSetEl (HChangeSetEl m1) (HChangeSetEl m2) = if leftAppRight
    then HChangeSetEl <$> M.foldrWithKey comb (Right m2) m1
    else HChangeSetEl <$> M.foldrWithKey comb (Right m1) m2
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

mconcatChangeSets
    :: (RecAll' xs ExnHKey, Default (HChangeSet xs))
    => [HChangeSet xs] -> Either CSMappendException (HChangeSet xs)
mconcatChangeSets = foldM mappendChangeSet def

-- splitByPrefix
--     :: forall id v . (IdSumPrefixed id, Ord id)
--     => ChangeSet id v
--     -> M.Map Prefix (ChangeSet id v)
-- splitByPrefix (ChangeSet c) = M.foldrWithKey f mempty c
--   where
--     f i v = M.alter (alterF i v) (idSumPrefix i)

--     alterF i csVal Nothing   = Just $ ChangeSet $ M.singleton i csVal
--     alterF i csVal (Just cs) = Just $ ChangeSet $ M.insert i csVal $ changeSet cs

-- filterByPrefix :: IdSumPrefixed id => Prefix -> ChangeSet id v -> ChangeSet id v
-- filterByPrefix p = filterByPrefixPred (== p)

-- filterByPrefixPred :: IdSumPrefixed id => (Prefix -> Bool) -> ChangeSet id v -> ChangeSet id v
-- filterByPrefixPred predicate
--     = ChangeSet . M.filterWithKey (curry (predicate . idSumPrefix . fst)) . changeSet

-- filterSetByPrefixPred :: IdSumPrefixed id => (Prefix -> Bool) -> Set id -> Set id
-- filterSetByPrefixPred predicate mp = S.filter (predicate . idSumPrefix) mp

-- mapKeysMonotonicCS :: Ord id1 => (id -> id1) -> ChangeSet id val -> ChangeSet id1 val
-- mapKeysMonotonicCS f = ChangeSet . M.mapKeysMonotonic f . changeSet
