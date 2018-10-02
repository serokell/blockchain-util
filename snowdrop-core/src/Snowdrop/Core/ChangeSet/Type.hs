{-# LANGUAGE DeriveFunctor #-}

module Snowdrop.Core.ChangeSet.Type
       ( HChangeSet
       , HChangeSetEl (..)
       , hChangeSetToHSet
       , hChangeSetToHMap
       , hChangeSetFromMap
       , HUpCastableChSet
       , Undo (..)
       , AvlRevisions
       , AvlRevision (..)
       , UpCastUndo
       , upcastUndo
       , downcastUndo

       , MappendHChSet
       , mappendChangeSet
       , mconcatChangeSets
       , hChangeSetElToList
       , csRemove
       , csNew
       , csUpdate
       , CSMappendException (..)
       ) where

import           Universum hiding (head, init, last)

import           Data.Default (Default (..))
import qualified Data.Set as S
import qualified Data.ByteString as BS
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Recursive (rmap)

import qualified Data.Map.Strict as M

import           Snowdrop.Core.ChangeSet.ValueOp (ValueOp (..), ValueOpEx (..))
import           Snowdrop.Util

newtype HChangeSetEl t = HChangeSetEl {unHChangeSetEl :: Map (HKey t) (ValueOp (HVal t)) }

deriving instance (Show (HKey t), Show (ValueOp (HVal t))) => Show (HChangeSetEl t)

type HChangeSet = Rec HChangeSetEl

instance IntersectionF HChangeSetEl HMapEl where
    intersectf (HChangeSetEl a) (HMapEl b) = HChangeSetEl $ a `M.intersection` b

instance IntersectionF HChangeSetEl HSetEl where
    intersectf (HChangeSetEl a) (HSetEl b) = HChangeSetEl $ a `M.intersection` (toDummyMap b)

instance DifferenceF HSetEl HChangeSetEl where
    differencef (HSetEl a) (HChangeSetEl b) = HSetEl $ a `S.difference` (M.keysSet b)

instance Default (HChangeSetEl t) where
    def = HChangeSetEl M.empty

type HUpCastableChSet = HUpCastable HChangeSetEl

hChangeSetToHSet :: HChangeSet xs -> HSet xs
hChangeSetToHSet = rmap (HSetEl . M.keysSet . unHChangeSetEl)

hChangeSetToHMap :: HChangeSet xs -> HMap xs
hChangeSetToHMap = rmap hChangeSetElToMap

hChangeSetFromMap :: Map (HKey t) (ValueOp (HVal t)) -> HChangeSet '[t]
hChangeSetFromMap = (:& RNil) . HChangeSetEl

newtype AvlRevision t = AvlRevision {unAvlRevision :: ByteString}
    deriving (Eq, Ord, Show)

instance Default (AvlRevision t) where
    def = AvlRevision BS.empty
type AvlRevisions = Rec AvlRevision

data Undo xs = Undo
    { undoChangeSet :: HChangeSet xs
    , undoSnapshots :: AvlRevisions xs
    } deriving (Generic)

deriving instance (Show (HChangeSet xs), Show (AvlRevisions xs)) => Show (Undo xs)
deriving instance (Eq (HChangeSet xs), Eq (AvlRevisions xs)) => Eq (Undo xs)

type UpCastUndo xs supxs = (HUpCastableChSet xs supxs, HUpCastable AvlRevision xs supxs)

upcastUndo :: UpCastUndo xs supxs => Undo xs -> Undo supxs
upcastUndo (Undo cs bs) = Undo (hupcast cs) (hupcast bs)

downcastUndo :: HDownCastable xs res => Undo xs -> Undo res
downcastUndo (Undo cs bs) = Undo (hdowncast cs) (hdowncast bs)

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

hChangeSetElToMap :: HChangeSetEl t -> HMapEl t
hChangeSetElToMap = HMapEl . M.mapMaybe toJust . unHChangeSetEl
  where
    toJust Rem        = Nothing
    toJust NotExisted = Nothing
    toJust (Upd x)    = Just x
    toJust (New x)    = Just x

hChangeSetElToList :: HChangeSetEl t -> [(HKey t, ValueOp (HVal t))]
hChangeSetElToList = M.toList . unHChangeSetEl

data CSMappendException = forall id . (Show id, Eq id) => CSMappendException id

deriving instance Show CSMappendException
instance Exception CSMappendException

-- instance Buildable id => Buildable (CSMappendException id) where
--     build (CSMappendException i) =
--         bprint ("Failed to mappend ChangeSets due to conflict for key "%build) i

type MappendHChSet xs = AllExn xs

mappendChangeSet
    :: MappendHChSet xs
    => HChangeSet xs
    -> HChangeSet xs
    -> Either CSMappendException (HChangeSet xs)
mappendChangeSet RNil RNil = Right RNil
mappendChangeSet (x :& xs) (y :& ys) = case x `mappendChangeSetEl` y of
    Left e    -> Left e
    Right res -> (res :&) <$> mappendChangeSet xs ys

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
