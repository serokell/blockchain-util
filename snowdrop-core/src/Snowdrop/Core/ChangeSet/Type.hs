{-# LANGUAGE DeriveFunctor #-}

-- | Description of ChangeSet and basic functions to work with it.

module Snowdrop.Core.ChangeSet.Type
       ( HChangeSet
       , HChangeSetEl (..)
       , hChangeSetToHSet
       , hChangeSetToHMap
       , hChangeSetFromMap
       , HUpCastableChSet

       , MappendHChSet
       , mappendChangeSet
       , mconcatChangeSets
       , hChangeSetElToList

       , csRemove
       , csNew
       , csUpdate
       , CSMappendException (..)

       , diffChangeSet
       ) where

import           Universum hiding (head, init, last)

import           Data.Default (Default (..))
import qualified Data.Set as S
import qualified Data.Text.Buildable as Buildable
import           Data.Typeable (cast)
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Recursive (rmap)
import           Formatting (bprint, shown, (%))

import qualified Data.Map.Strict as M

import           Snowdrop.Core.ChangeSet.ValueOp (ValueOp (..), ValueOpEx (..), (<->))
import           Snowdrop.Util

newtype HChangeSetEl t = HChangeSetEl {unHChangeSetEl :: Map (HKey t) (ValueOp (HVal t)) }
    deriving (Generic)

deriving instance (Show (HKey t), Show (ValueOp (HVal t))) => Show (HChangeSetEl t)
deriving instance (Eq (HKey t), Eq (ValueOp (HVal t))) => Eq (HChangeSetEl t)

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

-- | Returns a set of keys which Rem operation should be applied to.
csRemove :: HChangeSetEl t -> Set (HKey t)
csRemove = M.keysSet . M.filter isRem . unHChangeSetEl
  where
    isRem Rem = True
    isRem _   = False

-- | Returns a map containg keys which New operation should be applied to
-- and corresponding new values according to ChangeSet.
csNew :: HChangeSetEl t -> Map (HKey t) (HVal t)
csNew = M.mapMaybe isNew . unHChangeSetEl
  where
    isNew (New x) = Just x
    isNew _       = Nothing

-- | Returns a map containg keys which Upd operation should be applied to
-- and corresponding new values according to ChangeSet.
csUpdate :: HChangeSetEl t -> Map (HKey t) (HVal t)
csUpdate = M.mapMaybe isUpd . unHChangeSetEl
  where
    isUpd (Upd x) = Just x
    isUpd _       = Nothing

-- | Returns map consisted of keys corresponding to New and Upd operations and
-- values from operations themselves.
hChangeSetElToMap :: HChangeSetEl t -> HMapEl t
hChangeSetElToMap = HMapEl . M.mapMaybe toJust . unHChangeSetEl
  where
    toJust Rem        = Nothing
    toJust NotExisted = Nothing
    toJust (Upd x)    = Just x
    toJust (New x)    = Just x

-- | Like @hChangeSetElToMap@ but returns a list.
hChangeSetElToList :: HChangeSetEl t -> [(HKey t, ValueOp (HVal t))]
hChangeSetElToList = M.toList . unHChangeSetEl

-- | Exception throwing from @mappendChangeSet@.
data CSMappendException = forall id . (Show id, Eq id, Typeable id) => CSMappendException id

deriving instance Show CSMappendException
-- deriving instance Eq CSMappendException
instance Eq CSMappendException where
    (==) (CSMappendException a) (CSMappendException b) = case cast a of
        Nothing -> False
        Just b' -> b == b'

instance Exception CSMappendException

instance Buildable CSMappendException where
    build (CSMappendException i) =
        bprint ("Failed to mappend ChangeSets due to conflict for key "%shown) i

type MappendHChSet xs = RecAll' xs ExnHKey

-- | Combine @ValueOp@s corresponding the same keys.
-- @CSMappendException@ will be returned if after an aplication of
-- the first ChangeSet to a state it won't be possible to apply the second ChangeSet
-- without violation ValueOp's invariant.
-- Don't apply any changes to state itself.
-- This implementation works for O(min(N, M) * log(max(N, M)))
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

-- | Like @mconcatChangeSet@ but for list of ChangeSet.
mconcatChangeSets
  :: (MappendHChSet xs, Default (HChangeSet xs))
    => [HChangeSet xs] -> Either CSMappendException (HChangeSet xs)
mconcatChangeSets = foldM mappendChangeSet def

-- | Calculates diff of two changesets, namely
-- @c `diffChangeSet` a = Right b@ iff @a `mappendChangeSet` b = Right c@
diffChangeSet
    :: MappendHChSet xs
    => HChangeSet xs
    -> HChangeSet xs
    -> Either CSMappendException (HChangeSet xs)
diffChangeSet RNil RNil = Right RNil
diffChangeSet (x :& xs) (y :& ys) = do
    res <- x `diffChangeSetEl` y
    (res :&) <$> diffChangeSet xs ys

diffChangeSetEl
    :: forall t. ExnHKey t
    => HChangeSetEl t
    -> HChangeSetEl t
    -> Either CSMappendException (HChangeSetEl t)
diffChangeSetEl (HChangeSetEl c) (HChangeSetEl a) = do
    let err :: HKey t -> Either CSMappendException a
        err = Left . CSMappendException
        processKey b (k, aV) = do
          bV <- case k `M.lookup` c of
            Just cV -> maybe (err k) pure (cV <-> aV)
            _       -> err k
          pure $ M.insert k bV b
    b' <- foldM processKey mempty (M.toList a)
    pure $ HChangeSetEl $ (c M.\\ a) <> b'
