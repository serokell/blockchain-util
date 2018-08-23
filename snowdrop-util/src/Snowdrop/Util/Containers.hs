module Snowdrop.Util.Containers
    (
      OldestFirst (..)
    , NewestFirst (..)
    , oldestFirstFContainer
    , newestFirstFContainer
    , toDummyMap
    , toOldestFirst
    , mapSet
    , mapKeys
    ) where

import           Universum hiding (head, init, last)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype OldestFirst b a = OldestFirst { unOldestFirst :: b a }
deriving instance Foldable b => Foldable (OldestFirst b)
deriving instance Functor b => Functor (OldestFirst b)

newtype NewestFirst b a = NewestFirst { unNewestFirst :: b a }
deriving instance Foldable b => Foldable (NewestFirst b)
deriving instance Functor b => Functor (NewestFirst b)

oldestFirstFContainer :: (b a -> c a) -> OldestFirst b a -> OldestFirst c a
oldestFirstFContainer f (OldestFirst xs) = OldestFirst $ f xs

newestFirstFContainer :: (b a -> c a) -> NewestFirst b a -> NewestFirst c a
newestFirstFContainer f (NewestFirst xs) = NewestFirst $ f xs

toDummyMap :: Set a -> M.Map a ()
toDummyMap = M.fromSet (const ())

mapSet :: Ord id1 => (id -> id1) -> Set id -> Set id1
mapSet f = S.fromList . map f . S.toList

mapKeys :: Ord id => (a -> id) -> M.Map a b -> (M.Map id b)
mapKeys f = M.fromList . map (first f) . M.toList

toOldestFirst :: NewestFirst [] a -> OldestFirst [] a
toOldestFirst (NewestFirst xs) = OldestFirst $ reverse xs
