{-# LANGUAGE DeriveTraversable #-}
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
       , IsEmpty (..)
       ) where

import           Universum hiding (head, init, last)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Snowdrop.Util.Text (DBuildable)

newtype OldestFirst b a = OldestFirst { unOldestFirst :: b a }
deriving instance Foldable b => Foldable (OldestFirst b)
deriving instance Functor b => Functor (OldestFirst b)
deriving instance Applicative b => Applicative (OldestFirst b)
deriving instance Traversable b => Traversable (OldestFirst b)
deriving instance Eq (b a) => Eq (OldestFirst b a)
deriving instance Show (b a) => Show (OldestFirst b a)
deriving instance DBuildable (b a) => DBuildable (OldestFirst b a)
deriving instance Hashable (b a) => Hashable (OldestFirst b a)
deriving instance Semigroup (b a) => Semigroup (OldestFirst b a)
deriving instance Monoid (b a) => Monoid (OldestFirst b a)

newtype NewestFirst b a = NewestFirst { unNewestFirst :: b a }
deriving instance Foldable b => Foldable (NewestFirst b)
deriving instance Functor b => Functor (NewestFirst b)
deriving instance Applicative b => Applicative (NewestFirst b)
deriving instance Traversable b => Traversable (NewestFirst b)
deriving instance Eq (b a) => Eq (NewestFirst b a)
deriving instance Show (b a) => Show (NewestFirst b a)
deriving instance DBuildable (b a) => DBuildable (NewestFirst b a)
deriving instance Hashable (b a) => Hashable (NewestFirst b a)
deriving instance Semigroup (b a) => Semigroup (NewestFirst b a)
deriving instance Monoid (b a) => Monoid (NewestFirst b a)

instance Foldable b => Container (NewestFirst b a) where
  type Element (NewestFirst b a) = a

instance Foldable b => Container (OldestFirst b a) where
  type Element (OldestFirst b a) = a

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

class IsEmpty x where
  isEmpty :: x -> Bool

instance (Hashable i, Hashable v) => Hashable (Map i v) where
    hashWithSalt salt (M.toList -> a) = hashWithSalt salt a

instance Hashable v => Hashable (Set v) where
    hashWithSalt salt (S.toList -> a) = hashWithSalt salt a
