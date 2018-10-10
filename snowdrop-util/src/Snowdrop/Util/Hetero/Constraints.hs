{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Util.Hetero.Constraints where

import           Universum hiding (Const (..), show)

import           Data.Kind
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Lens (RElem)
import           Data.Vinyl.TypeLevel (RIndex, RecAll)

import           GHC.TypeLits (ErrorMessage (..), TypeError)

import           Snowdrop.Util.Containers (IsEmpty (..))

-- Aux type level fuctions and constraints

-- | Project the first component of a type-level tuple.
type family Fst a where Fst '(x,y) = x
-- | Project the second component of a type-level tuple.
type family Snd a where Snd '(x,y) = y
type family Snd' f a where Snd' f '(x,y) = f y
type family Head xs where Head (x ': xs') = x

type family Elem (a :: k) (xs::[k]) where
    Elem a '[] = TypeError ('Text "Type " ':<>: 'ShowType a ':<>: 'Text " doesn't belong to list")
    Elem a (a:xs) = (() :: Constraint)
    Elem a (_:xs) = Elem a xs

type family NotElem (a :: k) (xs::[k]) where
    NotElem a '[] = (() :: Constraint)
    NotElem a (a:xs) = TypeError ('Text "Type " ':<>: 'ShowType a ':<>: 'Text " belongs to list")
    NotElem a (_:xs) = NotElem a xs

type family Unique (xs::[*]) where
    Unique '[] = (()::Constraint)
    Unique (a:xs) = (NotElem a xs, Unique xs)

type family NotIntersect (xs::[k]) (ys::[k]) where
    NotIntersect '[] _ = (() :: Constraint)
    NotIntersect _ '[] = (() :: Constraint)
    NotIntersect (a:xs) ys = (NotElem a ys, NotIntersect xs ys)

-- | RemoveElem type family removes type from the list of types if it's presented.
-- Otherwise it leaves the list unchanged.
type family RemoveElem (t :: k) (xs :: [k]) where
    RemoveElem t '[] = '[]
    RemoveElem t (t:xs') = xs'
    RemoveElem t (x:xs') = x ': RemoveElem t xs'

type family RecAll' (rs :: [u]) (c :: u -> Constraint) :: Constraint where
    RecAll' '[] c = ()
    RecAll' (r ': rs) c = (c r, RecAll' rs c)

-- xs ++ ys \ xs
type family UnionTypes xs ys :: [k] where
    UnionTypes '[] ys = ys
    UnionTypes (t : xs) ys = t ': UnionTypes xs (RemoveElem t ys)

class RecToList f xs a where
    recToList :: Rec f xs -> [a]

instance RecToList f '[] a where
    recToList _ = []

instance (f x ~ a, RecToList f xs a) => RecToList f (x ': xs) a where
    recToList (fx :& xs) = fx : recToList xs

data SomeData (d :: * -> *) (c :: * -> Constraint) =
    forall txtype . c txtype => SomeData (d txtype)

applySomeData :: (forall txtype . c txtype => d txtype -> a) -> SomeData d c -> a
applySomeData f (SomeData x) = f x

usingSomeData :: SomeData d c -> (forall txtype . c txtype => d txtype -> a) -> a
usingSomeData sd f = applySomeData f sd

class (c1 x, c2 x) => Both c1 c2 x
instance (c1 x, c2 x) => Both c1 c2 x

type family CList (xs :: [* -> Constraint]) :: * -> Constraint where
    CList '[x] = x
    CList (x ': xs) = Both x (CList xs)

remConstraint :: SomeData d (Both c1 c2) -> SomeData d c2
remConstraint (SomeData x) = SomeData x

class RElem r rs (RIndex r rs) => RContains rs r
instance RElem r rs (RIndex r rs) => RContains rs r

rAllEmpty :: RecAll f rs IsEmpty => Rec f rs -> Bool
rAllEmpty RNil      = True
rAllEmpty (x :& xs) = isEmpty x && rAllEmpty xs
