{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeInType #-}

module Snowdrop.Util.Hetero.Constraints where

import           Universum hiding (show)

import           Data.Kind
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.Lens (RElem)
import           Data.Vinyl.TypeLevel (RIndex)

import           GHC.TypeLits (ErrorMessage (..), TypeError)

-- Aux type level fuctions and constraints

-- | Project the first component of a type-level tuple.
type family Fst a where Fst '(x,y) = x
-- | Project the second component of a type-level tuple.
type family Snd a where Snd '(x,y) = y
type family Snd' f a where Snd' f '(x,y) = f y

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

type family Rest (t :: k) (xs :: [k]) where
    Rest t '[] = '[]
    Rest t (t:xs') = xs'
    Rest t (x:xs') = x ': Rest t xs'

type family RecAll' (rs :: [u]) (c :: u -> Constraint) :: Constraint where
    RecAll' '[] c = ()
    RecAll' (r ': rs) c = (c r, RecAll' rs c)

-- xs ++ ys \ xs
type family UnionTypes xs ys :: [k] where
    UnionTypes '[] ys = ys
    UnionTypes (t : xs) ys = t ': UnionTypes xs (Rest t ys)

class RecToList f xs a where
    recToList :: Rec f xs -> [a]

instance RecToList f '[] a where
    recToList _ = []

instance (f x ~ a, RecToList f xs a) => RecToList f (x ': xs) a where
    recToList (fx :& xs) = fx : recToList xs

-- xs is superset of res
-- instance res ⊆ xs => HasGetter (Rec f xs) (Rec f res) where
--     gett = rcast

-- type DownCastableRec f xs res = HasGetter (Rec f xs) (Rec f res)

-- rdowncast :: DownCastableRec f xs res => Rec f xs -> Rec f res
-- rdowncast = gett

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

class RElem r rs (RIndex r rs) => RContains rs r
instance RElem r rs (RIndex r rs) => RContains rs r
