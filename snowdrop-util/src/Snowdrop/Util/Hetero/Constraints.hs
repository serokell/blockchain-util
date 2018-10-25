{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Util.Hetero.Constraints where

import           Universum hiding (Compose (..), Const (..), show)

import           Data.Hashable (Hashable (..))
import           Data.Kind
import           Data.Union (USubset, Union, absurdUnion, ulift, union, urelax)
import           Data.Vinyl (RPureConstrained (..), Rec (..))
import           Data.Vinyl.Functor (Compose (..), Lift (..))
import           Data.Vinyl.Lens (RElem)
import           Data.Vinyl.TypeLevel (AllConstrained, RImage, RIndex, RecAll)

import           GHC.TypeLits (ErrorMessage (..), TypeError)

import           Snowdrop.Util.Containers (IsEmpty (..))
import           Snowdrop.Util.Lens (HasGetter (..))


instance Hashable (Rec f '[]) where
    hashWithSalt i RNil = i

instance (Hashable (Rec f xs), Hashable (f x)) => Hashable (Rec f (x ': xs)) where
    hashWithSalt i (f :& fs) = hashWithSalt (hashWithSalt i f) fs

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

instance HasGetter (Union f '[]) a where
    gett = absurdUnion

instance (HasGetter (Union f xs) (SomeData f c), AllConstrained c (x : xs))
    => HasGetter (Union f (x : xs)) (SomeData f c) where
    gett = union gett SomeData

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

-- | A record of components @f r -> g r@ may be applied to a union of @f@ to
-- get a union of @g@.
class UApply rs where
    uapply
      :: Rec (Lift (->) f g) rs
      -> Union f rs
      -> Union g rs

instance UApply '[] where
    uapply RNil = absurdUnion
    {-# INLINE uapply #-}

instance (UApply xs, USubset xs (x ': xs) (RImage xs (x ': xs))) => UApply (x ': xs) where
    uapply (Lift f :& fs) = union (urelax . uapply fs) (ulift . f)
    {-# INLINE uapply #-}

type UHandler f g h = Lift (->) f (Compose h g)

toUHandler
  :: forall t f g h .
     (f t -> h (g t))
  -> UHandler f g h t
toUHandler f = Lift (Compose . f)

-- | A record of components @f r -> h (g r)@ may be applied to a union of @f@ to
-- get a union of @g@.
class UTraverse rs where
    utraverse
      :: Functor h
      => Rec (UHandler f g h) rs
      -> Union f rs
      -> h (Union g rs)

instance UTraverse '[] where
    utraverse RNil = absurdUnion
    {-# INLINE utraverse #-}

instance (UTraverse xs, USubset xs (x ': xs) (RImage xs (x ': xs))) => UTraverse (x ': xs) where
    utraverse (Lift f :& fs) = union (fmap urelax . utraverse fs) (fmap ulift . getCompose . f)
    {-# INLINE utraverse #-}

rliftA2
  :: forall c f g h ts .
    RPureConstrained c ts
  => (forall t . c t => f t -> g t -> h t)
  -> Rec (Lift (->) f (Lift (->) g h)) ts
rliftA2 f = rpureConstrained @c (Lift $ Lift . f)

rliftA
  :: forall c f g ts .
    RPureConstrained c ts
  => (forall t . c t => f t -> g t)
  -> Rec (Lift (->) f g) ts
rliftA f = rpureConstrained @c (Lift f)

