{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Hetero.HMap where

import           Universum hiding (show)

import           Data.Default (Default (..))
import           Data.Kind
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Data.Vinyl (Rec (..), rappend, rcast)
import           Data.Vinyl.Lens (RElem, RSubset)
import           Data.Vinyl.TypeLevel (type (++), AllConstrained, Fst, RImage, RIndex, Snd)
import           Formatting (bprint, build, (%))
import           Snowdrop.Util (IsEmpty (..), toDummyMap)
import           Snowdrop.Hetero.Constraints (NotIntersect, RemoveElem, UnionTypes)

------------------------
-- HMap and HSet
------------------------

-- Basics

type family HKeyVal (t :: u) :: (*, *)
type HKey t = Fst (HKeyVal t)
type HVal t = Snd (HKeyVal t)

class Ord (HKey t) => OrdHKey t
instance Ord (HKey t) => OrdHKey t

class (Ord (HKey t), Show (HKey t), Buildable (HKey t), Typeable (HKey t)) => ExnHKey t
instance (Ord (HKey t), Show (HKey t), Buildable (HKey t), Typeable (HKey t)) => ExnHKey t

-- HMap
newtype HMapEl (t :: u) = HMapEl {unHMapEl :: Map (HKey t) (HVal t)}
type HMap = Rec HMapEl

instance IsEmpty (HMapEl t) where
    isEmpty = M.null . unHMapEl

instance Default (HMapEl t) where
    def = HMapEl M.empty

instance OrdHKey t => Semigroup (HMapEl t) where
    HMapEl a <> HMapEl b = HMapEl $ a <> b

instance OrdHKey t => Monoid (HMapEl t) where
    a `mappend` b = a <> b
    mempty = HMapEl mempty

hmapEl :: OrdHKey t => [(HKey t, HVal t)] -> HMapEl t
hmapEl = HMapEl . M.fromList

hmapFromMap :: OrdHKey t => Map (HKey t) (HVal t) -> HMap '[t]
hmapFromMap = (:& RNil) . HMapEl

hmapToMap :: OrdHKey t => HMap '[t] -> Map (HKey t) (HVal t)
hmapToMap (HMapEl x :& RNil) = x


-- HSet
newtype HSetEl (t :: u) = HSetEl {unHSetEl :: Set (HKey t)}
type HSet = Rec HSetEl

instance IsEmpty (HSetEl t) where
    isEmpty = S.null . unHSetEl

instance Default (HSetEl t) where
    def = HSetEl S.empty

hsetEl :: OrdHKey t  => [HKey t] -> HSetEl t
hsetEl = HSetEl . S.fromList

hsetFromSet :: OrdHKey t => Set (HKey t) -> HSet '[t]
hsetFromSet = (:& RNil) . HSetEl

hsetToSet :: OrdHKey t => HSet '[t] -> Set (HKey t)
hsetToSet (HSetEl x :& RNil) = x

hmapToHSet :: HMap xs -> HSet xs
hmapToHSet RNil             = RNil
hmapToHSet (HMapEl x :& xs) = HSetEl (M.keysSet x) :& hmapToHSet xs

------------------------
-- Instances
------------------------

instance Default (Rec f '[]) where
    def = RNil

instance (Default (f x), Default (Rec f xs')) => Default (Rec f (x ': xs')) where
    def = def :& def

instance Buildable (Rec f '[]) where
    build _ = bprint "RNil"

instance (Buildable (f x), Buildable (Rec f xs')) => Buildable (Rec f (x ': xs')) where
    build (x :& xs) = bprint (build%" :& "%build) x xs

------------------------
-- HRemoveElement
------------------------

-- | If @t@ belongs to the list of types @xs@, @hremoveElem@ methods returns pair
-- from @Just@ with corresponding element and rest of the list.
-- Otherwise it returns @Nothing@ and unchanged list.
class HRemoveElem (t :: k) (xs :: [k]) where
    hremoveElem :: Rec f xs -> (Maybe (f t), Rec f (RemoveElem t xs))

instance HRemoveElem t '[] where
    hremoveElem _ = (Nothing, RNil)

instance HRemoveElem t (t ': xs') where
    hremoveElem (x :& xs) = (Just x, xs)

instance {-# OVERLAPPABLE #-}
    ( RemoveElem t (x ': xs') ~ (x ': RemoveElem t xs')
    , HRemoveElem t xs')
    => HRemoveElem t (x ': xs') where
    hremoveElem (x :& xs') = (x :&) <$> hremoveElem xs'

------------------------
-- Castable
------------------------

-- @xs@ must be subset of @res@, i.e. xs ⊆ res
class HUpCastable f xs res where
    hupcast :: Rec f xs -> Rec f res

instance HUpCastable f '[] '[] where
    hupcast RNil = RNil

instance ( Default (f t)
         , HUpCastable f (RemoveElem t xs) res'
         , HRemoveElem t xs
         )
         => HUpCastable f xs (t ': res') where
    hupcast xs = case hremoveElem xs of
        (Nothing, rest) -> def :& hupcast @f @(RemoveElem t xs) @res' rest
        (Just rx, rest) -> rx :& hupcast @f @(RemoveElem t xs) @res' rest

type HUpCastableMap = HUpCastable HMapEl
type HUpCastableSet = HUpCastable HSetEl

class (HRemoveElem t xs, OrdHKey t) => HCast xs t
instance (HRemoveElem t xs, OrdHKey t) => HCast xs t
type HCastable f res xs = (AllConstrained (HCast xs) res, Default (Rec f res))

-- | Takes xs ∩ res components from the passed Rec
-- and fill res \ xs components by def value.
-- So xs \ res components are ignored.
castStrip
    :: forall xs res f . HCastable f res xs
    => Rec f xs -> Rec f res
castStrip = impl def
  where
    impl :: forall rs . (AllConstrained (HCast xs) rs) => Rec f rs -> Rec f xs -> Rec f rs
    impl RNil _         = RNil
    impl rs@(_ :& _) xs = impl1 rs xs

    impl1 :: forall rs r rs' . (rs ~ (r ': rs'), AllConstrained (HCast xs) rs)
          => Rec f rs -> Rec f xs -> Rec f rs
    impl1 (rdef :& rs') xs = case hremoveElem xs of
        (Just rx, _) -> rx :& impl @rs' rs' xs
        (Nothing, _) -> rdef :& impl @rs' rs' xs


-- @xs@ must be superset of @res@, i.e. xs ⊇ res
type HDownCastable xs res = RSubset res xs (RImage res xs)

hdowncast :: HDownCastable xs res => Rec f xs -> Rec f res
hdowncast = rcast

type HElem r rs = RElem r rs (RIndex r rs)

class HElem x xs => HElemFlipped xs x
instance HElem x xs => HElemFlipped xs x


------------------------
-- Intersection && Difference && Mappend
------------------------

class IntersectionF f g where
    intersectf :: OrdHKey t => f t -> g t -> f t

instance IntersectionF HMapEl HMapEl where
    intersectf (HMapEl a) (HMapEl b) = HMapEl $ a `M.intersection` b

instance IntersectionF HMapEl HSetEl where
    intersectf (HMapEl a) (HSetEl b) = HMapEl $ a `M.intersection` (toDummyMap b)

instance IntersectionF HSetEl HMapEl where
    intersectf (HSetEl a) (HMapEl b) = HSetEl $ a `S.intersection` (M.keysSet b)

type HIntersectable xs res = (HDownCastable xs res, AllConstrained OrdHKey res)

hintersect
    :: forall xs res f g . (IntersectionF f g, HIntersectable xs res)
    => Rec f xs -> Rec g res -> Rec f res
hintersect a b = hdowncast a `hintersect'` b
  where
    hintersect' :: AllConstrained OrdHKey res1 => Rec f res1 -> Rec g res1 -> Rec f res1
    hintersect' RNil RNil           = RNil
    hintersect' (x :& xs) (y :& ys) = (x `intersectf` y) :& (xs `hintersect'` ys)

-- Difference
class DifferenceF f g where
    differencef :: OrdHKey t => f t -> g t -> f t

instance DifferenceF HSetEl HMapEl where
    differencef (HSetEl a) (HMapEl b) = HSetEl $ a `S.difference` (M.keysSet b)

type HDifference xs res = (HDownCastable xs res, AllConstrained OrdHKey res)

hdifference
    :: forall xs res f g . (DifferenceF f g, HIntersectable xs res)
    => Rec f xs -> Rec g res -> Rec f res
hdifference a b = hdowncast a `hdiff` b
  where
    hdiff :: AllConstrained OrdHKey res1 => Rec f res1 -> Rec g res1 -> Rec f res1
    hdiff RNil RNil           = RNil
    hdiff (x :& xs) (y :& ys) = (x `differencef` y) :& (xs `hdiff` ys)

type HMappend f xs ys =
    ( HUpCastable f xs (UnionTypes xs ys)
    , HUpCastable f ys (UnionTypes xs ys)
    , Semigroup (Rec f (UnionTypes xs ys))
    )
type HMappendMap xs ys = HMappend HMapEl xs ys

hmappend
    :: forall xs ys f . HMappend f xs ys
    => Rec f xs -> Rec f ys -> Rec f (UnionTypes xs ys)
hmappend xs ys = hupcast xs <> hupcast ys

happend :: NotIntersect xs ys => Rec f xs -> Rec f ys -> Rec f (xs ++ ys)
happend = rappend

-- ------------------------
-- -- Utilities
-- ------------------------

rnull :: Rec f xs -> Bool
rnull RNil = True
rnull _    = False

rone :: f x -> Rec f '[x]
rone = (:& RNil)

unone :: Rec f '[x] -> f x
unone (x :& RNil) = x

rzipWithM
    :: forall f g h m . Applicative m
    => (forall x  .     f x  ->     g x  ->     m (h x))
    -> (forall xs . Rec f xs -> Rec g xs -> m (Rec h xs))
rzipWithM m = \r -> case r of
  RNil        -> \RNil        -> pure RNil
  (fa :& fas) -> \(ga :& gas) -> (:&) <$> (m fa ga) <*> rzipWithM m fas gas

------------------------
-- Examples
------------------------

data Component1
type instance HKeyVal Component1 = '(Int, Sum Int)

data Component2
type instance HKeyVal Component2 = '(Int, String)

data Component3
type instance HKeyVal Component3 = '(Int, Sum Word16)

type K1 = '[Component1, Component2]
type K2 = '[Component2, Component3]
type K1K2Union = '[Component1, Component2, Component3]

k1map :: HMap K1
k1map = hmapEl [(1, 1), (1, 3)] :& (hmapEl [(3, "aaa"), (4, "bbb")] :& RNil)

k2map :: HMap K2
k2map = hmapEl [(3, "ccc"), (5, "ddd")] :& (hmapEl [(3, 1), (4, 1)] :& RNil)

-- res1 :: HMap K1
-- res1 = gmappend @Identity @'[] @K1 GNil k1map

-- res2 :: HMap (UnionTypes K1 K2)
-- res2 = gmappend k1map k2map
