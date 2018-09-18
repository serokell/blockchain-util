{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Util.Hetero.HMap where

import           Universum hiding (show)

import           Data.Default (Default (..))
import           Data.Kind
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Vinyl (Rec (..), rcast, rappend)
import           Data.Vinyl.Lens (RElem, RSubset)
import           Data.Vinyl.TypeLevel (Fst, RImage, RIndex, Snd, type (++))
import           Snowdrop.Util.Containers (toDummyMap)
import           Snowdrop.Util.Hetero.Constraints (NotIntersect, RecAll', RemoveElem, UnionTypes)

------------------------
-- HMap and HSet
------------------------

-- Basics

type family HKeyVal (t :: u) :: (*, *)
type HKey t = Fst (HKeyVal t)
type HVal t = Snd (HKeyVal t)

class Ord (HKey t) => OrdHKey t
instance Ord (HKey t) => OrdHKey t

class (Ord (HKey t), Show (HKey t), Buildable (HKey t)) => ExnHKey t
instance (Ord (HKey t), Show (HKey t), Buildable (HKey t)) => ExnHKey t
type AllExn xs = RecAll' xs ExnHKey

-- HMap
newtype HMapEl (t :: u) = HMapEl {unHMapEl :: Map (HKey t) (HVal t)}
type HMap = Rec HMapEl

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

instance Default (HSetEl t) where
    def = HSetEl S.empty

hsetEl :: OrdHKey t  => [HKey t] -> HSetEl t
hsetEl = HSetEl . S.fromList

hsetFromSet :: OrdHKey t => Set (HKey t) -> HSet '[t]
hsetFromSet = (:& RNil) . HSetEl

hsetToSet :: OrdHKey t => HSet '[t] -> Set (HKey t)
hsetToSet (HSetEl x :& RNil) = x

instance Default (Rec f '[]) where
    def = RNil

instance (Default (f x), Default (Rec f xs')) => Default (Rec f (x ': xs')) where
    def = def :& def

hmapToHSet :: HMap xs -> HSet xs
hmapToHSet RNil             = RNil
hmapToHSet (HMapEl x :& xs) = HSetEl (M.keysSet x) :& hmapToHSet xs

------------------------
-- HRemoveElement
------------------------

-- | If @t@ belongs to the list of types @xs@, @hremoveElem@ methods returns pair
-- from @Just@ with corresponding element and rest of the list.
-- Otherwise it returns @Nothing@ and unchanged list.
class HRemoveElem (t :: *) (xs :: [*]) where
    hremoveElem :: Rec f xs -> (Maybe (f t), Rec f (RemoveElem t xs))

instance HRemoveElem t '[] where
    hremoveElem _ = (Nothing, RNil)

instance HRemoveElem t (t ': xs') where
    hremoveElem (x :& xs) = (Just x, xs)

instance {-# OVERLAPPABLE #-}
    ( RemoveElem t (x ': xs') ~ (x ': RemoveElem t xs')
    , HRemoveElem t xs')
    => HRemoveElem t (x ': xs') where
    hremoveElem (x :& xs') = (x :&) <$> hremoveElem @t @xs' xs'

-- ------------------------
-- -- GElem
-- ------------------------

-- class GElem (t :: *) (xs :: [*]) where
--     elemAt :: GMap f xs -> (MapKV f t)

-- instance GElem t (t ': xs') where
--     elemAt (x :> _) = x

-- instance {-# OVERLAPPABLE #-}
--     GElem t xs' => GElem t (x ': xs') where
--     elemAt (_ :> gm) = elemAt @t @xs' gm

-- ------------------------
-- -- GMappendable
-- ------------------------

-- -- xs ++ ys \ xs
-- class GMappendable f xs ys where
--     gmappend :: GMap f xs -> GMap f ys -> GMap f (UnionTypes xs ys)

-- instance GMappendable f '[] ys where
--     gmappend _ ys = ys

-- instance ( Semigroup (GValF f t)
--          , Ord (GKey t)
--          , RemoveElem t ys
--          , GMappendable f xs (Rest t ys))
--          => GMappendable f (t : xs) ys where
--     gmappend (x :> xs) ys = case removeElem @t @ys ys of
--         (Nothing, rest) -> x :> gmappend xs rest
--         (Just y, rest)  -> (M.unionWith (<>) x y) :> gmappend xs rest

-- type GMappendableSet' xs = GMappendable (Const ()) xs
-- type GMappendableMap' xs = GMappendable Identity xs

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
    hupcast xs = case hremoveElem @t @xs xs of
        (Nothing, rest) -> def :& hupcast @f @(RemoveElem t xs) @res' rest
        (Just rx, rest) -> rx :& hupcast @f @(RemoveElem t xs) @res' rest

type HUpCastableMap = HUpCastable HMapEl
type HUpCastableSet = HUpCastable HSetEl

-- @xs@ must be superset of @res@, i.e. xs ⊇ res
type HDownCastable xs res = RSubset res xs (RImage res xs)

hdowncast :: HDownCastable xs res => Rec f xs -> Rec f res
hdowncast = rcast

type HElem r rs = RElem r rs (RIndex r rs)

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

type HIntersectable xs res = (HDownCastable xs res, RecAll' res OrdHKey)

hintersect
    :: forall xs res f g . (IntersectionF f g, HIntersectable xs res)
    => Rec f xs -> Rec g res -> Rec f res
hintersect a b = hdowncast a `hintersect'` b
  where
    hintersect' :: RecAll' res1 OrdHKey => Rec f res1 -> Rec g res1 -> Rec f res1
    hintersect' RNil RNil           = RNil
    hintersect' (x :& xs) (y :& ys) = (x `intersectf` y) :& (xs `hintersect'` ys)

-- Difference
class DifferenceF f g where
    differencef :: OrdHKey t => f t -> g t -> f t

instance DifferenceF HSetEl HMapEl where
    differencef (HSetEl a) (HMapEl b) = HSetEl $ a `S.difference` (M.keysSet b)

type HDifference xs res = (HDownCastable xs res, RecAll' res OrdHKey)

hdifference
    :: forall xs res f g . (DifferenceF f g, HIntersectable xs res)
    => Rec f xs -> Rec g res -> Rec f res
hdifference a b = hdowncast a `hdiff` b
  where
    hdiff :: RecAll' res1 OrdHKey => Rec f res1 -> Rec g res1 -> Rec f res1
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

-- gnull :: GMap f xs -> Bool
-- gnull GNil = True
-- gnull _ = False

-- -- gsetElemToSet :: Map k (Const () v) -> Set k
-- -- gsetElemToSet = M.keysSet

-- -- mapToGMapElem :: Map k v -> Map k (Identity v)
-- -- mapToGMapElem = fmap Identity

-- gsetFromSet :: forall t . Set (GKey t) -> GSet' '[t]
-- gsetFromSet s = fmap Const (toDummyMap s) :> GNil

-- gmapToMap :: forall t f . GMap f '[t] -> MapKV f t
-- gmapToMap (x :> GNil) = x

-- gmapToMap' :: forall t . GMap' '[t] -> Map (GKey t) (GVal t)
-- gmapToMap' (x :> GNil) = fmap runIdentity x

-- gmapFromMap :: forall t f . MapKV f t -> GMap f '[t]
-- gmapFromMap mp = mp :> GNil

-- class (Ord (GKey t), Show (GKey t), Buildable (GKey t)) => GKeyC t
-- instance (Ord (GKey t), Show (GKey t), Buildable (GKey t)) => GKeyC t

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
