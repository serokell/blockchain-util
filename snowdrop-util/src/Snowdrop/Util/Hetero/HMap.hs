{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeInType #-}

module Snowdrop.Util.Hetero.HMap where

import           Universum hiding (show)

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Kind
import           Data.Default (Default (..))
import           Data.Vinyl (Rec (..), RecSubset, rcast)
import           Data.Vinyl.TypeLevel (Fst, Snd)
import           Snowdrop.Util.Hetero.Constraints (type Rest)

------------------------
-- HMap and HSet
------------------------

type family HKeyVal (t :: u) :: (*, *)
type HKey t = Fst (HKeyVal t)
type HVal t = Snd (HKeyVal t)

newtype HMapEl (t :: u) = HMapEl {unHMapEl :: Map (HKey t) (HVal t)}
type HMap = Rec HMapEl

hmapElFromList :: Ord (HKey t) => [(HKey t, HVal t)] -> HMapEl t
hmapElFromList = HMapEl . M.fromList

newtype HSetEl (t :: u) = HSetEl {unHSetEl :: Set (HKey t)}
type HSet = Rec HSetEl

hsetElFromList :: Ord (HKey t) => [HKey t] -> HSetEl t
hsetElFromList = HSetEl . S.fromList

------------------------
-- HRemoveElement
------------------------

class HRemoveElem (t :: *) (xs :: [*]) where
    hremoveElem :: Rec f xs -> (Maybe (f t), Rec f (Rest t xs))

instance HRemoveElem t '[] where
    hremoveElem _ = (Nothing, RNil)

instance HRemoveElem t (t ': xs') where
    hremoveElem (x :& xs) = (Just x, xs)

instance {-# OVERLAPPABLE #-}
    ( Rest t (x ': xs') ~ (x ': Rest t xs')
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

-- gappend :: NotIntersect xs ys => GMap f xs -> GMap f ys -> GMap f (xs ++ ys)
-- gappend = gappend'
--   where
--     gappend' :: GMap f xs -> GMap f ys -> GMap f (xs ++ ys)
--     gappend' GNil ys = ys
--     gappend' (x :> xs) ys = x :> gappend' xs ys

-- type GMappendableSet' xs = GMappendable (Const ()) xs
-- type GMappendableMap' xs = GMappendable Identity xs

-- ------------------------
-- -- GIntersectable
-- ------------------------

-- -- GIntersectable doesn't impose restrictions on @xs@ and @res@.
-- -- If @xs@ isn't subset of @res@ then absent types in @res@ will be mempty.
-- class GIntersectable f res xs where
--     gintersect :: GMap f res -> GMap g xs -> GMap f xs

-- instance GIntersectable f res '[] where
--     gintersect _ GNil = GNil

-- instance (Ord (GKey t), GIntersectable f (Rest t res) xs', RemoveElem t res)
--          => GIntersectable f res (t ': xs') where
--     gintersect r (x :> xs) = case removeElem @t @res r of
--         (Nothing, rest) -> mempty @(MapKV f t) :> gintersect rest xs
--         (Just rx, rest) -> intersectMaps rx x :> gintersect rest xs

-- type GIntersectableMap' = GIntersectable Identity

-- gintersectMap :: GIntersectableMap' res xs => GMap' res -> GMap g xs -> GMap' xs
-- gintersectMap = gintersect @Identity

------------------------
-- Castable
------------------------

-- TODO UpCastable equalents to HasReview

-- @xs@ must be subset of @res@
class HUpCastable f xs res where
    hupcast :: Rec f xs -> Rec f res

instance HUpCastable f '[] '[] where
    hupcast RNil = RNil

instance ( Default (f t)
         , HUpCastable f (Rest t xs) res'
         , HRemoveElem t xs
         )
         => HUpCastable f xs (t ': res') where
    hupcast xs = case hremoveElem @t @xs xs of
        (Nothing, rest) -> def :& hupcast @f @(Rest t xs) @res' rest
        (Just rx, rest) -> rx :& hupcast @f @(Rest t xs) @res' rest

type HUpCastableMap xs res = HUpCastable HMapEl xs res
type HUpCastableSet xs res = HUpCastable HSetEl xs res

hupcastMap :: HUpCastableMap xs res => HMap xs -> HMap res
hupcastMap = hupcast

hupcastSet :: HUpCastableSet xs res => HSet xs -> HSet res
hupcastSet = hupcast

-- @xs@ must be superset of @res@
class HDownCastable f xs res where
    hdowncast :: Rec f xs -> Rec f res

instance RecSubset Rec res xs is => HDownCastable f xs res where
    hdowncast = rcast

type HDownCastableMap xs res = HDownCastable HMapEl xs res
type HDownCastableSet xs res = HDownCastable HSetEl xs res

gdowncastMap :: HDownCastableMap xs res => HMap xs -> HMap res
gdowncastMap = hdowncast

gdowncastSet :: HDownCastableSet xs res => HSet xs -> HSet res
gdowncastSet = hdowncast

-- ------------------------
-- -- Semigroup
-- ------------------------

-- type SemigroupGF f xs = (GMappendable f xs xs, UnionTypes xs xs ~ xs)

-- instance SemigroupGF f xs => Semigroup (GMap f xs) where
--     gm1 <> gm2 = gm1 `gmappend` gm2

-- instance Default (GMap f '[]) where
--     def = GNil

-- instance Default (GMap f xs') => Default (GMap f (t ': xs')) where
--     def = def @(MapKV f t) :> def

-- instance Monoid (GMap f '[]) where
--     mappend _ _ = GNil
--     mempty = GNil

-- instance (Ord (GKey t), SemigroupGF f (t ': xs'), Monoid (GMap f xs'))
--        => Monoid (GMap f (t ': xs')) where
--     mappend = (<>)
--     mempty = mempty @(MapKV f t) :> mempty

-- memptyGMap' :: Monoid (GMap' xs) => GMap' xs
-- memptyGMap' = mempty

-- memptyGSet' :: Monoid (GSet' xs) => GSet' xs
-- memptyGSet' = mempty

-- ------------------------
-- -- Utilities
-- ------------------------

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

-- ------------------------
-- -- Examples
-- ------------------------

-- data Component1
-- type instance HKeyVal Component1 = '(Int, Sum Int)

-- data Component2
-- type instance HKeyVal Component2 = '(Int, String)

-- data Component3
-- type instance HKeyVal Component3 = '(Int, Sum Word16)

-- type K1 = '[Component1, Component2]
-- type K2 = '[Component2, Component3]
-- type K1K2Union = '[Component1, Component2, Component3]

-- k1map :: HMap K1
-- k1map = M.fromList [(1, 1), (1, 3)] :> (M.fromList [(3, "aaa"), (4, "bbb")] :> GNil)

-- k2map :: HMap K2
-- k2map = M.fromList [(3, "ccc"), (5, "ddd")] :> (M.fromList [(3, 1), (4, 1)] :> GNil)

-- res1 :: HMap K1
-- res1 = gmappend @Identity @'[] @K1 GNil k1map

-- res2 :: HMap (UnionTypes K1 K2)
-- res2 = gmappend k1map k2map
