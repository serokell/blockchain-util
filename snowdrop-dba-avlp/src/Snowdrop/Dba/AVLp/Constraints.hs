{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.AVLp.Constraints where

import           Data.Vinyl
import           Universum

import qualified Data.Tree.AVL as AVL
import           Snowdrop.Hetero (HKey, HVal)

class AVL.Hash h (HKey x) (HVal x) => HAvlHash h x
instance AVL.Hash h (HKey x) (HVal x) => HAvlHash h x

class RMapWithC (c :: (* -> Constraint)) rs where
    rmapWithC :: (forall x. c x => f x -> g x) -> Rec f rs -> Rec g rs

instance RMapWithC h '[] where
  rmapWithC _ RNil = RNil
  {-# INLINE rmapWithC #-}

instance (c x, RMapWithC c xs) => RMapWithC c (x ': xs) where
    rmapWithC f (x :& xs) = f x :& rmapWithC @c f xs
    {-# INLINE rmapWithC #-}

-- Although RHashable definition using RMapWithC is more abstract, it leads to
-- worse error messages
class RHashable h rs where
    rmapWithHash :: (forall x. HAvlHash h x => f x -> g x) -> Rec f rs -> Rec g rs

instance RHashable h '[] where
    rmapWithHash _ RNil = RNil
    {-# INLINE rmapWithHash #-}

instance (HAvlHash h x, RHashable h xs) => RHashable h (x ': xs) where
    rmapWithHash f (x :& xs) = f x :& rmapWithHash @h f xs
    {-# INLINE rmapWithHash #-}
