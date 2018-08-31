module Snowdrop.Util.Lens
       (
         HasGetter (..)
       , HasLens (..)
       ) where

import           Universum hiding (head, init, last)

import           Control.Lens (Getter, lens, to)

class HasGetter s a where
    {-# MINIMAL getterOf | gett #-}
    getterOf :: Getter s a
    getterOf = to gett

    gett :: HasGetter s a => s -> a
    gett = (^. getterOf)

class HasGetter s a => HasLens s a where
    {-# MINIMAL lensFor | sett #-}

    sett :: HasLens s a => s -> a -> s
    sett s b = s & (lensFor @s @a) .~ b

    lensFor :: HasLens s a => Lens s s a a
    lensFor = lens gett (sett @s @a)

instance HasGetter (NonEmpty a) a where
    gett (x :| _) = x

instance HasGetter a a where
    gett = id

instance HasLens a a where
    sett = flip const
