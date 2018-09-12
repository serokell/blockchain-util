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

instance HasGetter (a, b) a where gett = fst
instance HasGetter (a, b) b where gett = snd

instance HasGetter (a, b, c) a where gett (a, _, _) = a
instance HasGetter (a, b, c) b where gett (_, b, _) = b
instance HasGetter (a, b, c) c where gett (_, _, c) = c

instance HasGetter (a, b, c, d) a where gett (a, _, _, _) = a
instance HasGetter (a, b, c, d) b where gett (_, b, _, _) = b
instance HasGetter (a, b, c, d) c where gett (_, _, c, _) = c
instance HasGetter (a, b, c, d) d where gett (_, _, _, d) = d

instance HasGetter (a, b, c, d, e) a where gett (a, _, _, _, _) = a
instance HasGetter (a, b, c, d, e) b where gett (_, b, _, _, _) = b
instance HasGetter (a, b, c, d, e) c where gett (_, _, c, _, _) = c
instance HasGetter (a, b, c, d, e) d where gett (_, _, _, d, _) = d
instance HasGetter (a, b, c, d, e) e where gett (_, _, _, _, e) = e
