{-# LANGUAGE DeriveFunctor #-}

module Snowdrop.Core.ERoComp.Types
       (
         Prefix (..)
       , IdSumPrefixed (..)
       , StateR
       , StateP
       , FoldF (..)
       , ERoComp
       , ChgAccum
       , ChgAccumCtx (..)
       , DbAccess(..)
       , ChgAccumModifier (..)

       , foldFMappend
       ) where

import           Universum

import           Snowdrop.Core.BaseM (BaseM)

import           Snowdrop.Core.Undo
import           Snowdrop.Core.ChangeSet (CSMappendException (..), ChangeSet (..))
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))

type StateR id = Set id             -- ^Request of state
type StateP id value = Map id value -- ^Portion of state

data FoldF a res = forall b. FoldF (b, a -> b -> b, b -> res)

instance Functor (FoldF a) where
    fmap f (FoldF (e, foldf, applier)) = FoldF (e, foldf, f . applier)

foldFMappend :: (res -> res -> res) -> FoldF a res -> FoldF a res -> FoldF a res
foldFMappend resMappend (FoldF (e1, f1, applier1)) (FoldF (e2, f2, applier2))
    = FoldF (e, f, applier)
  where
    e = (e1, e2)
    f a (b1, b2) = (f1 a b1, f2 a b2)
    applier (b1, b2) = applier1 b1 `resMappend` applier2 b2

-- We can't define Monoid for FoldF a res because we can't define mempty (without undefined)
instance Semigroup res => Semigroup (FoldF a res) where
    f1 <> f2 = foldFMappend (<>) f1 f2

-- | Change accum modifier object.
-- Holds either change set or undo object which is to be applied to change accumulator.
data ChgAccumModifier id value undo
    = CAMChange { camChg  :: ChangeSet id value }
    | CAMRevert { camUndo :: Undo undo }

-- Datatype for access to database.
--
--     * mappend of two DbQuery executes as one DBQuery (as one Free iteration)
--     * mappend of DBIterator and x makes DBIterator execute one iteration of Free
--     and require one more for x
--
-- It's essential to understand that unlike DBQuery,
-- iterator always requires additional Free iteration.
-- Given iterators are to be used rarely, this shall be ok.
data DbAccess chgAccum id value res
    = DbQuery (StateR id) (StateP id value -> res)
    | DbIterator Prefix (FoldF (id, value) res)
    | DbModifyAccum
        chgAccum
        (ChgAccumModifier id value chgAccum)
        (Either (CSMappendException id) (chgAccum, Undo chgAccum) -> res)
    deriving (Functor)

-- | Reader computation which allows you to query for part of bigger state
-- and build computation considering returned result.
--
-- Note, response might contain more records than were requested in `req`
-- (because of monoidal gluing of many computations)

type family ChgAccum ctx :: *

type ERoComp e id value ctx = BaseM e (DbAccess (ChgAccum ctx) id value) ctx

data ChgAccumCtx ctx = CANotInitialized | CAInitialized (ChgAccum ctx)
