{-# LANGUAGE DeriveFunctor #-}

module Snowdrop.Core.ERoComp.Types
       ( FoldF (..)
       , foldFMappend
       , ERoComp
       , ChgAccum
       , DbAccess(..)
       , ChgAccumModifier (..)
       ) where

import           Universum

import           Snowdrop.Core.BaseM (BaseM)

import           Snowdrop.Core.ChangeSet (CSMappendException (..), HChangeSet, Undo)
import           Snowdrop.Util

------------------------
-- FoldF
------------------------

-- | FoldF holds functions which are intended to accumulate result of iteratio
-- over entries.
-- The first field is an initial value.
-- The second one is an accumulator.
-- The third one is convertor result of iteration to continuation (a next request to state).
data FoldF a res = forall b. FoldF (b, a -> b -> b, b -> res)

instance Functor (FoldF a) where
    fmap f (FoldF (e, foldf, applier)) = FoldF (e, foldf, f . applier)

-- | Mappend operation for two @FoldF@s.
foldFMappend :: (res -> res -> res) -> FoldF a res -> FoldF a res -> FoldF a res
foldFMappend resMappend (FoldF (e1, f1, applier1)) (FoldF (e2, f2, applier2)) = FoldF (e, f, applier)
  where
    e = (e1, e2)
    f a (b1, b2) = (f1 a b1, f2 a b2)
    applier (b1, b2) = applier1 b1 `resMappend` applier2 b2

-- It can't be defined Monoid instance for @FoldF@ because @mempty@ can't be defined.
instance Semigroup res => Semigroup (FoldF a res) where
    f1 <> f2 = foldFMappend (<>) f1 f2

------------------------
-- DbAccess
------------------------

-- | Datatype describing read only interface for access to a state.
-- By "access" it's meant any interation with any kind of external state,
-- including: in-memory, persistent etc.
-- @id@, @values@ describes types of key and value of key-value storage.
-- @chgAccum@ is in-memory state of snowdrop.
data DbAccess chgAccum (components :: [*]) (res :: *)
    = DbQuery (HSet components) (HMap components -> res)
    -- ^ Request to state.
    -- The first field is request set of keys which are requested from state.
    -- The second one is a callback which accepts result of request: Map key value
    -- and returns a continuation (a next request to state).

    -- @t@ must belong to @components@, but it's extremely hard to manage with existential quantifier
    -- so let's prohibit construction of DbAccess outside of this module
    -- | forall t . DbIterator (Proxy t) (FoldF (GKey t, GVal t) res)
    -- TODO manage with DbIterator
    | DbModifyAccum
        chgAccum
        (ChgAccumModifier components)
        (Either CSMappendException (chgAccum, Undo components) -> res)
    -- ^ Operation to modify Change Accumulator.
    -- This action doesn't imply an explicit access to state,
    -- but AVL tree requires this access. This operation is read only,
    -- so it doesn't affect passed @chgAccum@ and doesn't produce any changes id db.

deriving instance Functor (DbAccess chgAccum xs)

-- | Change accum modifier object.
-- Holds either change set or undo object which is to be applied to change accumulator.
data ChgAccumModifier xs
    = CAMChange { camChg  :: HChangeSet xs }
    | CAMRevert { camUndo :: Undo xs }

------------------------
-- ERoComp
------------------------

-- | Change Accumulator is in-memory state of snowdrop's functions.
-- Most likely it should be consisnted with persistent state because it's just
-- one more layer of state which must be considered during access to state.
-- Also it has to be possible to apply ChangeSet to this Change Accumulator.
type family ChgAccum ctx :: *

-- | Reader computation which allows you to query for part of bigger state
-- and build computation considering returned result.
-- DbAccess is used as an effect of BaseM.
type ERoComp e ctx xs = BaseM e (DbAccess (ChgAccum ctx) xs) ctx
