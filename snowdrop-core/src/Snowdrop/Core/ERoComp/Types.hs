{-# LANGUAGE DeriveFunctor #-}

-- | Basic types for Exceptionable Read-Only Computation.

module Snowdrop.Core.ERoComp.Types
       ( Prefix (..)
       , IdSumPrefixed (..)
       , StateR
       , StateP
       , FoldF (..)
       , ChgAccum
       , ChgAccumCtx (..)

       , DbAccess (..)
       , ERoComp

       , DbAccessM (..)
       , ERoCompM

       , DbAccessU (..)
       , ERoCompU

       , foldFMappend
       ) where

import           Universum

import           Snowdrop.Core.BaseM (BaseM)

import           Snowdrop.Core.ChangeSet (CSMappendException (..), ChangeSet (..))
import           Snowdrop.Core.Prefix (IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Util (NewestFirst, OldestFirst)

-- | Set of requested keys from a key-value storage.
type StateR id = Set id

-- | Result of request.
type StateP id value = Map id value

-- | Datatype describing read only interface for access to a state.
-- By "access" it's meant any interation with any kind of external state,
-- including: in-memory, persistent etc.
-- @id@, @values@ describes types of key and value of key-value storage.
-- @chgAccum@ is in-memory state of snowdrop.
data DbAccess id value res
    = DbQuery (StateR id) (StateP id value -> res)
    -- ^ Request to state.
    -- The first field is request set of keys which are requested from state.
    -- The second one is a callback which accepts result of request: Map key value
    -- and returns a continuation (a next request to state).
    | DbIterator Prefix (FoldF (id, value) res)
    -- ^ Iteration over state.
    -- The first field is prefix for iteration over keys with this prefix.
    -- The second one is used to accumulate entries during iteration.

data DbAccessM chgAccum id value res
    = DbModifyAccum
        (OldestFirst [] (ChangeSet id value))
        (Either (CSMappendException id) (OldestFirst [] chgAccum) -> res)
    -- ^ Operation to modify Change Accumulator.
    -- This action doesn't imply an explicit access to state,
    -- but AVL tree requires this access. This operation is read only,
    -- so it doesn't affect passed @chgAccum@ and doesn't produce any changes id db.
    | DbAccess (DbAccess id value res)

data DbAccessU chgAccum undo id value res
    = DbModifyAccumUndo
        (NewestFirst [] undo)
        (Either (CSMappendException id) chgAccum -> res)
    | DbComputeUndo
        chgAccum
        (Either (CSMappendException id) undo -> res)
    | DbAccessM (DbAccessM chgAccum id value res)

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
foldFMappend resMappend (FoldF (e1, f1, applier1)) (FoldF (e2, f2, applier2))
    = FoldF (e, f, applier)
  where
    e = (e1, e2)
    f a (b1, b2) = (f1 a b1, f2 a b2)
    applier (b1, b2) = applier1 b1 `resMappend` applier2 b2

-- It can't be defined Monoid instance for @FoldF@ because @mempty@ can't be defined.
instance Semigroup res => Semigroup (FoldF a res) where
    f1 <> f2 = foldFMappend (<>) f1 f2

-- | Change Accumulator is in-memory state of snowdrop's functions.
-- Most likely it should be consisnted with persistent state because it's just
-- one more layer of state which must be considered during access to state.
-- Also it has to be possible to apply ChangeSet to this Change Accumulator.
type family ChgAccum ctx :: *

-- | Reader computation which allows you to query for part of bigger state
-- and build computation considering returned result.
-- DbAccess is used as an effect of BaseM.
type ERoComp e id value = BaseM e (DbAccess id value)

type ERoCompM e id value ctx = BaseM e (DbAccessM (ChgAccum ctx) id value) ctx
type ERoCompU e id value undo ctx = BaseM e (DbAccessU (ChgAccum ctx) undo id value) ctx

-- | Auxiliary datatype for context-dependant computations.
data ChgAccumCtx ctx = CANotInitialized | CAInitialized (ChgAccum ctx)
