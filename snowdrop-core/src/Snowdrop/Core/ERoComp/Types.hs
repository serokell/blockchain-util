{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types    #-}

-- | Basic types for Exceptionable Read-Only Computation.

module Snowdrop.Core.ERoComp.Types
       ( ChgAccum
       , Undo
       , BException
       , Ctx

       , FoldF (..)
       , DbAccess (..)
       , ERoComp

       , DbAccessM (..)
       , ERoCompM

       , DbAccessU (..)
       , ERoCompU

       , foldFMappend
       ) where

import           Universum

import           Data.Vinyl.Core (Rec)

import           Snowdrop.Core.BaseM (BaseM)

import           Snowdrop.Core.ChangeSet (CSMappendException (..), HChangeSet, ValueOp)
import           Snowdrop.Hetero (HKey, HMap, HSet, HVal)
import           Snowdrop.Util (NewestFirst, OldestFirst)

------------------------
-- DbAccess
------------------------


-- | Change Accumulator is in-memory state of snowdrop's functions.
-- Most likely it should be consisnted with persistent state because it's just
-- one more layer of state which must be considered during access to state.
-- Also it has to be possible to apply ChangeSet to this Change Accumulator.
type family ChgAccum conf :: *
type family Undo conf :: *
type family BException conf :: *
type family Ctx conf :: *

-- | Datatype describing read only interface for access to a state:
--
-- * Simple query, iteration
--
-- Type variables @id@, @value@ describe types of key and value of key-value storage.
-- Variable @res@ denotes result of 'DbAccess' execution.
data DbAccess conf (components :: [*]) (res :: *)
    = DbQuery (HSet components) (HMap ValueOp components -> res)
    -- ^ Request to state.
    -- The first field is request set of keys which are requested from state.
    -- The second one is a callback which accepts result of request: Map key value
    -- and returns a continuation (a next request to state).
    | forall t . DbIterator (forall f . Rec f components -> f t) (FoldF (HKey t, ValueOp (HVal t)) res)
    -- ^ Iteration over state.
    -- The first field is prefix for iteration over keys with this prefix.
    -- The second one is used to accumulate entries during iteration.

-- | Datatype describing read only interface for access to a state:
--
-- * Simple query, iteration
-- * Ability to convert sequence of change sets to sequence of change accumulators
--
-- Monad in which 'DbAccessM' is executed is expected to provide method
-- to use change accumulator in order to alter accesses done with 'DbAccess'.
--
-- Type variables @id@, @value@ describe types of key and value of key-value storage,
-- @chgAccum@ is in-memory accumulator of changes.
-- Variable @res@ denotes result of 'DbAccessM' execution.
data DbAccessM (conf :: *) (components :: [*]) (res :: *)
    = DbModifyAccum
        (OldestFirst [] (HChangeSet components))
        (Either CSMappendException (OldestFirst [] (ChgAccum conf)) -> res)
    -- ^ Operation to construct sequence of @chgAccum@ change accumulators
    -- from a sequence of change sets.
    -- Resulting sequence can later be applied to current state
    -- (and modify it in the way corresponding change set prescribes).
    -- This action doesn't necessarily imply an explicit access to state
    -- (and might be a pure operation, e.g. for storage types that
    -- use 'SumChangeSet' as change accumulator).
    -- Some storage types though do require an access to internal state (e.g. AVL+ storage)
    -- due to more complicated structure of their respective change accumulator.
    --
    -- Operation's name "modify accumulator" refers to the fact that
    -- 'DbAccessM' is typically executed in monad with read access to internal
    -- change accumulator which modifies the actual state. To perform the
    -- 'DbModifyAccum' operation underlying monad is required to read this
    -- internal change accumulator and apply sequence of change sets to it.
    | DbAccess (DbAccess conf components res)
    -- ^ Object for simple access to state (query, iteration).

-- | Datatype describing read only interface for access to a state:
--
-- * Simple query, iteration
-- * Ability to convert sequence of change sets to sequence of change accumulators
-- * Ability to compute undo for a given change accumulator
-- * Ability to convert sequence of undo objects to a change accumulator
--
-- Type variables @id@, @value@ describe types of key and value of key-value storage,
-- @chgAccum@ is in-memory accumulator of changes,
-- @undo@ is an object allowing to revert changes proposed by some @chgAccum@.
-- Variable @res@ denotes result of 'DbAccessU' execution.
data DbAccessU (conf :: *) (components :: [*]) (res :: *)
    = DbModifyAccumUndo
        (NewestFirst [] (Undo conf))
        (Either CSMappendException (ChgAccum conf) -> res)
    -- ^ Operation to construct change accumulator @chgAccum@
    -- from a sequence of undo objects.
    -- Resulting change accumulator can later be applied to current state
    -- (and application will effectively rollback changes as prescribed by
    -- sequence of undo objects).
    -- This action doesn't necessarily imply an explicit access to state
    -- (and might be a pure operation, e.g. for storage types that
    -- use 'SumChangeSet' as change accumulator).
    -- Some storage types though do require an access to internal state (e.g. AVL+ storage)
    -- due to more complicated structure of their respective change accumulator.
    --
    -- Operation's name "modify accumulator with undo" refers to the fact that
    -- 'DbAccessU' is typically executed in monad with read access to internal
    -- change accumulator which modifies the actual state. To perform the
    -- 'DbModifyAccumUndo' operation underlying monad is required to read this
    -- internal change accumulator and apply sequence of change sets to it.
    | DbComputeUndo
        (ChgAccum conf)
        (Either CSMappendException (Undo conf) -> res)
    -- ^ Operation to construct @undo@ object from a given
    -- change accumulator @chgAccum@.
    -- Resulting undo object can later be used to rollback state
    -- on which given @chgAccum@ was applied the last.
    --
    -- This action doesn't necessarily imply an explicit access to state
    -- (and might be a pure operation, e.g. for storage types that
    -- use 'SumChangeSet' as change accumulator).
    -- Some storage types though do require an access to internal state (e.g. AVL+ storage)
    -- due to more complicated structure of their respective change accumulator.
    | DbAccessM (DbAccessM conf components res)
    -- ^ Object for simple access to state (query, iteration)
    -- and change accumulator construction.

deriving instance Functor (DbAccess conf xs)
deriving instance Functor (DbAccessM conf xs)
deriving instance Functor (DbAccessU conf xs)

-- | FoldF holds functions which are intended to accumulate result of iteratio
-- over entries.
-- The first field is an initial value.
-- The second one is an accumulator.
-- The third one is convertor result of iteration to continuation (a next request to state).
data FoldF a res = forall b. FoldF (b, b -> a -> b, b -> res)

instance Functor (FoldF a) where
    fmap f (FoldF (e, foldf, applier)) = FoldF (e, foldf, f . applier)

-- | Mappend operation for two @FoldF@s.
foldFMappend :: (res -> res -> res) -> FoldF a res -> FoldF a res -> FoldF a res
foldFMappend resMappend (FoldF (e1, f1, applier1)) (FoldF (e2, f2, applier2)) = FoldF (e, f, applier)
  where
    e = (e1, e2)
    f (b1, b2) a = (f1 b1 a, f2 b2 a)
    applier (b1, b2) = applier1 b1 `resMappend` applier2 b2

-- It can't be defined Monoid instance for @FoldF@ because @mempty@ can't be defined.
instance Semigroup res => Semigroup (FoldF a res) where
    f1 <> f2 = foldFMappend (<>) f1 f2

-- | Reader computation which allows you to query for part of bigger state
-- and build computation considering returned result.
-- DbAccess is used as an effect of BaseM.
type ERoComp conf xs = BaseM (BException conf) (DbAccess conf xs) (Ctx conf)

type ERoCompM conf xs = BaseM (BException conf) (DbAccessM conf xs) (Ctx conf)
type ERoCompU conf xs = BaseM (BException conf) (DbAccessU conf xs) (Ctx conf)
