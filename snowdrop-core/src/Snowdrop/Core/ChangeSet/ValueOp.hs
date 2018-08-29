{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

-- | ValueOp primitive.

module Snowdrop.Core.ChangeSet.ValueOp
       (
         ValueOp (..)
       , ValueOpEx (..)
       ) where

import           Universum hiding (head, init, last)

import           Snowdrop.Util

-- | ValueOp datatype is an action which should be performed over an entry
-- stored in a key-value storage. Intended to be used in @ChangeSet@,
-- in Map Id (ValueOp Value).
data ValueOp v
    = New v
    -- ^ @New@ operation requires the following invariants (both of them):
    --   1. key which holds this ValueOp doesn't exist in a state,
    --   2. after aplication of entry (key, New v) to the state a value stored
    --      by @key@ is @v@.
    | Upd v
    -- ^ @Upd@ operation requires the following invariants (both of them):
    --   1. key which holds this ValueOp exists in a state,
    --   2. after aplication of entry (key, Upd v) to the state a value stored
    --      by @key@ is @v@.
    | Rem
    -- ^ @Rem@ operation requires the following invariants (both of them):
    --   1. key which holds this ValueOp exists in a state,
    --   2. after aplication of entry (key, Rem) to the state the @key@ doesn't
    -- exist in the state anymore.
    | NotExisted
    -- ^ @NotExisted@ operation requires the following invariant:
    -- key which holds this ValueOp doesn't exist in a state.
    -- This operation has no side effects with respect to the state.
    -- However, if invariant isn't hold an exception will be caused.
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic)

instance HasReview v v1 => HasReview (ValueOp v) (ValueOp v1) where
    inj = fmap inj

instance HasPrism v v1 => HasPrism (ValueOp v) (ValueOp v1) where
    proj Rem        = Just Rem
    proj NotExisted = Just NotExisted
    proj (New v)    = New <$> proj v
    proj (Upd v)    = Upd <$> proj v

-- | Auxiliary datatype to provide Semigroup instance for ValueOp.
-- ValueOp doesn't hold Semigroup laws, but it holds if there is
-- an additional constructor corresponding error during mappend.
data ValueOpEx v
    = Op (ValueOp v)
    | Err
    deriving (Functor, Show, Eq)

instance Semigroup (ValueOpEx v) where
    Err <> _ = Err
    _ <> Err = Err

    Op NotExisted <> Op (Upd _) = Err
    Op NotExisted <> Op (New v) = Op (New v)
    Op NotExisted <> Op Rem     = Err

    Op (Upd _) <> Op NotExisted    = Err
    Op (New _) <> Op NotExisted    = Err
    Op Rem     <> Op NotExisted    = Op Rem
    Op NotExisted <> Op NotExisted = Op NotExisted

    Op Rem <> Op (New x) = Op $ Upd x
    Op Rem <> Op Rem     = Err
    Op Rem <> Op (Upd _) = Err

    Op (New _) <> Op (New _) = Err
    Op (New _) <> Op Rem     = Op NotExisted
    Op (New _) <> Op (Upd x) = Op $ New x

    Op (Upd _) <> Op (New _) = Err
    Op (Upd _) <> Op Rem     = Op Rem
    Op (Upd _) <> Op (Upd y) = Op $ Upd y
