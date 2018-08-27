{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Snowdrop.Core.ChangeSet.ValueOp
       (
         ValueOp (..)
       , ValueOpEx (..)
       ) where

import           Universum hiding (head, init, last)

import           Snowdrop.Util

data ValueOp v
    = New v
    | Upd v
    | Rem
    | NotExisted
    deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic)

instance HasReview v v1 => HasReview (ValueOp v) (ValueOp v1) where
    inj = fmap inj

instance HasPrism v v1 => HasPrism (ValueOp v) (ValueOp v1) where
    proj Rem        = Just Rem
    proj NotExisted = Just NotExisted
    proj (New v)    = New <$> proj v
    proj (Upd v)    = Upd <$> proj v

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
