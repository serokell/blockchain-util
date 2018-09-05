module Snowdrop.Core.ChangeSet.ValueOp
    (
      ValueOp (..)
    , ValueOpEx (..)
    , ValueOpErr (..)
    ) where

import           Universum hiding (head, init, last)

import qualified Prelude
import           Snowdrop.Util

data ValueOp v
    = New v
    | Upd (v -> Either ValueOpErr v)
    | Rem
    | NotExisted
    deriving (Generic)

instance Prelude.Show v => Prelude.Show (ValueOp v) where
    show (New v)    = "New " <> show v
    show (Upd _)    = "Upd <function>"
    show Rem        = "Rem"
    show NotExisted = "NotExisted"

instance HasPrism v v1 => HasReview (ValueOp v) (ValueOp v1) where
    inj (Upd f)    = Upd $ (fmap inj . f) <=< (maybe (Left BasicErr) Right . proj)
    inj (New x)    = New (inj x)
    inj Rem        = Rem
    inj NotExisted = NotExisted

instance HasPrism v v1 => HasPrism (ValueOp v) (ValueOp v1) where
    proj Rem        = Just Rem
    proj NotExisted = Just NotExisted
    proj (New v)    = New <$> proj v
    proj (Upd f)    = Just $ Upd $ (maybe (Left BasicErr) Right . proj) <=< (f . inj)

data ValueOpEx v
    = Op (ValueOp v)
    | Err ValueOpErr
    deriving (Show)

data ValueOpErr
    = ValueOpErr Text
    | BasicErr
    deriving (Eq, Show)

instance Exception ValueOpErr where
  displayException (ValueOpErr t) = show t
  displayException BasicErr       = "BasicErr occured"

instance Semigroup (ValueOpEx v) where
    Err e <> _ = Err e
    _ <> Err e = Err e

    Op NotExisted <> Op (Upd _) = Err BasicErr
    Op NotExisted <> Op (New v) = Op (New v)
    Op NotExisted <> Op Rem     = Err BasicErr

    Op (Upd _) <> Op NotExisted    = Err BasicErr
    Op (New _) <> Op NotExisted    = Err BasicErr
    Op Rem     <> Op NotExisted    = Op Rem
    Op NotExisted <> Op NotExisted = Op NotExisted

    Op Rem <> Op (New x) = Op $ New x -- NOTE: @id: is it ok?
    Op Rem <> Op Rem     = Err BasicErr
    Op Rem <> Op (Upd _) = Err BasicErr

    Op (New _) <> Op (New _) = Err BasicErr
    Op (New _) <> Op Rem     = Op NotExisted
    Op (New _) <> Op (Upd x) = Op $ Upd x -- NOTE: @id: is it ok?

    Op (Upd _) <> Op (New _) = Err BasicErr
    Op (Upd _) <> Op Rem     = Op Rem
    Op (Upd f1) <> Op (Upd f2) = Op $ Upd $ f2 <=< f1

