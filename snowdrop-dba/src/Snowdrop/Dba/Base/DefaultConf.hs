{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Snowdrop.Dba.Base.DefaultConf where

import           Universum

import           Data.Union (OpenUnion)

import           Snowdrop.Core (BException, ChgAccum, Ctx, Undo)
import           Snowdrop.Dba.Base.DbActions (DbApplyProof, DbComponents)
import           Snowdrop.Util (HasReview (..))


newtype Exceptions (es :: [*]) = Exceptions (OpenUnion es)
deriving instance Show (OpenUnion es) => Show (Exceptions es)
deriving instance Buildable (OpenUnion es) => Buildable (Exceptions es)
instance HasReview (OpenUnion es) e => HasReview (Exceptions es) e where
    inj = Exceptions . inj
instance (Typeable es, Show (OpenUnion es)) => Exception (Exceptions es)

data DefaultConf undo chgAccum (xs :: [*]) applyProof ctx (errs :: [*])

-- TODO Remove DbComponents, DbApplyProof type families and move to snowdrop-core

type instance BException (DefaultConf u c xs ap ctx errs) = Exceptions errs
type instance Undo (DefaultConf undo c xs ap ctx e) = undo
type instance ChgAccum (DefaultConf u chgAccum xs ap ctx e) = chgAccum
type instance DbComponents (DefaultConf u c xs ap ctx e) = xs
type instance DbApplyProof (DefaultConf u c xs applyProof ctx e) = applyProof
type instance Ctx (DefaultConf u c xs ap ctx e) = ctx

