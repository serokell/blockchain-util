{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Core.Stuff
       ( StateModificationException(..)

       , UnitedTxType
       , Prf (..)
       ) where

import           Universum

import           Control.Lens (to)
import qualified Data.Text.Buildable
import           Data.Vinyl (Rec (..), type (∈), rget)
import           Formatting (bprint, build, stext, (%))

import           Snowdrop.Core.Transaction (TxComponents, TxProof)
import           Snowdrop.Util (HasGetter (..), UnionTypes)

------------------------
-- Compute undo
------------------------

data StateModificationException
    = forall id . (Buildable id, Show id) => UnexpectedKeyExists id
    | forall id . (Buildable id, Show id) => UnexpectedKeyAbsent id

deriving instance Show StateModificationException

instance Buildable StateModificationException where
    build = \case
        UnexpectedKeyExists i -> problemWithKey "exist" i
        UnexpectedKeyAbsent i -> problemWithKey "be absent" i
      where
        problemWithKey desc key =
            bprint ("Key "%build%" was not expected to "%stext%
                " during performed modification") key desc

------------------------
-- UnitedTxType
------------------------

data UnitedTxType (types :: [*])

type instance TxComponents (UnitedTxType '[x]) = TxComponents x
type instance TxComponents (UnitedTxType (x ': y ': xs)) = UnionTypes (TxComponents x) (TxComponents (UnitedTxType (y ': xs)))
type instance TxProof (UnitedTxType '[x]) = TxProof x

newtype Prf x = Prf {unPrf :: TxProof x} -- Gospodi kak je ya ustal ot etoi parashi
type instance TxProof (UnitedTxType (x ': y ': xs)) = Rec Prf (x ': y ': xs)

instance (prftype ~ TxProof x) => HasGetter (Prf x) prftype where
    gett = unPrf
instance (prftype ~ TxProof x, x ∈ xs) => HasGetter (Rec Prf xs) prftype where
    gett = gett . (rget @x)
    getterOf = to (gett . (rget @x)) -- to avoid some troubles with @x@, dunno
