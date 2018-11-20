{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Core.Stuff
       ( UnitedTxType
       , Prf (..)
       ) where

import           Universum

import           Control.Lens (to)
import           Data.Vinyl (Rec (..), type (∈), rget)

import           Snowdrop.Core.Transaction (TxComponents, TxProof)
import           Snowdrop.Hetero (UnionTypes)
import           Snowdrop.Util (HasGetter (..))

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
