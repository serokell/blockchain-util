{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.AVLp.Avl
       ( AllAvlEntries
       , IsAvlEntry
       , RootHashes
       , RootHashComp (..)
       , AvlProof (..)
       , AvlProofs

       , AvlHashable
       , KVConstraint
       , RootHash (..)
       , AvlUndo
       , saveAVL
       , mkAVL
       , avlRootHash
       ) where

import           Universum

import qualified Data.Tree.AVL as AVL
import           Data.Vinyl.Core (Rec (..))
import           Data.Vinyl.TypeLevel (AllConstrained)

import qualified Data.Text.Buildable as Buildable
import           Snowdrop.Dba.Base (DbApplyProofWrapper (..))
import           Snowdrop.Hetero (HKey, HVal)
type AvlHashable h = (Ord h, Show h, Typeable h)
type KVConstraint k v = (Typeable k, Ord k, Show k, Show v, Typeable v, Eq v)

newtype RootHash h = RootHash { unRootHash :: h }
  deriving (Eq, Show)

deriving instance Hashable h => Hashable (RootHash h)

newtype RootHashComp h t = RootHashComp {unRootHashComp :: RootHash h}
  deriving (Eq, Show)
type RootHashes h = Rec (RootHashComp h)

newtype AvlProof h t = AvlProof {unAvlProof :: AVL.Proof h (HKey t) (HVal t)}
type AvlProofs h = Rec (AvlProof h)

deriving instance (Show h, Show (HKey t), Show (HVal t)) => Buildable (AvlProof h t)

instance Buildable (AvlProofs h xs) => Buildable (DbApplyProofWrapper (AvlProofs h xs)) where
    build (DbApplyProofWrapper proofs) = Buildable.build proofs

class ( KVConstraint (HKey t) (HVal t)
      , AVL.Hash h (HKey t) (HVal t)
      ) => IsAvlEntry h t
instance ( KVConstraint (HKey t) (HVal t)
         , AVL.Hash h (HKey t) (HVal t)
         ) => IsAvlEntry h t
type AllAvlEntries h xs = AllConstrained (IsAvlEntry h) xs

type AvlUndo h = RootHashes h

saveAVL :: forall h k v m . (AVL.Stores h k v m, MonadCatch m) => AVL.Map h k v -> m (RootHash h)
saveAVL avl = avlRootHash <$> AVL.save avl

mkAVL :: RootHash h -> AVL.Map h k v
mkAVL = pure . unRootHash

avlRootHash :: AVL.Hash h k v => AVL.Map h k v -> RootHash h
avlRootHash = RootHash . AVL.rootHash

----------------------------------------------------------------------------
-- MapLayer instances
----------------------------------------------------------------------------

instance (Show h, Show k, Show v) => Buildable (AVL.Map h k v) where
    build = Buildable.build . AVL.showMap

instance (Show h, Show k, Show v) => Buildable (AVL.Proof h k v) where
    build (AVL.Proof tree) = Buildable.build tree
