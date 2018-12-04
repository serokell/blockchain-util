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
       , materialize
       , mkAVL
       , avlRootHash
       , deserialiseM
       ) where

import           Universum

import           Control.Monad.Free (Free (Free))
import           Data.Tree.AVL (MapLayer (..))
import qualified Data.Tree.AVL as AVL
import           Data.Vinyl.Core (Rec (..))
import           Data.Vinyl.TypeLevel (AllConstrained)

import           Data.Default (Default (def))
import qualified Data.Text.Buildable as Buildable
import           Snowdrop.Dba.Base (DbActionsException (..), DbApplyProofWrapper (..))
import           Snowdrop.Hetero (HKey, HVal)

import           Snowdrop.Util (Serialisable (..))

type AvlHashable h = (Ord h, Show h, Typeable h, Serialisable h)
type KVConstraint k v = (Typeable k, Ord k, Show k,
                         Serialisable k, Serialisable v, Show v, Typeable v, Eq v)

newtype RootHash h = RootHash { unRootHash :: h }
  deriving (Eq, Serialisable, Show)

deriving instance Hashable h => Hashable (RootHash h)

newtype RootHashComp h t = RootHashComp {unRootHashComp :: RootHash h}
  deriving (Eq, Serialisable, Show)
type RootHashes h = Rec (RootHashComp h)

newtype AvlProof h t = AvlProof {unAvlProof :: AVL.Proof h (HKey t) (HVal t)}
type AvlProofs h = Rec (AvlProof h)

deriving instance (Show h, Show (HKey t), Show (HVal t)) => Buildable (AvlProof h t)

instance Buildable (AvlProofs h xs) => Buildable (DbApplyProofWrapper (AvlProofs h xs)) where
    build (DbApplyProofWrapper proofs) = Buildable.build proofs

class ( KVConstraint (HKey t) (HVal t)
      , Serialisable (MapLayer h (HKey t) (HVal t) h)
      , AVL.Hash h (HKey t) (HVal t)
      ) => IsAvlEntry h t
instance ( KVConstraint (HKey t) (HVal t)
      , Serialisable (MapLayer h (HKey t) (HVal t) h)
      , AVL.Hash h (HKey t) (HVal t)
      ) => IsAvlEntry h t
type AllAvlEntries h xs = AllConstrained (IsAvlEntry h) xs

type AvlUndo h = RootHashes h

saveAVL :: forall h k v m . (AVL.Stores h k v m, MonadCatch m) => AVL.Map h k v -> m (RootHash h)
saveAVL avl = avlRootHash <$> AVL.save avl

-- TODO: AVL no longer exports load
-- TODO: is this method even used somewhere?
materialize :: forall h k v m . AVL.Retrieves h k v m => AVL.Map h k v -> m (AVL.Map h k v)
materialize initAVL = flip AVL.loadAndM initAVL $ \case
    MLBranch rev h m c t l r -> fmap Free $ MLBranch rev h m c t <$> materialize l <*> materialize r
    rest -> pure $ Free rest

mkAVL :: RootHash h -> AVL.Map h k v
mkAVL = pure . unRootHash

-- | Get root hash of AVL tree.
-- TODO: in which cases it returns Nothing?
avlRootHash :: AVL.Hash h k v => AVL.Map h k v -> RootHash h
avlRootHash x = maybe (error "FIXME") RootHash (AVL.rootHash . AVL.getFreshlyRehashed . AVL.fullRehash $ x)

deserialiseM :: (MonadThrow m, Serialisable v) => ByteString -> m v
deserialiseM =
    either (throwM . DbProtocolError . ("Deserialisation error "<>) . toText) pure . deserialise

----------------------------------------------------------------------------
-- MapLayer instances
----------------------------------------------------------------------------

instance Hashable AVL.Tilt
instance Hashable b => Hashable (AVL.WithBounds b)
instance (Hashable h, Hashable k, Hashable v, Hashable s) => Hashable (MapLayer h k v s)

instance Default (MapLayer h k v s) where
    def = MLEmpty def def

instance (Show h, Show k, Show v) => Buildable (AVL.Map h k v) where
    build = Buildable.build . AVL.showMap

instance (Show h, Show k, Show v) => Buildable (AVL.Proof h k v) where
    build (AVL.Proof tree) = Buildable.build tree
