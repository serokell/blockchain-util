{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.AVLp.Avl
       ( AvlHashable
       , KVConstraint
       , RootHash (..)
       , saveAVL
       , materialize
       , mkAVL
       , avlRootHash
       , deserialiseM
       ) where

import           Universum

import           Control.Monad.Free (Free (Free))
import           Data.Tree.AVL (MapLayer (..), Serialisable (..))
import qualified Data.Tree.AVL as AVL

import           Data.Default (Default (def))
import qualified Data.Text.Buildable as Buildable
import           Snowdrop.Execution.DbActions.Types (DbActionsException (..))


type AvlHashable h = (Ord h, Show h, Typeable h, Serialisable h)
type KVConstraint k v = (Typeable k, Ord k, Show k,
                         Serialisable k, Serialisable v, Show v, Eq v)

newtype RootHash h = RootHash { unRootHash :: h }
    deriving (Eq, Serialisable)

saveAVL :: forall h k v m . (AVL.Stores h k v m, MonadCatch m) => AVL.Map h k v -> m (RootHash h)
saveAVL avl = AVL.save avl $> avlRootHash avl

-- | Load whole tree from disk in memory.
materialize :: forall h k v m . AVL.Stores h k v m => AVL.Map h k v -> m (AVL.Map h k v)
materialize initAVL = flip AVL.openAndM initAVL $ \case
    MLBranch h m c t l r -> fmap Free $ MLBranch h m c t <$> materialize l <*> materialize r
    rest -> pure $ Free rest

mkAVL :: RootHash h -> AVL.Map h k v
mkAVL = pure . unRootHash

-- | Get root hash of AVL tree.
avlRootHash :: AVL.Map h k v -> RootHash h
avlRootHash = RootHash . AVL.rootHash

deserialiseM :: (MonadThrow m, Serialisable v) => ByteString -> m v
deserialiseM =
    either (throwM . DbProtocolError . ("Deserialisation error "<>) . toText) pure . deserialise

----------------------------------------------------------------------------
-- MapLayer instances
----------------------------------------------------------------------------

instance Hashable AVL.Tilt
instance Hashable b => Hashable (AVL.WithBounds b)
instance (Hashable h, Hashable k, Hashable v, Hashable s) => Hashable (MapLayer h k v s)

instance Default h => Default (MapLayer h k v s) where
    def = MLEmpty def

instance (Show h, Show k, Show v) => Buildable (AVL.Map h k v) where
    build = Buildable.build . AVL.showMap

instance (Show h, Show k, Show v) => Buildable (AVL.Proof h k v) where
    build (AVL.Proof tree) = Buildable.build tree
