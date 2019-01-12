{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.AVLp.Constraints where

import qualified Data.Tree.AVL as AVL
import           Snowdrop.Hetero (HKey, HVal)

class AVL.Hash h (HKey x) (HVal x) => AvlHashC h x
instance AVL.Hash h (HKey x) (HVal x) => AvlHashC h x
