{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Primitive operations inside ERoComp. Utilities to modify context of compiutation.

module Snowdrop.Core.ERoComp.Helpers
       ( HasBException
       , HasBExceptions
       , QueryERo
       , query
       , iterator
       , queryOne
       , queryOneExists

       , StatePException (..)

       , computeUndo
       , modifyAccumUndo
       , modifyAccum
       , modifyAccumOne

       , withModifiedAccumCtxOne
       ) where

import           Universum

import qualified Data.Map.Strict as M
import qualified Data.Text.Buildable

import           Snowdrop.Core.ChangeSet (HChangeSet)

import           Snowdrop.Core.ERoComp.Types (BException, ChgAccum, DbAccess (..),
                                              DbAccessM (..), DbAccessU (..), Undo)
import           Snowdrop.Hetero (HIntersectable, HKey, HVal, HMap, HMapEl (..),
                                  HUpCastableSet)
import           Snowdrop.Util (HasReview (..), HasReviews, NewestFirst (..),
                                OldestFirst (..), unOldestFirst)

import           Data.Vinyl.Lens (type (∈), rget)
import           Control.Exception (throw)

type HasBException conf e1 = HasReview (BException conf) e1
type HasBExceptions conf xs = HasReviews (BException conf) xs

----------------------------------------------------
--- Primitive operations inside ERoComp
----------------------------------------------------

class (HUpCastableSet '[t] xs, HIntersectable xs '[t] ) => QueryERo xs t
instance (HUpCastableSet '[t] xs, HIntersectable xs '[t]) => QueryERo xs t

-- | Creates a DbQuery operation.

query :: forall t xs . (t ∈ xs, Ord (HKey t)) => Set (HKey t) -> HMap xs -> Map (HKey t) (HVal t)
query req hmap = M.restrictKeys (unHMapEl $ rget @t hmap) req

iterator :: b -> (b -> k -> v -> b) -> DbAccess k v b
iterator e foldf = FoldF (e, foldf)

computeUndo
  :: forall conf . ChgAccum conf -> DbAccessU conf (Undo conf)
computeUndo chgAcc = DbComputeUndo chgAcc (either throw id)

-- | Creates a DbModifyAccumUndo operation.
modifyAccumUndo
  :: forall conf . NewestFirst [] (Undo conf) -> DbAccessU conf (ChgAccum conf)
modifyAccumUndo undos = DbModifyAccumUndo undos (either throw id)

-- | Creates a DbModifyAccum operation.
modifyAccum
    :: forall xs conf . OldestFirst [] (HChangeSet xs) -> DbAccessM conf xs (OldestFirst [] (ChgAccum conf))
modifyAccum css = DbModifyAccum css (either throw id)

modifyAccumOne
    :: forall xs conf . HChangeSet xs -> DbAccessM conf xs (ChgAccum conf)
modifyAccumOne cs = DbModifyAccum (OldestFirst [cs]) (either throw (head' . unOldestFirst))
  where
    head' []    = error "modifyAccumOne: unexpected empty effect execution result"
    head' (a:_) = a

queryOne :: forall t xs . (t ∈ xs, Ord (HKey t)) => HKey t -> HMap xs -> Maybe (HVal t)
-- Might use query but won't
queryOne k hmap = M.lookup k (unHMapEl $ rget @t hmap)

queryOneExists :: forall t xs . (t ∈ xs, Ord (HKey t)) => HKey t -> HMap xs -> Bool
queryOneExists k hmap = isJust $ queryOne @t @_ k hmap

------------------------
-- ChgAccumCtx
------------------------

-- | Possible errors throwing from basic functions in ERoComp.
data StatePException = ChgAccumCtxUnexpectedlyInitialized
    deriving (Show, Eq)

instance Buildable StatePException where
    build _ = "Change accum context is unexpectedly initialized"

instance Exception StatePException

withModifiedAccumCtxOne
  :: forall xs conf a . HChangeSet xs -> DbAccessM conf xs a -> DbAccessM conf xs a
-- FIXME? Is this what we want???
-- AFAIUI, we shouldn't simply replace the changeset under DbModifyAccum with the new one
-- We should inspect if it is "initialised" and throw an error if it is or put the supplied changeset in otherwise
-- Well, let's now pretend "initialised" means the corresponding list is nonempty!
withModifiedAccumCtxOne chgSet (DbModifyAccum (OldestFirst cs0) f)
  | null cs0  = DbModifyAccum (OldestFirst [chgSet]) f
  | otherwise = throw ChgAccumCtxUnexpectedlyInitialized

-- initAccumCtx doesn't make any sense in our settings
