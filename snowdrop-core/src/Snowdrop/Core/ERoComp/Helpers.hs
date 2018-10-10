{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Primitive operations inside ERoComp. Utilities to modify context of compiutation.

module Snowdrop.Core.ERoComp.Helpers
       ( QueryERo
       , query
       , iterator
       , queryOne
       , queryOneExists

       , ChgAccumCtx (..)
       , StatePException (..)

       , computeUndo
       , modifyAccumUndo
       , modifyAccum
       , modifyAccumOne

       , initAccumCtx
       , getCAOrDefault
       , withModifiedAccumCtxOne

       , UpCastableERo
       , UpCastableERoM
       , upcastDbAccess
       , upcastDbAccessM
       , upcastEffERoComp
       , upcastEffERoCompM

       , ConvertEffect (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Data.Vinyl.Lens (rget)

import           Snowdrop.Core.BaseM (BaseM (..), Effectful (..), effect, hoistEffectful)
import           Snowdrop.Core.ChangeSet (CSMappendException (..), HChangeSet, HUpCastableChSet)

import           Snowdrop.Core.ERoComp.Types (ChgAccum, DbAccess (..), DbAccessM (..),
                                              DbAccessU (..), ERoComp, ERoCompM, ERoCompU,
                                              FoldF (..))
import           Snowdrop.Util
import qualified Snowdrop.Util as Log

----------------------------------------------------
--- Primitive operations inside ERoComp
----------------------------------------------------

class (HUpCastableSet '[t] xs, HIntersectable xs '[t] ) => QueryERo xs t
instance (HUpCastableSet '[t] xs, HIntersectable xs '[t]) => QueryERo xs t

-- | Creates a DbQuery operation.
query
    :: forall t xs e ctx . QueryERo xs t
    => Set (HKey t) -> ERoComp e xs ctx (Map (HKey t) (HVal t))
query req = do
    let hreq :: HSet '[t]
        hreq = hsetFromSet req
    effect $
        DbQuery
            (hupcast @_ @_ @xs hreq)
            (hmapToMap . flip (hintersect @xs @'[t]) hreq)

-- | Creates a DbIterator operation.
iterator
    :: forall t b xs e ctx. (HElem t xs)
    => b
    -> (b -> (HKey t, HVal t) -> b)
    -> ERoComp e xs ctx b
iterator e foldf = effect $ DbIterator @xs @b @t rget (FoldF (e, foldf, id))

-- | Creates a DbComputeUndo operation.
computeUndo
  :: forall e xs undo ctx . HasException e CSMappendException
  => ChgAccum ctx -> ERoCompU e xs undo ctx undo
computeUndo chgAcc = effect (DbComputeUndo @_ @_ @xs chgAcc id) >>= either throwLocalError pure

-- | Creates a DbModifyAccumUndo operation.
modifyAccumUndo
  :: forall e xs undo ctx . HasException e CSMappendException
  => NewestFirst [] undo -> ERoCompU e xs undo ctx (ChgAccum ctx)
modifyAccumUndo undos = effect (DbModifyAccumUndo @_ @_ @xs undos id) >>= either throwLocalError pure

-- | Creates a DbModifyAccum operation.
modifyAccum
    :: forall e xs ctx .
      HasException e CSMappendException
    => OldestFirst [] (HChangeSet xs)
    -> ERoCompM e xs ctx (OldestFirst [] (ChgAccum ctx))
modifyAccum css = effect (DbModifyAccum css id) >>= either throwLocalError pure

modifyAccumOne
    :: forall e xs ctx .
        HasException e CSMappendException
    => HChangeSet xs
    -> ERoCompM e xs ctx (ChgAccum ctx)
modifyAccumOne cs =
    effect (DbModifyAccum (OldestFirst [cs]) (fmap $ head' . unOldestFirst))
        >>= either throwLocalError pure
  where
    head' []    = error "modifyAccumOne: unexpected empty effect execution result"
    head' (a:_) = a

queryOne
    :: forall t components e ctx . (QueryERo components t, Ord (HKey t))
    => HKey t -> ERoComp e components ctx (Maybe (HVal t))
queryOne k = M.lookup k <$> query @t (S.singleton k)

-- | Check key exists in state.
queryOneExists
    :: forall t components e ctx . (QueryERo components t, Ord (HKey t))
    => HKey t -> ERoComp e components ctx Bool
queryOneExists k = isJust <$> queryOne @t k

------------------------
-- ChgAccumCtx
------------------------

-- | Possible errors throwing from basic functions in ERoComp.
data StatePException = ChgAccumCtxUnexpectedlyInitialized
    deriving (Show, Eq)

instance Buildable StatePException where
    build = \case
        ChgAccumCtxUnexpectedlyInitialized -> "Change accum context is unexpectedly initialized"

deriveIdView withInj ''StatePException

-- | Auxiliary datatype for context-dependant computations.
data ChgAccumCtx ctx = CANotInitialized | CAInitialized (ChgAccum ctx)

-- | Gets value of Change Accumulator or default value if it's not initialized.
getCAOrDefault :: Default (ChgAccum ctx) => ChgAccumCtx ctx -> ChgAccum ctx
getCAOrDefault CANotInitialized   = def
getCAOrDefault (CAInitialized cA) = cA

-- | Runs computation with modified Change Accumulator from @ctx@.
-- Passed ChangeSet will be appended to the accumulator as a modification.
withModifiedAccumCtxOne
  :: forall xs e ctx a .
    (HasException e CSMappendException, HasLens ctx (ChgAccumCtx ctx), Default (ChgAccum ctx))
  => HChangeSet xs
  -> ERoCompM e xs ctx a
  -> ERoCompM e xs ctx a
withModifiedAccumCtxOne chgSet comp = do
    acc' <- modifyAccumOne chgSet
    local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp

----------------------------------------------------
--- Functions to modify computation's context
----------------------------------------------------

-- | Runs computation with specified initial Change Accumulator.
initAccumCtx
    :: forall e eff ctx a .
    (HasException e StatePException, HasLens ctx (ChgAccumCtx ctx))
    => ChgAccum ctx
    -> BaseM e eff ctx a
    -> BaseM e eff ctx a
initAccumCtx acc' comp = do
    gett @_ @(ChgAccumCtx ctx) <$> ask >>= \case
        CAInitialized _ -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
        CANotInitialized ->
            local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp


------------------------
-- Cast and hoist
------------------------

type UpCastableERo xs supxs =
    ( HIntersectable supxs xs
    , HUpCastableSet xs supxs
    )

type UpCastableERoM xs supxs =
    ( UpCastableERo xs supxs
    , HUpCastableChSet xs supxs
    )

upcastDbAccessM
    :: forall xs supxs chgAccum a . UpCastableERoM xs supxs
    => DbAccessM chgAccum xs a
    -> DbAccessM chgAccum supxs a
upcastDbAccessM (DbAccess da)            = DbAccess (upcastDbAccess da)
upcastDbAccessM (DbModifyAccum css cont) = DbModifyAccum (hupcast <$> css) cont

upcastDbAccess
    :: forall xs supxs a . UpCastableERo xs supxs
    => DbAccess xs a
    -> DbAccess supxs a
upcastDbAccess (DbQuery s cont) =
    DbQuery (hupcast @_ @xs @supxs s) (\resp -> cont (hintersect @supxs @xs resp s))
upcastDbAccess (DbIterator pr foldf) = DbIterator (pr . hdowncast) foldf

-- TODO reimplement this method without use of ReaderT-based hoistEffectful (similar to ConvertEffect)
upcastEffERoComp
    :: forall xs supxs e ctx a . UpCastableERo xs supxs
    => ERoComp e xs ctx a
    -> ERoComp e supxs ctx a
upcastEffERoComp = hoistEffectful upcastDbAccess

upcastEffERoCompM
    :: forall xs supxs e ctx a . UpCastableERoM xs supxs
    => ERoCompM e xs ctx a
    -> ERoCompM e supxs ctx a
upcastEffERoCompM = hoistEffectful upcastDbAccessM

-- TODO rename ConvertEffect to WrapEffect
class ConvertEffect e ctx eff1 eff2 where
    convertEffect :: BaseM e eff1 ctx a -> BaseM e eff2 ctx a

newtype DbAccessT (eff1 :: [*] -> * -> *) (eff2 :: [*] -> * -> *) m a = DbAccessT { runDbAccessT :: m a }
    deriving ( Functor, Applicative, Monad, MonadError e
             , MonadReader ctx, Log.MonadLogging, Log.ModifyLogName)

instance (Effectful (DbAccessM chgAccum xs) m)
    => Effectful (DbAccess xs)
                 (DbAccessT DbAccess (DbAccessM chgAccum) m) where
    effect da = DbAccessT $ effect (DbAccess @chgAccum da)


instance (Effectful (DbAccessU chgAccum undo xs) m)
    => Effectful (DbAccess xs)
                 (DbAccessT DbAccess (DbAccessU chgAccum undo) m) where
    effect da = DbAccessT $ effect (DbAccessM @chgAccum @undo $ DbAccess da)

instance (Effectful (DbAccessU chgAccum undo xs) m)
    => Effectful (DbAccessM chgAccum xs)
                 (DbAccessT (DbAccessM chgAccum) (DbAccessU chgAccum undo) m) where
    effect da = DbAccessT $ effect (DbAccessM @chgAccum @undo da)

instance ConvertEffect e ctx (DbAccess xs) (DbAccessM chgAccum xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @DbAccess @(DbAccessM chgAccum) action )

instance ConvertEffect e ctx (DbAccessM chgAccum xs) (DbAccessU chgAccum undo xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @(DbAccessM chgAccum) @(DbAccessU chgAccum undo) action )

instance ConvertEffect e ctx (DbAccess xs) (DbAccessU chgAccum undo xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @DbAccess @(DbAccessU chgAccum undo) action )

instance ConvertEffect e ctx (DbAccessU chgAccum undo xs) (DbAccessU chgAccum undo xs) where
    convertEffect = id

instance ConvertEffect e ctx (DbAccessM chgAccum xs) (DbAccessM chgAccum xs) where
    convertEffect = id

instance ConvertEffect e ctx (DbAccess xs) (DbAccess xs) where
    convertEffect = id
