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

       , ChgAccumCtx (..)
       , StatePException (..)

       , computeUndo
       , modifyAccumUndo
       , modifyAccum
       , modifyAccumOne

       , initAccumCtx
       , getCAOrDefault
       , withAccum

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

import           Snowdrop.Core.ERoComp.Types (BException, ChgAccum, Ctx, DbAccess (..),
                                              DbAccessM (..), DbAccessU (..), ERoComp, ERoCompM,
                                              ERoCompU, FoldF (..), Undo)
import           Snowdrop.Hetero (HElem, HIntersectable, HKey, HSet, HUpCastableSet, HVal,
                                  hdowncast, hintersect, hmapToMap, hsetFromSet, hupcast)
import           Snowdrop.Util (HasGetter (..), HasLens (..), HasReview (..), HasReviews,
                                NewestFirst (..), OldestFirst (..), deriveIdView, throwLocalError,
                                unOldestFirst, withInj)
import qualified Snowdrop.Util as Log

type HasBException conf e1 = HasReview (BException conf) e1
type HasBExceptions conf xs = HasReviews (BException conf) xs

----------------------------------------------------
--- Primitive operations inside ERoComp
----------------------------------------------------

class (HUpCastableSet '[t] xs, HIntersectable xs '[t] ) => QueryERo xs t
instance (HUpCastableSet '[t] xs, HIntersectable xs '[t]) => QueryERo xs t

-- | Creates a DbQuery operation.
query
    :: forall t xs conf . QueryERo xs t
    => Set (HKey t) -> ERoComp conf xs (Map (HKey t) (HVal t))
query req = do
    let hreq :: HSet '[t]
        hreq = hsetFromSet req
    effect @(DbAccess conf xs) $
        DbQuery
            (hupcast @_ @_ @xs hreq)
            (hmapToMap . flip (hintersect @xs @'[t]) hreq)

-- | Creates a DbIterator operation.
iterator
    :: forall t b xs conf. (HElem t xs)
    => b
    -> (b -> (HKey t, HVal t) -> b)
    -> ERoComp conf xs b
iterator e foldf = effect $ DbIterator @conf @xs @b @t rget (FoldF (e, foldf, id))

-- | Creates a DbComputeUndo operation.
computeUndo
  :: forall xs conf . HasBException conf CSMappendException
  => ChgAccum conf -> ERoCompU conf xs (Undo conf)
computeUndo chgAcc = effect (DbComputeUndo @conf @xs chgAcc id) >>= either throwLocalError pure

-- | Creates a DbModifyAccumUndo operation.
modifyAccumUndo
  :: forall xs conf . HasBException conf CSMappendException
  => NewestFirst [] (Undo conf) -> ERoCompU conf xs (ChgAccum conf)
modifyAccumUndo undos = effect (DbModifyAccumUndo @conf @xs undos id) >>= either throwLocalError pure

-- | Creates a DbModifyAccum operation.
modifyAccum
    :: forall xs conf .
      HasBException conf CSMappendException
    => OldestFirst [] (HChangeSet xs)
    -> ERoCompM conf xs (OldestFirst [] (ChgAccum conf))
modifyAccum css = effect (DbModifyAccum @conf css id) >>= either throwLocalError pure

modifyAccumOne
    :: forall xs conf .
        HasBException conf CSMappendException
    => HChangeSet xs
    -> ERoCompM conf xs (ChgAccum conf)
modifyAccumOne cs =
    effect (DbModifyAccum @conf (OldestFirst [cs]) (fmap $ head' . unOldestFirst))
        >>= either throwLocalError pure
  where
    head' []    = error "modifyAccumOne: unexpected empty effect execution result"
    head' (a:_) = a

queryOne
    :: forall t xs conf . (QueryERo xs t, Ord (HKey t))
    => HKey t -> ERoComp conf xs (Maybe (HVal t))
queryOne k = M.lookup k <$> query @t @_ @conf (S.singleton k)

-- | Check key exists in state.
queryOneExists
    :: forall t xs conf . (QueryERo xs t, Ord (HKey t))
    => HKey t -> ERoComp conf xs Bool
queryOneExists k = isJust <$> queryOne @t @_ @conf k

------------------------
-- ChgAccumCtx
------------------------

-- | Possible errors throwing from basic functions in ERoComp.
data StatePException
  = ChgAccumCtxUnexpectedlyInitialized
    deriving (Show, Eq)

instance Buildable StatePException where
    build = \case
        ChgAccumCtxUnexpectedlyInitialized -> "Change accum context is unexpectedly initialized"

deriveIdView withInj ''StatePException

-- | Auxiliary datatype for context-dependant computations.
data ChgAccumCtx conf = CANotInitialized | CAInitialized (ChgAccum conf)

-- | Gets value of Change Accumulator or default value if it's not initialized.
getCAOrDefault :: Default (ChgAccum conf) => ChgAccumCtx conf -> ChgAccum conf
getCAOrDefault CANotInitialized   = def
getCAOrDefault (CAInitialized cA) = cA

withAccum
  :: forall conf ctx m a .
    ( MonadReader ctx m
    , HasLens ctx (ChgAccumCtx conf)
    )
  => ChgAccum conf
  -> m a
  -> m a
withAccum acc comp = local ( lensFor @ctx @(ChgAccumCtx conf) .~ CAInitialized @conf acc ) comp

----------------------------------------------------
--- Functions to modify computation's context
----------------------------------------------------

-- | Runs computation with specified initial Change Accumulator.
initAccumCtx
    :: forall eff conf a .
      ( HasBException conf StatePException
      , HasLens (Ctx conf) (ChgAccumCtx conf)
      )
    => ChgAccum conf
    -> BaseM (BException conf) eff (Ctx conf) a
    -> BaseM (BException conf) eff (Ctx conf) a
initAccumCtx acc' comp = do
    gett @_ @(ChgAccumCtx conf) <$> ask >>= \case
        CAInitialized _ -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
        CANotInitialized -> withAccum @conf acc' comp

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
    :: forall xs supxs conf a . UpCastableERoM xs supxs
    => DbAccessM conf xs a
    -> DbAccessM conf supxs a
upcastDbAccessM (DbAccess da)            = DbAccess (upcastDbAccess da)
upcastDbAccessM (DbModifyAccum css cont) = DbModifyAccum (hupcast <$> css) cont

upcastDbAccess
    :: forall xs supxs conf a . UpCastableERo xs supxs
    => DbAccess conf xs a
    -> DbAccess conf supxs a
upcastDbAccess (DbQuery s cont) =
    DbQuery (hupcast @_ @xs @supxs s) (\resp -> cont (hintersect @supxs @xs resp s))
upcastDbAccess (DbIterator pr foldf) = DbIterator (pr . hdowncast) foldf

-- TODO reimplement this method without use of ReaderT-based hoistEffectful (similar to ConvertEffect)
upcastEffERoComp
    :: forall xs supxs conf a . UpCastableERo xs supxs
    => ERoComp conf xs a
    -> ERoComp conf supxs a
upcastEffERoComp = hoistEffectful upcastDbAccess

upcastEffERoCompM
    :: forall xs supxs conf a . UpCastableERoM xs supxs
    => ERoCompM conf xs a
    -> ERoCompM conf supxs a
upcastEffERoCompM = hoistEffectful upcastDbAccessM

-- TODO rename ConvertEffect to WrapEffect
class ConvertEffect conf eff1 eff2 where
    convertEffect :: BaseM e eff1 ctx a -> BaseM e eff2 ctx a

newtype DbAccessT (eff1 :: [*] -> * -> *) (eff2 :: [*] -> * -> *) m a = DbAccessT { runDbAccessT :: m a }
    deriving ( Functor, Applicative, Monad, MonadError e
             , MonadReader ctx, Log.MonadLogging, Log.ModifyLogName)

instance (Effectful (DbAccessM conf xs) m)
    => Effectful (DbAccess conf xs)
                 (DbAccessT (DbAccess conf) (DbAccessM conf) m) where
    effect da = DbAccessT $ effect (DbAccess @conf da)


instance (Effectful (DbAccessU conf xs) m)
    => Effectful (DbAccess conf xs)
                 (DbAccessT (DbAccess conf) (DbAccessU conf) m) where
    effect da = DbAccessT $ effect (DbAccessM @conf $ DbAccess da)

instance (Effectful (DbAccessU conf xs) m)
    => Effectful (DbAccessM conf xs)
                 (DbAccessT (DbAccessM conf) (DbAccessU conf) m) where
    effect da = DbAccessT $ effect (DbAccessM @conf da)

instance ConvertEffect conf (DbAccess conf xs) (DbAccessM conf xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @(DbAccess conf) @(DbAccessM conf) action )

instance ConvertEffect conf (DbAccessM conf xs) (DbAccessU conf xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @(DbAccessM conf) @(DbAccessU conf) action )

instance ConvertEffect conf (DbAccess conf xs) (DbAccessU conf xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @(DbAccess conf) @(DbAccessU conf) action )

instance ConvertEffect conf (DbAccessU conf xs) (DbAccessU conf xs) where
    convertEffect = id

instance ConvertEffect conf (DbAccessM conf xs) (DbAccessM conf xs) where
    convertEffect = id

instance ConvertEffect conf (DbAccess conf xs) (DbAccess conf xs) where
    convertEffect = id
