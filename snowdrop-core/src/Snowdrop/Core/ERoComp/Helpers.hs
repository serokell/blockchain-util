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

       , ChgAccumMaybe (..)
       , ChgAccumCtx
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

import           Snowdrop.Core.BaseM (BaseM (..), Effectful (..), effect)
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
    effect $
        DbQuery
            (hupcast @_ @_ @xs hreq)
            (hmapToMap . flip (hintersect @xs @'[t]) hreq)

-- | Creates a DbIterator operation.
iterator
    :: forall t b xs conf. (HElem t xs)
    => b
    -> (b -> (HKey t, HVal t) -> b)
    -> ERoComp conf xs b
iterator e foldf = effect $ DbIterator @xs @b @t rget (FoldF (e, foldf, id))

-- | Creates a DbComputeUndo operation.
computeUndo
  :: forall xs conf . HasBException conf CSMappendException
  => ChgAccum conf -> ERoCompU conf xs (Undo conf)
computeUndo chgAcc = effect (DbComputeUndo @_ @_ @xs chgAcc id) >>= either throwLocalError pure

-- | Creates a DbModifyAccumUndo operation.
modifyAccumUndo
  :: forall xs conf . HasBException conf CSMappendException
  => NewestFirst [] (Undo conf) -> ERoCompU conf xs (ChgAccum conf)
modifyAccumUndo undos = effect (DbModifyAccumUndo @_ @_ @xs undos id) >>= either throwLocalError pure

-- | Creates a DbModifyAccum operation.
modifyAccum
    :: HasBException conf CSMappendException
    => OldestFirst [] (HChangeSet xs)
    -> ERoCompM conf xs (OldestFirst [] (ChgAccum conf))
modifyAccum css = effect (DbModifyAccum css id) >>= either throwLocalError pure

modifyAccumOne
    :: HasBException conf CSMappendException
    => HChangeSet xs
    -> ERoCompM conf xs (ChgAccum conf)
modifyAccumOne cs =
    effect (DbModifyAccum (OldestFirst [cs]) (fmap $ head' . unOldestFirst))
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
data ChgAccumMaybe chgAcc = CANotInitialized | CAInitialized chgAcc

type ChgAccumCtx conf = ChgAccumMaybe (ChgAccum conf)

-- | Gets value of Change Accumulator or default value if it's not initialized.
getCAOrDefault :: Default chgAcc => ChgAccumMaybe chgAcc -> chgAcc
getCAOrDefault CANotInitialized   = def
getCAOrDefault (CAInitialized cA) = cA

withAccum
  :: forall ctx chgAcc m a .
    ( MonadReader ctx m
    , HasLens ctx (ChgAccumMaybe chgAcc)
    )
  => chgAcc
  -> m a
  -> m a
withAccum acc comp = local ( lensFor @ctx .~ CAInitialized acc ) comp

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
        CANotInitialized -> withAccum acc' comp

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
    :: UpCastableERoM xs supxs
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

upcastEffERoComp
    :: forall xs supxs conf a . UpCastableERo xs supxs
    => ERoComp conf xs a
    -> ERoComp conf supxs a
upcastEffERoComp (BaseM action) = BaseM ( runUpCastERoT @DbAccess @supxs action )

upcastEffERoCompM
    :: forall xs supxs conf a . UpCastableERoM xs supxs
    => ERoCompM conf xs a
    -> ERoCompM conf supxs a
upcastEffERoCompM (BaseM action) = BaseM ( runUpCastERoT @(DbAccessM (ChgAccum conf)) @supxs action )

newtype UpCastERoT (eff :: [*] -> * -> *) (xs :: [*]) m a = UpCastERoT { runUpCastERoT :: m a }
    deriving ( Functor, Applicative, Monad, MonadError e
             , MonadReader ctx, Log.MonadLogging, Log.ModifyLogName)

instance (Effectful (DbAccess supxs) m, UpCastableERo xs supxs)
    => Effectful (DbAccess xs) (UpCastERoT DbAccess supxs m) where
    effect = UpCastERoT . effect . upcastDbAccess @_ @supxs

instance (Effectful (DbAccessM chgAccum supxs) m, UpCastableERoM xs supxs)
  => Effectful (DbAccessM chgAccum xs) (UpCastERoT (DbAccessM chgAccum) supxs m) where
    effect = UpCastERoT . effect . upcastDbAccessM @_ @supxs

class ConvertEffect eff1 eff2 where
    convertEffect :: BaseM e eff1 ctx a -> BaseM e eff2 ctx a

newtype DbAccessT (eff1 :: [*] -> * -> *) (eff2 :: [*] -> * -> *) m a = DbAccessT { runDbAccessT :: m a }
    deriving ( Functor, Applicative, Monad, MonadError e
             , MonadReader ctx, Log.MonadLogging, Log.ModifyLogName)

instance (Effectful (DbAccessM chgAccum xs) m)
    => Effectful (DbAccess xs)
                 (DbAccessT DbAccess (DbAccessM chgAccum) m) where
    effect da = DbAccessT $ effect (DbAccess @chgAccum da)


instance (Effectful (DbAccessU undo chgAccum xs) m)
    => Effectful (DbAccess xs)
                 (DbAccessT DbAccess (DbAccessU undo chgAccum) m) where
    effect da = DbAccessT $ effect (DbAccessM @undo @chgAccum $ DbAccess da)

instance (Effectful (DbAccessU undo chgAccum xs) m)
    => Effectful (DbAccessM chgAccum xs)
                 (DbAccessT (DbAccessM chgAccum) (DbAccessU undo chgAccum) m) where
    effect da = DbAccessT $ effect (DbAccessM @undo da)

instance ConvertEffect (DbAccess xs) (DbAccessM chgAccum xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @DbAccess @(DbAccessM chgAccum) action )

instance ConvertEffect (DbAccessM chgAccum xs) (DbAccessU undo chgAccum xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @(DbAccessM chgAccum) @(DbAccessU undo chgAccum) action )

instance ConvertEffect (DbAccess xs) (DbAccessU undo chgAccum xs) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @DbAccess @(DbAccessU undo chgAccum) action )

instance ConvertEffect (DbAccessU undo chgAccum xs) (DbAccessU undo chgAccum xs) where
    convertEffect = id

instance ConvertEffect (DbAccessM chgAccum xs) (DbAccessM chgAccum xs) where
    convertEffect = id

instance ConvertEffect (DbAccess xs) (DbAccess xs) where
    convertEffect = id
