{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Snowdrop.Core.ERoComp.Helpers
       ( QueryERo
       , query
       , iterator
       , modifyAccum
       , queryOne
       , queryOneExists

       , ChgAccumCtx (..)
       , StatePException (..)
       , withModifiedAccumCtx
       , initAccumCtx
       , getCAOrDefault

       , UpCastableERo
       , upcastDbAccess
       , upcastEffERoComp
       ) where

import           Universum

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable

import           Data.Default (Default (def))

import           Snowdrop.Core.BaseM (effect, hoistEffectful)
import           Snowdrop.Core.ChangeSet (CSMappendException (..), HChangeSet, HUpCastableChSet,
                                          Undo, downcastUndo, upcastUndo)
import           Snowdrop.Core.ERoComp.Types (ChgAccum, ChgAccumModifier (..), DbAccess (..),
                                              ERoComp)
import           Snowdrop.Util

------------------------
-- Basic operations in ERoComp
------------------------

class (HUpCastableSet '[t] xs, HIntersectable xs '[t] ) => QueryERo xs t
instance (HUpCastableSet '[t] xs, HIntersectable xs '[t]) => QueryERo xs t

query
    :: forall t xs e ctx . QueryERo xs t
    => Set (HKey t) -> ERoComp e ctx xs (Map (HKey t) (HVal t))
query req = do
    let hreq :: HSet '[t]
        hreq = hsetFromSet req
    effect $
        DbQuery @(ChgAccum ctx)
            (hupcast @_ @_ @xs hreq)
            (hmapToMap . flip (hintersect @xs @'[t]) hreq)

iterator
    :: forall t b xs e ctx. (Elem t xs)
    => b
    -> ((HKey t, HVal t) -> b -> b)
    -> ERoComp e ctx xs b
iterator = error "iterator not supported yet"
-- iterator e foldf = effect $ DbIterator @(ChgAccum ctx) @xs @b @t (Proxy @t) (FoldF (e, foldf, id))

modifyAccum
    :: forall xs e ctx .
       ChgAccum ctx
    -> ChgAccumModifier xs
    -> ERoComp e ctx xs (Either CSMappendException (ChgAccum ctx, Undo xs))
modifyAccum chgAccum chgSet = effect $ DbModifyAccum chgAccum chgSet id

queryOne
    :: forall t components e ctx . (QueryERo components t, Ord (HKey t))
    => HKey t -> ERoComp e ctx components (Maybe (HVal t))
queryOne k = M.lookup k <$> query @t (S.singleton k)

queryOneExists
    :: forall t components e ctx . (QueryERo components t, Ord (HKey t))
    => HKey t -> ERoComp e ctx components Bool
queryOneExists k = isJust <$> queryOne @t k

------------------------
-- ChgAccumCtx
------------------------

data StatePException = ChgAccumCtxUnexpectedlyInitialized
    deriving (Show, Eq)

instance Buildable StatePException where
    build = \case
        ChgAccumCtxUnexpectedlyInitialized -> "Change accum context is unexpectedly initialized"

deriveIdView withInj ''StatePException

-- | Auxiliary datatype for context-dependant computations.
data ChgAccumCtx ctx = CANotInitialized | CAInitialized (ChgAccum ctx)

getCAOrDefault :: Default (ChgAccum ctx) => ChgAccumCtx ctx -> ChgAccum ctx
getCAOrDefault CANotInitialized   = def
getCAOrDefault (CAInitialized cA) = cA

withModifiedAccumCtx
  :: forall xs e ctx a .
    (HasException e CSMappendException, HasLens ctx (ChgAccumCtx ctx), Default (ChgAccum ctx))
  => HChangeSet xs
  -> ERoComp e ctx xs a
  -> ERoComp e ctx xs a
withModifiedAccumCtx chgSet comp = do
    ctxAcc <- getCAOrDefault @ctx . gett <$> ask
    newAccOrErr <- modifyAccum ctxAcc $ CAMChange chgSet
    case newAccOrErr of
        Left err            -> throwLocalError err
        Right (acc', _undo) -> local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp

initAccumCtx
  :: forall xs e ctx a .
     (HasException e StatePException, HasLens ctx (ChgAccumCtx ctx))
  => ChgAccum ctx
  -> ERoComp e ctx xs a
  -> ERoComp e ctx xs a
initAccumCtx acc' comp = do
    gett @_ @(ChgAccumCtx ctx) <$> ask >>= \case
        CAInitialized _ -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
        CANotInitialized -> local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp

------------------------
-- Cast and hoist
------------------------

type UpCastableERo xs supxs =
    ( HIntersectable supxs xs
    , HUpCastableMap xs supxs
    , HUpCastableSet xs supxs
    , HUpCastableChSet xs supxs
    )

upcastDbAccess
    :: forall xs supxs chgAcc a . UpCastableERo xs supxs
    => DbAccess chgAcc xs a
    -> DbAccess chgAcc supxs a
upcastDbAccess (DbQuery s cont) =
    DbQuery (hupcast @_ @xs @supxs s) (\resp -> cont (hintersect @supxs @xs resp s))
-- upcastDbAccess (DbIterator pr foldf) = DbIterator pr foldf
upcastDbAccess (DbModifyAccum acc md cont) = case md of
    CAMChange cs -> DbModifyAccum acc (CAMChange $ hupcast @_ @xs @supxs cs) (cont . fmap (fmap downcastUndo))
    CAMRevert undo -> DbModifyAccum acc (CAMRevert $ upcastUndo @xs @supxs undo) (cont . fmap (fmap downcastUndo))

upcastEffERoComp
    :: forall xs supxs e ctx a . UpCastableERo xs supxs
    => ERoComp e ctx xs a
    -> ERoComp e ctx supxs a
upcastEffERoComp = hoistEffectful upcastDbAccess

