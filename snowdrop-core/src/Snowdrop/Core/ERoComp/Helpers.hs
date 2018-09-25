{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Primitive operations inside ERoComp. Utilities to modify context of compiutation.

module Snowdrop.Core.ERoComp.Helpers
       ( StatePException (..)

       , query
       , iterator
       , iteratorFor
       , querySet
       , queryOne
       , queryOneExists

       , computeUndo
       , modifyAccumUndo
       , modifyAccum
       , modifyAccumOne

       , initAccumCtx
       , getCAOrDefault
       , withModifiedAccumCtxOne

       , ConvertEffect (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable

import           Snowdrop.Core.BaseM (BaseM (..), Effectful (..))
import           Snowdrop.Core.ChangeSet (CSMappendException (..), ChangeSet (..))
import           Snowdrop.Core.ERoComp.Types (ChgAccum, ChgAccumCtx (..), DbAccess (..),
                                              DbAccessM (..), DbAccessU (..), ERoComp, ERoCompM,
                                              ERoCompU, FoldF (..), Prefix (..), StateP, StateR)
import           Snowdrop.Core.Transaction (HasKeyValue)
import           Snowdrop.Util
import qualified Snowdrop.Util as Log

-- | Possible errors throwning from basic functions in ERoComp.
data StatePException
    = QueryProjectionFailed
    | IteratorProjectionFailed
    | ChgAccumCtxUnexpectedlyInitialized
    deriving (Show, Eq)

instance Buildable StatePException where
    build = \case
        QueryProjectionFailed -> "Projection of query result failed"
        IteratorProjectionFailed -> "Projection within iterator failed"
        ChgAccumCtxUnexpectedlyInitialized -> "Change accum context is unexpectedly initialized"

deriveIdView withInj ''StatePException

----------------------------------------------------
--- Primitive operations inside ERoComp
----------------------------------------------------

-- | Creates a DbQuery operation.
query :: forall e id value ctx . StateR id -> ERoComp e id value ctx (StateP id value)
query req = effect $ DbQuery req id

-- | Creates a DbIterator operation.
iterator
    :: forall e id value ctx b.
       Prefix
    -> b
    -> ((id, value) -> b -> b)
    -> ERoComp e id value ctx b
iterator prefix e foldf =
    effect $ DbIterator prefix (FoldF (e, foldf, id))

-- | Creates a DbModifyAccum operation.
modifyAccumOne
    :: forall e id value ctx .
      (HasException e (CSMappendException id))
    => ChangeSet id value
    -> ERoCompM e id value ctx (ChgAccum ctx)
modifyAccumOne cs =
    effect (DbModifyAccum (OldestFirst [cs]) (fmap $ head' . unOldestFirst))
      >>= either throwLocalError pure
  where
    head' []    = error "modifyAccumOne: unexpected empty effect execution result"
    head' (a:_) = a

-- | Creates a DbComputeUndo operation.
computeUndo
  :: forall e id value undo ctx . (HasException e (CSMappendException id))
  => ChgAccum ctx -> ERoCompU e id value undo ctx undo
computeUndo chgAcc = effect (DbComputeUndo @_ @_ @id @value chgAcc id) >>= either throwLocalError pure

modifyAccumUndo
  :: forall e id value undo ctx . (HasException e (CSMappendException id))
  => NewestFirst [] undo -> ERoCompU e id value undo ctx (ChgAccum ctx)
modifyAccumUndo undos = effect (DbModifyAccumUndo @_ @_ @id @value undos id) >>= either throwLocalError pure

-- | Creates a DbModifyAccum operation.
modifyAccum
    :: forall e id value ctx .
      (HasException e (CSMappendException id))
    => OldestFirst [] (ChangeSet id value)
    -> ERoCompM e id value ctx (OldestFirst [] (ChgAccum ctx))
modifyAccum css = effect (DbModifyAccum css id) >>= either throwLocalError pure

-- | Like @query@, however, inject @id'@ to @id@ before a query and project @value@ to @value'@
-- after the query.
-- If some @value'@ couldn't be projected from corresponding @value@, the request will fail.
querySet
    :: forall id id' value value' e ctx .
    ( Ord id, Ord id'
    , HasKeyValue id value id' value'
    , HasException e StatePException
    )
    => Set id' -> ERoComp e id value ctx (StateP id' value')
querySet ids' =
    -- Choose needed ids from state, then try to project all values.
    maybe (throwLocalError QueryProjectionFailed) pure . proj =<< query ids
  where
    ids :: Set id
    ids = S.fromList $ map inj $ S.toList ids'

-- | Request one value for passed key.
queryOne
    :: forall id id' value value' e ctx .
    ( Ord id, Ord id'
    , HasKeyValue id value id' value'
    , HasException e StatePException
    )
    => id' -> ERoComp e id value ctx (Maybe value')
queryOne id' = M.lookup id' <$> querySet (S.singleton id')

-- | Check key exists in state.
queryOneExists
    :: forall id id' value e ctx .
    ( Ord id, Ord id'
    , HasPrism id id'
    )
    => id' -> ERoComp e id value ctx Bool
queryOneExists id' = not . M.null <$> query (S.singleton (inj id'))

-- | Like @iterator@, however, project @id@ and @value@ before passing them to
-- the accumulator function.
iteratorFor
    :: forall id id' value value' b e ctx .
    ( HasPrism id id'
    , HasPrism value value'
    , HasException e StatePException
    )
    => Prefix
    -> b
    -> ((id', value') -> b -> b)
    -> ERoComp e id value ctx b
iteratorFor prefix e foldf = do
    res <- effect $ DbIterator @id @value prefix (FoldF (e1, foldf1, id))
    case res of
        Left ex -> throwLocalError ex
        Right x -> pure x
  where
    e1 = Right e

    foldf1 _ (Left ex) = Left ex
    foldf1 (i, value) (Right b) = case fPair (proj i, proj value) of
        Nothing           -> Left IteratorProjectionFailed
        Just (i', value') -> Right $ foldf (i', value') b

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

-- | Gets value of Change Accumulator or default value if it's not initialized.
getCAOrDefault :: Default (ChgAccum ctx) => ChgAccumCtx ctx -> ChgAccum ctx
getCAOrDefault CANotInitialized   = def
getCAOrDefault (CAInitialized cA) = cA

class ConvertEffect e ctx eff1 eff2 where
    convertEffect :: BaseM e eff1 ctx a -> BaseM e eff2 ctx a

newtype DbAccessT (eff1 :: * -> * -> * -> *) (eff2 :: * -> * -> * -> *) m a = DbAccessT { runDbAccessT :: m a }
    deriving ( Functor, Applicative, Monad, MonadError e
             , MonadReader ctx, Log.MonadLogging, Log.ModifyLogName)

instance (Effectful (DbAccessM chgAccum id value) m)
    => Effectful (DbAccess id value)
                 (DbAccessT DbAccess (DbAccessM chgAccum) m) where
    effect da = DbAccessT $ effect (DbAccess @chgAccum da)

instance (Effectful (DbAccessU chgAccum undo id value) m)
    => Effectful (DbAccess id value)
                 (DbAccessT DbAccess (DbAccessU chgAccum undo) m) where
    effect da = DbAccessT $ effect (DbAccessM @chgAccum @undo $ DbAccess da)

instance (Effectful (DbAccessU chgAccum undo id value) m)
    => Effectful (DbAccessM chgAccum id value)
                 (DbAccessT (DbAccessM chgAccum) (DbAccessU chgAccum undo) m) where
    effect da = DbAccessT $ effect (DbAccessM @chgAccum @undo da)

instance ConvertEffect e ctx (DbAccess id value) (DbAccessM chgAccum id value) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @DbAccess @(DbAccessM chgAccum) action )

instance ConvertEffect e ctx (DbAccessM chgAccum id value) (DbAccessU chgAccum undo id value) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @(DbAccessM chgAccum) @(DbAccessU chgAccum undo) action )

instance ConvertEffect e ctx (DbAccess id value) (DbAccessU chgAccum undo id value) where
    convertEffect (BaseM action) = BaseM ( runDbAccessT @DbAccess @(DbAccessU chgAccum undo) action )

instance ConvertEffect e ctx (DbAccessU chgAccum undo id value) (DbAccessU chgAccum undo id value) where
    convertEffect = id

instance ConvertEffect e ctx (DbAccessM chgAccum id value) (DbAccessM chgAccum id value) where
    convertEffect = id

instance ConvertEffect e ctx (DbAccess id value) (DbAccess id value) where
    convertEffect = id

-- | Runs computation with modified Change Accumulator from @ctx@.
-- Passed ChangeSet will be appended to the accumulator as a modification.
withModifiedAccumCtxOne
    :: forall e id value ctx a .
    ( HasException e (CSMappendException id)
    , HasLens ctx (ChgAccumCtx ctx)
    , Default (ChgAccum ctx)
    )
    => ChangeSet id value
    -> ERoCompM e id value ctx a
    -> ERoCompM e id value ctx a
withModifiedAccumCtxOne chgSet comp = do
    acc' <- modifyAccumOne chgSet
    local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp
