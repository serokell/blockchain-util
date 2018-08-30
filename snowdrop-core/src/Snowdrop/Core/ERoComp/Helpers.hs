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
       , modifyAccum

       , withModifiedAccumCtx
       , initAccumCtx
       , getCAOrDefault
       ) where

import           Universum

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable

import           Data.Default (Default (def))

import           Snowdrop.Core.BaseM (effect)
import           Snowdrop.Core.ChangeSet (CSMappendException (..), ChangeSet (..), Undo)
import           Snowdrop.Core.ERoComp.Types (ChgAccum, ChgAccumCtx (..), ChgAccumModifier (..),
                                              DbAccess (..), ERoComp, FoldF (..), Prefix (..),
                                              StateP, StateR)
import           Snowdrop.Core.Transaction (HasKeyValue)
import           Snowdrop.Util

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
query req = effect $ DbQuery @(ChgAccum ctx) req id

-- | Creates a DbIterator operation.
iterator
    :: forall e id value ctx b.
       Prefix
    -> b
    -> ((id, value) -> b -> b)
    -> ERoComp e id value ctx b
iterator prefix e foldf =
    effect $ DbIterator @(ChgAccum ctx) prefix (FoldF (e, foldf, id))

-- | Creates a DbModifyAccum operation.
modifyAccum
    :: forall e id value ctx .
       ChgAccum ctx
    -> ChgAccumModifier id value
    -> ERoComp e id value ctx (Either (CSMappendException id) (ChgAccum ctx, Undo id value))
modifyAccum chgAccum chgSet = effect $ DbModifyAccum chgAccum chgSet id

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
    res <- effect $ DbIterator @(ChgAccum ctx) @id @value prefix (FoldF (e1, foldf1, id))
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

-- | Runs computation with modified Change Accumulator from @ctx@.
-- Passed ChangeSet will be appended to the accumulator as a modification.
withModifiedAccumCtx
    :: forall e id value ctx a .
    ( HasException e (CSMappendException id)
    , HasLens ctx (ChgAccumCtx ctx)
    , Default (ChgAccum ctx)
    )
    => ChangeSet id value
    -> ERoComp e id value ctx a
    -> ERoComp e id value ctx a
withModifiedAccumCtx chgSet comp = do
    ctxAcc <- getCAOrDefault @ctx . gett <$> ask
    newAccOrErr <- modifyAccum ctxAcc $ CAMChange chgSet
    case newAccOrErr of
        Left err   -> throwLocalError err
        Right (acc', _undo) ->
            local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp

-- | Runs computation with specified initial Change Accumulator.
initAccumCtx
    :: forall e id value ctx a .
    (HasException e StatePException, HasLens ctx (ChgAccumCtx ctx))
    => ChgAccum ctx
    -> ERoComp e id value ctx a
    -> ERoComp e id value ctx a
initAccumCtx acc' comp = do
    gett @_ @(ChgAccumCtx ctx) <$> ask >>= \case
        CAInitialized _ -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
        CANotInitialized ->
            local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp

-- | Gets value of Change Accumulator or default value if it's not initialized.
getCAOrDefault :: Default (ChgAccum ctx) => ChgAccumCtx ctx -> ChgAccum ctx
getCAOrDefault CANotInitialized   = def
getCAOrDefault (CAInitialized cA) = cA
