{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Core.ERoComp.Helpers
    (
      StatePException (..)
    , TxValidationException (..)

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

    , valid
    , validateIff
    , validateAll

    , StateModificationException(..)
    ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Formatting (bprint, build, stext, (%))

import           Data.Default (Default (def))

import           Snowdrop.Core.BaseM (effect)
import           Snowdrop.Core.ChangeSet (CSMappendException (..), ChangeSet (..), Undo)
import           Snowdrop.Core.ERoComp.Types (ChgAccum, ChgAccumCtx (..), ChgAccumModifier (..),
                                              DbAccess (..), ERoComp, FoldF (..), Prefix (..),
                                              StateP, StateR)
import           Snowdrop.Core.Transaction (HasKeyValue, StateTxType)
import           Snowdrop.Util

modifyAccum
  :: forall e id value ctx .
       ChgAccum ctx
    -> ChgAccumModifier id value
    -> ERoComp e id value ctx (Either (CSMappendException id) (ChgAccum ctx, Undo id value))
modifyAccum chgAccum chgSet = effect $ DbModifyAccum chgAccum chgSet id

query :: forall e id value ctx . StateR id -> ERoComp e id value ctx (StateP id value)
query req = effect $ DbQuery @(ChgAccum ctx) req id

iterator
  :: forall e id value ctx b.
    Prefix
    -> b
    -> ((id, value) -> b -> b)
    -> ERoComp e id value ctx b
iterator prefix e foldf =
  effect $ DbIterator @(ChgAccum ctx) prefix (FoldF (e, foldf, id))

data TxValidationException
    = ProofProjectionFailed StateTxType
    | PayloadProjectionFailed StateTxType Prefix
    | UnexpectedPayload [Prefix]
    deriving (Show)

instance Buildable TxValidationException where
    build = \case
        ProofProjectionFailed tx -> bprint ("Projection of proof is failed during validation of StateTx with type: "%build) tx
        PayloadProjectionFailed tx p ->
            bprint ("Projection of payload is failed during validation of StateTx with type: "%build%", got prefix: "%build) tx p
        UnexpectedPayload p -> bprint ("Unexpected payload, prefixes: "%listF ", " build) p

-- | For each id' from passed set of ids get value' from state of computation.
-- HasAlt id id' satisfies that @id@ may be injected from @id'@.
-- If some value' couldn't be projected from corresponding value, the request will fail.
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

-- TODO Not Exist shall be different error from getEx Left ()
queryOne
  :: forall id id' value value' e ctx .
     ( Ord id, Ord id'
     , HasKeyValue id value id' value'
     , HasException e StatePException
     )
  => id' -> ERoComp e id value ctx (Maybe value')
queryOne id' = M.lookup id' <$> querySet (S.singleton id')

queryOneExists
  :: forall id id' value e ctx .
     ( Ord id, Ord id'
     , HasPrism id id'
     )
  => id' -> ERoComp e id value ctx Bool
queryOneExists id' = not . M.null <$> query (S.singleton (inj id'))

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

getCAOrDefault :: Default (ChgAccum ctx) => ChgAccumCtx ctx -> ChgAccum ctx
getCAOrDefault CANotInitialized   = def
getCAOrDefault (CAInitialized cA) = cA

withModifiedAccumCtx
  :: forall e id value ctx a .
    (HasException e (CSMappendException id), HasLens ctx (ChgAccumCtx ctx), Default (ChgAccum ctx))
  => ChangeSet id value
  -> ERoComp e id value ctx a
  -> ERoComp e id value ctx a
withModifiedAccumCtx chgSet comp = do
    ctxAcc <- getCAOrDefault @ctx . gett <$> ask
    newAccOrErr <- modifyAccum ctxAcc $ CAMChange chgSet
    case newAccOrErr of
        Left err   -> throwLocalError err
        Right (acc', _undo) -> local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp

initAccumCtx
  :: forall e id value ctx a .
    (HasException e StatePException, HasLens ctx (ChgAccumCtx ctx))
  => ChgAccum ctx
  -> ERoComp e id value ctx a
  -> ERoComp e id value ctx a
initAccumCtx acc' comp = do
    gett @_ @(ChgAccumCtx ctx) <$> ask >>= \case
        CAInitialized _ -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
        CANotInitialized -> local ( lensFor @ctx @(ChgAccumCtx ctx) .~ CAInitialized @ctx acc' ) comp

valid :: (Monoid a, Monad m) => m a
valid = pure mempty

validateIff :: forall e e1 m a . (Monoid a, MonadError e m,  HasReview e e1) => e1 -> Bool -> m a
validateIff e1 = bool (throwLocalError e1) valid

validateAll :: (Foldable f, MonadError e m, Container (f a)) => (Element (f a) -> e) -> (Element (f a) -> Bool) -> f a -> m ()
validateAll ex p ls = maybe (pure ()) (throwError . ex) (find (not . p) ls)

------------------------
-- Compute undo
------------------------

data StateModificationException id
    = UnexpectedKeyExists id
    | UnexpectedKeyAbsent id
    deriving (Show)

instance Buildable id => Buildable (StateModificationException id) where
    build = \case
        UnexpectedKeyExists i -> problemWithKey "exist" i
        UnexpectedKeyAbsent i -> problemWithKey "be absent" i
      where
        problemWithKey desc key =
            bprint ("Key "%build%" was not expected to "%stext%
                " during performed modification") key desc
