{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Simple
       (
         sumChangeSetDaa
       , sumChangeSetDaaM
       , sumChangeSetDaaU
       , SumChangeSet (..)
       , mappendStOrThrow
       , accumToDiff
       , modifySumChgSet
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Snowdrop.Core (CSMappendException (..), ChangeSet (..), IdSumPrefixed (..),
                                Prefix (..), StateP, StateR, ValueOp (..), changeSetToMap, csNew,
                                diffChangeSet, filterByPrefix, mappendChangeSet)
import           Snowdrop.Util

import           Snowdrop.Execution.DbActions.Types

mappendStOrThrow
    :: forall e id value m .
    ( Monad m
    , Ord id
    , IdSumPrefixed id
    , MonadState (SumChangeSet id value) m
    , HasException e (CSMappendException id)
    )
    => ChangeSet id value
    -> m (Either e ())
mappendStOrThrow chg = (flip modifySumChgSet chg) <$> get >>=
    either (pure . Left . inj) (\s -> put s $> Right ())

-- | SumChangeSet holds some change set which is sum of several ChangeSet
newtype SumChangeSet id value = SumChangeSet {unSumCS :: ChangeSet id value}
    deriving Show

instance Default (SumChangeSet id value) where
    def = SumChangeSet def

modifySumChgSet
    :: Ord id
    => SumChangeSet id value
    -> ChangeSet id value
    -> Either (CSMappendException id) (SumChangeSet id value)
modifySumChgSet (SumChangeSet cs1) cs2 = SumChangeSet <$> mappendChangeSet cs1 cs2

accumToDiff :: SumChangeSet id value -> ChangeSet id value
accumToDiff = unSumCS

queryAccum :: Ord id => SumChangeSet id value -> StateR id -> (StateR id, StateP id value)
queryAccum (SumChangeSet accum) reqIds = (reqIds', resp)
  where
    resp    = changeSetToMap accum `M.intersection` toDummyMap reqIds
    reqIds' = reqIds S.\\ M.keysSet (changeSet accum)

queryAccumOne :: Ord id => SumChangeSet id value -> id -> Maybe (ValueOp value)
queryAccumOne (SumChangeSet (ChangeSet csm)) = flip M.lookup csm

getNewKeys :: IdSumPrefixed id => SumChangeSet id value -> Prefix -> Map id value
getNewKeys (SumChangeSet cs) p = csNew $ filterByPrefix p cs

sumChangeSetDaaM
    :: forall id value m . (IdSumPrefixed id, Ord id, MonadCatch m)
    => DbAccessActions (SumChangeSet id value) id value m
    -> DbAccessActionsM (SumChangeSet id value) id value m
sumChangeSetDaaM daa = daaM
  where
    liftA' f a b = pure $ f a b
    daaM = DbAccessActionsM daa (liftA' modifyAccum)

    modifyAccum
      :: SumChangeSet id value
      -> OldestFirst [] (ChangeSet id value)
      -> Either (CSMappendException id) (OldestFirst [] (SumChangeSet id value))
    modifyAccum initScs (OldestFirst css) = OldestFirst <$> scss
      where
        scss = reverse . snd <$> foldM modScs (initScs, []) css
        modScs (scs, res) cs = (\scs' -> (scs', scs':res)) <$> scs `modifySumChgSet` cs

sumChangeSetDaaU
    :: forall id value undo m .
      (IdSumPrefixed id, Ord id, MonadCatch m, HasPrism undo (ChangeSet id value))
    => DbAccessActions (SumChangeSet id value) id value m
    -> DbAccessActionsU (SumChangeSet id value) undo id value m
sumChangeSetDaaU daa = daaU
  where
    daaU = DbAccessActionsU (sumChangeSetDaaM daa) modifyAccumU computeUndo

    modifyAccumU
      :: SumChangeSet id value
      -> NewestFirst [] undo
      -> m (Either (CSMappendException id) (SumChangeSet id value))
    modifyAccumU scs undos = do
        undos' <- maybe (throwM DbUndoProjectionError) pure (traverse proj undos)
        pure (modifyAccumU' scs undos')


    modifyAccumU'
      :: SumChangeSet id value
      -> NewestFirst [] (ChangeSet id value)
      -> Either (CSMappendException id) (SumChangeSet id value)
    modifyAccumU' scs (NewestFirst css) = foldM modifySumChgSet scs css

    computeUndo
        :: SumChangeSet id value
        -> SumChangeSet id value
        -> m (Either (CSMappendException id) undo)
    computeUndo scs@(SumChangeSet scs1) (SumChangeSet scs2) =
      either (pure . Left) ((fmap $ fmap inj) <$> computeUndoDo scs) (scs2 `diffChangeSet` scs1)

    computeUndoDo
        :: SumChangeSet id value
        -> ChangeSet id value
        -> m (Either (CSMappendException id) (ChangeSet id value))
    computeUndoDo ca (ChangeSet cs) = runExceptT $ do
        vals <- lift $ (daaGetter daa) ca (M.keysSet cs)
        let processOne m (k, valueop) = case (valueop, k `M.lookup` vals) of
              (New _, Nothing)      -> pure $ M.insert k Rem m
              (Upd _, Just v0)      -> pure $ M.insert k (Upd v0) m
              (NotExisted, Nothing) -> pure $ M.insert k NotExisted m
              (Rem, Just v0)        -> pure $ M.insert k (New v0) m
              _                     -> throwError (CSMappendException k)
        ChangeSet <$> foldM processOne mempty (M.toList cs)

sumChangeSetDaa
    :: (IdSumPrefixed id, Ord id, Applicative m)
    => (StateR id -> m (StateP id value))
    -> (forall b . Prefix -> b -> ((id, value) -> b -> b) -> m b)
    -> DbAccessActions (SumChangeSet id value) id value m
sumChangeSetDaa getterImpl iterImpl =
    DbAccessActions (chgAccumGetter getterImpl) (chgAccumIter iterImpl)

chgAccumIter
    :: (IdSumPrefixed id, Ord id, Applicative m)
    => (Prefix -> b -> ((id, value) -> b -> b) -> m b)
    -> SumChangeSet id value
    -> Prefix
    -> b
    -> ((id, value) -> b -> b)
    -> m b
chgAccumIter iter accum prefix initB foldF =
    let newKeysB = M.foldrWithKey (\i v r -> foldF (i, v) r) initB $ getNewKeys accum prefix
        newFoldF (i, val) b = case queryAccumOne accum i of
            Nothing         -> foldF (i, val) b
            Just Rem        -> b
            Just NotExisted -> b -- something strange happened -- TODO shall we throw error here ?
            Just (Upd newV) -> foldF (i, newV) b
            Just (New _)    -> b -- something strange happened
     in iter prefix newKeysB newFoldF

chgAccumGetter
    :: (IdSumPrefixed id, Ord id, Applicative m)
    => (StateR id -> m (StateP id value))
    -> SumChangeSet id value
    -> StateR id
    -> m (StateP id value)
chgAccumGetter getter accum reqIds =
    bool (unionStateP resp <$> getter reqIds') (pure resp) (null reqIds')
  where
    (reqIds', resp) = queryAccum accum reqIds
    unionStateP = M.unionWith (error "chgAccumGetter: unexpected overlap of keys")
