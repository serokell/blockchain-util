{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Simple
       (
         sumChangeSetDBA
       , SumChangeSet (..)
       , mappendStOrThrow
       , accumToDiff
       , modifySumChgSet
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import qualified Data.ByteString as BS
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Snowdrop.Core (CSMappendException (..), ChangeSet (..), ChgAccumModifier (..),
                                ChgAccumOps (..), IdSumPrefixed (..), Prefix (..), StateP, StateR,
                                Undo (..), ValueOp (..), changeSetToMap, csNew, filterByPrefix,
                                mappendChangeSet)
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

-- | ghci> queryAccum (SumChangeSet (ChangeSet (Map.fromList [(1, Rem), (2, New "A"), (5, Rem), (6, Upd "7")]))) (S.fromList [1,2,3,4,5,6,7])
-- | (fromList [3,4,7],fromList [(2,"A"),(6,"7")]))
queryAccum :: Ord id => SumChangeSet id value -> StateR id -> (StateR id, StateP id value)
queryAccum (SumChangeSet accum) reqIds = (reqIds', resp)
  where
    resp    = changeSetToMap accum `M.intersection` toDummyMap reqIds
    reqIds' = reqIds S.\\ M.keysSet (changeSet accum)

queryAccumOne :: Ord id => SumChangeSet id value -> id -> Maybe (ValueOp value)
queryAccumOne (SumChangeSet (ChangeSet csm)) = flip M.lookup csm

getNewKeys :: IdSumPrefixed id => SumChangeSet id value -> Prefix -> Map id value
getNewKeys (SumChangeSet cs) p = csNew $ filterByPrefix p cs

instance Ord id => ChgAccumOps id value (SumChangeSet id value)
  where
    modifyAccum accum = \case
        CAMChange cs -> processCS cs
        CAMRevert (Undo cs _sn) -> processCS cs
      where
        processCS :: ChangeSet id value -> Either (CSMappendException id) (SumChangeSet id value, Undo id value)
        processCS cs' = (,) <$> accum `modifySumChgSet` cs' <*> (flip Undo BS.empty <$> computeUndo accum cs')

        computeUndo
            :: forall id value
            .
            ( Ord id
            )
            => SumChangeSet id value
            -> ChangeSet id value
            -> Either (CSMappendException id) (ChangeSet id value)
        computeUndo (SumChangeSet accum) (ChangeSet cs) = let
            vals = changeSetToMap accum -- <- I'm not sure about this line
            processOne m (k, valueop) = case (valueop, k `M.lookup` vals) of
                  (New _, Nothing)      -> pure $ M.insert k Rem m
                  (Upd _, Just v0)      -> pure $ M.insert k (Upd v0) m
                  (NotExisted, Nothing) -> pure $ M.insert k NotExisted m
                  (Rem, Just v0)        -> pure $ M.insert k (New v0) m
                  _                     -> throwError (CSMappendException k)
            in ChangeSet <$> foldM processOne mempty (M.toList cs)

sumChangeSetDBA
    :: forall id value m . (IdSumPrefixed id, Ord id, MonadCatch m)
    => (StateR id -> m (StateP id value))
    -> (forall b. Prefix -> b -> ((id, value) -> b -> b) -> m b)
    -> DbAccessActions (SumChangeSet id value) id value m
sumChangeSetDBA getImpl iterImpl =
    DbAccessActions
      (chgAccumGetter getImpl)
      (chgAccumIter iterImpl)

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
