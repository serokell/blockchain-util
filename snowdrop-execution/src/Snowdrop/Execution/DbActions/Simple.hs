{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Simple
       ( SumChangeSet (..)
       , sumChangeSetDBA
       , mappendStOrThrow
       , modifySumChgSet
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Vinyl.Core (Rec (..))

import           Snowdrop.Core (AvlRevisions, CSMappendException (..), ChgAccumModifier (..),
                                HChangeSet, HChangeSetEl (..), MappendHChSet, Undo (..),
                                ValueOp (..), csNew, hChangeSetToHMap, hChangeSetToHSet,
                                mappendChangeSet)
import           Snowdrop.Execution.DbActions.Types (DGetter, DGetter', DIter, DIter',
                                                     DbAccessActions (..), IterAction (..))
import           Snowdrop.Util

-- | SumChangeSet holds some change set which is sum of several ChangeSet
newtype SumChangeSet xs = SumChangeSet {unSumCS :: HChangeSet xs}

deriving instance Show (HChangeSet xs) => Show (SumChangeSet xs)

instance Default (HChangeSet xs) => Default (SumChangeSet xs) where
    def = SumChangeSet def

modifySumChgSet
    :: RecAll' xs ExnHKey
    => SumChangeSet xs
    -> HChangeSet xs
    -> Either CSMappendException (SumChangeSet xs)
modifySumChgSet (SumChangeSet cs1) cs2 = SumChangeSet <$> mappendChangeSet cs1 cs2

mappendStOrThrow
    :: forall e xs m .
    ( Monad m
    , MonadState (SumChangeSet xs) m
    , HasException e CSMappendException
    , MappendHChSet xs
    )
    => HChangeSet xs
    -> m (Either e ())
mappendStOrThrow chg = (flip modifySumChgSet chg) <$> get >>=
    either (pure . Left . inj) (\s -> put s $> Right ())

querySumChSet :: HIntersectable xs xs => SumChangeSet xs -> HSet xs -> (HSet xs, HMap xs)
querySumChSet (SumChangeSet accum) reqIds = (reqIds', resp)
  where
    resp    = hChangeSetToHMap accum `hintersect` reqIds
    reqIds' = reqIds `hdifference` accum

----------------------------------------------------------------------------
-- SumChangeSet DbActions
----------------------------------------------------------------------------

sumChangeSetDBA
    :: forall xs m .
       ( MonadCatch m
       , MappendHChSet xs
       , HIntersectable xs xs
       , Semigroup (HMap xs)
       , Default (AvlRevisions xs)
       )
    => DGetter' xs m
    -> DIter' xs m
    -> DbAccessActions (SumChangeSet xs) xs m
sumChangeSetDBA getImpl iterImpl = DbAccessActions getter modifySumChgSetA iterer
  where
    getter = chgAccumGetter getImpl
    iterer = chgAccumIter iterImpl

    modifySumChgSetA accum = \case
        CAMChange cs -> processCS cs
        CAMRevert (Undo cs _sn) -> processCS cs
      where
        processCS cs' =
          (liftA2 (,) $ accum `modifySumChgSet` cs')
            <$> runExceptT (flip Undo def <$> computeHChSetUndo getter accum cs')

-- | Compute undo and verify change is valid for being applied to state
computeHChSetUndo
  :: ( Monad m
     , MappendHChSet xs
     )
  => DGetter (SumChangeSet xs) xs m
  -> SumChangeSet xs
  -> HChangeSet xs
  -> ExceptT CSMappendException m (HChangeSet xs)
computeHChSetUndo getter sch chs = do
    vals <- lift $ getter sch (hChangeSetToHSet chs)
    ExceptT $ pure $ computeHChSetUndo' vals chs
  where
    computeHChSetUndo'
      :: MappendHChSet xs
      => HMap xs
      -> HChangeSet xs
      -> Either CSMappendException (HChangeSet xs)
    computeHChSetUndo' RNil RNil        = pure RNil
    computeHChSetUndo' (vals :& valxs) (ch :& ys) =
        case computeUndo vals ch of
            Left e    -> Left e
            Right res -> (res :&) <$> computeHChSetUndo' valxs ys

    computeUndo
        :: ExnHKey t
        => HMapEl t
        -> HChangeSetEl t
        -> Either CSMappendException (HChangeSetEl t)
    computeUndo (HMapEl vals) (HChangeSetEl cs) = do
        let processOne m (k, valueop) = case (valueop, k `M.lookup` vals) of
              (New _, Nothing)      -> pure $ M.insert k Rem m
              (Upd _, Just v0)      -> pure $ M.insert k (Upd v0) m
              (NotExisted, Nothing) -> pure $ M.insert k NotExisted m
              (Rem, Just v0)        -> pure $ M.insert k (New v0) m
              _                     -> throwError (CSMappendException k)
        HChangeSetEl <$> foldM processOne mempty (M.toList cs)

chgAccumIter
    :: (Applicative m, RecAll' xs OrdHKey)
    => DIter' xs m
    -> DIter (SumChangeSet xs) xs m
chgAccumIter RNil (SumChangeSet RNil) = pure RNil
chgAccumIter (iter' :& xs) (unSumCS -> csel' :& ys) =
    (chgAccumIter' iter' csel' :&) <$> chgAccumIter xs (SumChangeSet ys)
  where
    chgAccumIter' :: OrdHKey t => IterAction m t -> HChangeSetEl t -> IterAction m t
    chgAccumIter' iter ac'@(HChangeSetEl accum) = IterAction $ \initB foldF -> do
        let newKeysB = M.foldrWithKey (\i v r -> foldF (i, v) r) initB $ csNew ac'
            newFoldF (i, val) b = case M.lookup i accum of
                Nothing         -> foldF (i, val) b
                Just Rem        -> b
                Just NotExisted -> b -- TODO shall we throw error here ?
                Just (Upd newV) -> foldF (i, newV) b
                Just (New _)    -> b -- TODO shall we throw error here?
        runIterAction iter newKeysB newFoldF
chgAccumIter _ _ = error "Absurd. Lol, without this I get non exhaustive pattern matching "

chgAccumGetter
    :: (Applicative m, HIntersectable xs xs, Semigroup (HMap xs))
    => DGetter' xs m
    -> DGetter (SumChangeSet xs) xs m
chgAccumGetter getter accum reqIds =
    bool (unionStateP resp <$> getter reqIds') (pure resp) (rnull reqIds')
  where
    (reqIds', resp) = querySumChSet accum reqIds
    unionStateP x y =
        if rnull (x `hintersect` y) then x <> y
        else error "chgAccumGetter: unexpected overlap of keys"
