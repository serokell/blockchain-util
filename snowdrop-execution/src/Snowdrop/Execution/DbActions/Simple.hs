{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Execution.DbActions.Simple
       (
         sumChangeSetDaa
       , sumChangeSetDaaM
       , sumChangeSetDaaU
       , SumChangeSet (..)
       , mappendStOrThrow
       , modifySumChgSet
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Vinyl.Core (Rec (..))

import           Snowdrop.Core (CSMappendException (..), HChangeSet, HChangeSetEl (..),
                                MappendHChSet, ValueOp (..), csNew, diffChangeSet, hChangeSetToHMap,
                                hChangeSetToHSet, mappendChangeSet)
import           Snowdrop.Execution.DbActions.Types (DGetter, DGetter', DIter, DIter',
                                                     DbAccessActions (..), DbAccessActionsM (..),
                                                     DbAccessActionsU (..), DbActionsException (..),
                                                     IterAction (..))
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

-- | Compute undo and verify change is valid for being applied to state
computeHChSetUndo
  :: ( Monad m
     , MappendHChSet xs
     )
  => DGetter (SumChangeSet xs) xs m
  -> SumChangeSet xs
  -> HChangeSet xs
  -> m (Either CSMappendException (HChangeSet xs))
computeHChSetUndo getter sch chs = do
    vals <- getter sch (hChangeSetToHSet chs)
    pure $ computeHChSetUndo' vals chs
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

sumChangeSetDaa
    :: forall xs m .
       ( Applicative m
       , MappendHChSet xs
       , HIntersectable xs xs
       , Semigroup (HMap xs)
       )
    => DGetter' xs m
    -> DIter' xs m
    -> DbAccessActions (SumChangeSet xs) xs m
sumChangeSetDaa getterImpl iterImpl =
    DbAccessActions (chgAccumGetter getterImpl) (chgAccumIter iterImpl)

sumChangeSetDaaM
    :: forall xs m .
       ( Applicative m
       , MappendHChSet xs
       , HIntersectable xs xs
       , Semigroup (HMap xs)
       )
    => DbAccessActions  (SumChangeSet xs) xs m
    -> DbAccessActionsM (SumChangeSet xs) xs m
sumChangeSetDaaM daa = DbAccessActionsM daa (pure ... modifyAccum)
  where

    modifyAccum
      :: SumChangeSet xs
      -> OldestFirst [] (HChangeSet xs)
      -> Either CSMappendException (OldestFirst [] (SumChangeSet xs))
    modifyAccum initScs (OldestFirst css) = OldestFirst <$> scss
      where
        scss = reverse . snd <$> foldM modScs (initScs, []) css
        modScs (scs, res) cs = (\scs' -> (scs', scs':res)) <$> scs `modifySumChgSet` cs

sumChangeSetDaaU
    :: forall xs undo m .
       ( MonadThrow m
       , MappendHChSet xs
       , HIntersectable xs xs
       , Semigroup (HMap xs)
       , HasPrism undo (HChangeSet xs)
       )
    => DbAccessActions  (SumChangeSet xs) xs m
    -> DbAccessActionsU (SumChangeSet xs) undo xs m
sumChangeSetDaaU daa = daaU
  where
    daaU = DbAccessActionsU (sumChangeSetDaaM daa) modifyAccumU computeUndo

    modifyAccumU
      :: SumChangeSet xs
      -> NewestFirst [] undo
      -> m (Either CSMappendException (SumChangeSet xs))
    modifyAccumU scs undos = do
        undos' <- maybe (throwM DbUndoProjectionError) pure (traverse projUndo undos)
        pure (modifyAccumU' scs undos')

    projUndo :: undo -> Maybe (HChangeSet xs)
    projUndo = proj

    modifyAccumU'
      :: SumChangeSet xs
      -> NewestFirst [] (HChangeSet xs)
      -> Either CSMappendException (SumChangeSet xs)
    modifyAccumU' scs (NewestFirst css) = foldM modifySumChgSet scs css

    computeUndo
        :: SumChangeSet xs
        -> SumChangeSet xs
        -> m (Either CSMappendException undo)
    computeUndo scs@(SumChangeSet scs1) (SumChangeSet scs2) =
        either (pure . Left) ((fmap $ fmap injUndo) <$> computeUndoDo scs) (scs2 `diffChangeSet` scs1)

    injUndo :: HChangeSet xs -> undo
    injUndo = inj

    computeUndoDo
        :: SumChangeSet xs
        -> HChangeSet xs
        -> m (Either CSMappendException (HChangeSet xs))
    computeUndoDo = computeHChSetUndo (daaGetter daa)

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
