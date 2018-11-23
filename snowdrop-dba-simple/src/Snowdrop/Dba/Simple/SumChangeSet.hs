{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snowdrop.Dba.Simple.SumChangeSet
       (
         sumChangeSetDaa
       , sumChangeSetDaaM
       , sumChangeSetDaaU
       , SimpleConf
       , SimpleConf'
       ) where

import           Universum

import qualified Data.Map.Strict as M
import           Data.Vinyl (Rec (..))
import           Data.Vinyl.TypeLevel (AllConstrained, RecAll)

import           Snowdrop.Core (CSMappendException (..), ChgAccum, HChangeSet, HChangeSetEl (..),
                                MappendHChSet, SumChangeSet (..), ValueUpd (..), Undo, ValueOp (..), getVal,
                                csNew, diffChangeSet, hChangeSetToHMap, hChangeSetToHSet, modifySumChgSet)
import           Snowdrop.Dba.Base (DGetter, DGetter', DIter', DbAccessActions (..), DbAccessActionsM (..),
                                    DbAccessActionsU (..), DbApplyProof, DbComponents, IterAction (..))
import           Snowdrop.Hetero (ExnHKey, HIntersectable, HMap, HSet, HMapEl (..), HSetEl,
                                  OrdHKey, hdifference, hintersect, rAllEmpty)
import           Snowdrop.Util (IsEmpty (..), NewestFirst (..), OldestFirst (..))

data SimpleConf (xs :: [*])

type instance ChgAccum (SimpleConf xs) = SumChangeSet xs
type instance Undo (SimpleConf xs) = HChangeSet xs
type instance DbComponents (SimpleConf xs) = xs
type instance DbApplyProof (SimpleConf xs) = ()


-- | Simple configuration with phantom type parameter @t@
data SimpleConf' (t :: k) (xs :: [*])

type instance ChgAccum (SimpleConf' t xs) = SumChangeSet xs
type instance Undo (SimpleConf' t xs) = HChangeSet xs
type instance DbComponents (SimpleConf' t xs) = xs
type instance DbApplyProof (SimpleConf' t xs) = ()


querySumChSet :: HIntersectable xs xs => SumChangeSet xs -> HSet xs -> (HSet xs, HMap ValueOp xs)
querySumChSet (SumChangeSet accum) reqIds = (reqIds', resp)
  where
    resp    = hChangeSetToHMap accum `hintersect` reqIds
    reqIds' = reqIds `hdifference` accum

----------------------------------------------------------------------------
-- SumChangeSet DbActions
----------------------------------------------------------------------------

-- | Compute undo and verify change is valid for being applied to state
computeHChSetUndo
  :: forall conf m xs.
     ( Monad m
     , MappendHChSet xs
     , ChgAccum conf ~ SumChangeSet xs
     , xs ~ DbComponents conf
     )
  => DGetter conf m
  -> SumChangeSet xs
  -> HChangeSet xs
  -> m (Either CSMappendException (HChangeSet xs))
computeHChSetUndo getter sch chs = do
    vals <- getter sch (hChangeSetToHSet chs)
    pure $ computeHChSetUndo' vals chs
  where
    computeHChSetUndo'
      :: forall xs1 . MappendHChSet xs1
      => HMap ValueOp xs1
      -> HChangeSet xs1
      -> Either CSMappendException (HChangeSet xs1)
    computeHChSetUndo' RNil RNil        = pure RNil
    computeHChSetUndo' (vals :& valxs) (ch :& ys) =
        case computeUndo vals ch of
            Left e    -> Left e
            Right res -> (res :&) <$> computeHChSetUndo' valxs ys

    computeUndo
        :: ExnHKey t
        => HMapEl ValueOp t
        -> HChangeSetEl t
        -> Either CSMappendException (HChangeSetEl t)
    computeUndo (HMapEl vals) (HChangeSetEl cs) =
        let processOne m (k, valueop) = case (valueop, k `M.lookup` vals') of
              (New _, Nothing)      -> okinsert Rem
              -- FIXME (Replace)?
              (Upd _, Just v0)      -> okinsert (Upd (Replace v0))
              (NotExisted, Nothing) -> okinsert NotExisted
              (Rem, Just v0)        -> okinsert (New v0)
              _                     -> Left (CSMappendException k)
              where okinsert v = Right $ M.insert k v m
       in HChangeSetEl <$> foldM processOne mempty (M.toList cs)
      where
        vals' = M.map getVal vals
      {- Alternative impl
        let processOne m (k, valueop) = case (valueop, k `M.lookup` vals) of
              (New _, Nothing)      -> okinsert Rem
              -- (Upd _, Just v0)      -> pure $ M.insert k (Upd v0) m
              -- FIXME! Put correct logic here
              (Upd _, Just v0)      -> okinsert v0
              (NotExisted, Nothing) -> okinsert NotExisted
              -- (Rem, Just v0)        -> pure $ M.insert k (New v0) m
              -- FIXME! Put correct logic here
              (Rem, Just v0)        -> okinsert v0
              _                     -> Left (CSMappendException k)
              where okinsert v = Right $ M.insert k v m
        in HChangeSetEl <$> foldM processOne mempty (M.toList cs)
       -}

sumChangeSetDaa
    :: forall conf m xs .
       ( Monad m
       , MappendHChSet xs
       , HIntersectable xs xs
       , Semigroup (HMap ValueOp xs)
       , RecAll (HMapEl ValueOp) xs IsEmpty
       , RecAll HSetEl xs IsEmpty
       , ChgAccum conf ~ SumChangeSet xs
       , xs ~ DbComponents conf
       )
    => DGetter' xs m
    -> DIter' xs m
    -> DbAccessActions conf m
sumChangeSetDaa getterImpl iterImpl =
    DbAccessActions (chgAccumGetter getterImpl) (chgAccumIter iterImpl)

sumChangeSetDaaM
    :: forall conf m xs .
       ( Applicative m
       , MappendHChSet xs
       , HIntersectable xs xs
       , Semigroup (HMap ValueOp xs)
       , ChgAccum conf ~ SumChangeSet xs
       , xs ~ DbComponents conf
       )
    => DbAccessActions  conf m
    -> DbAccessActionsM conf m
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
    :: forall conf m undo xs .
       ( MonadThrow m
       , MappendHChSet xs
       , HIntersectable xs xs
       , Semigroup (HMap ValueOp xs)
       , undo ~ Undo conf
       , undo ~ HChangeSet xs
       , xs ~ DbComponents conf
       , ChgAccum conf ~ SumChangeSet xs
       )
    => DbAccessActions  conf m
    -> DbAccessActionsU conf m
sumChangeSetDaaU daa = daaU
  where
    daaU = DbAccessActionsU (sumChangeSetDaaM daa) (pure ... modifyAccumU) computeUndo

    modifyAccumU
      :: SumChangeSet xs
      -> NewestFirst [] (HChangeSet xs)
      -> Either CSMappendException (SumChangeSet xs)
    modifyAccumU scs (NewestFirst css) = foldM modifySumChgSet scs css

    computeUndo
        :: SumChangeSet xs
        -> SumChangeSet xs
        -> m (Either CSMappendException undo)
    computeUndo scs@(SumChangeSet scs1) (SumChangeSet scs2) =
        either (pure . Left) (computeUndoDo scs) (scs2 `diffChangeSet` scs1)

    computeUndoDo
        :: SumChangeSet xs
        -> HChangeSet xs
        -> m (Either CSMappendException (HChangeSet xs))
    computeUndoDo = computeHChSetUndo @conf (daaGetter daa)

chgAccumIter
    :: ( Monad m
       , AllConstrained OrdHKey xs
       )
    => DIter' xs m
    -> SumChangeSet xs
    -> m (DIter' xs m)
chgAccumIter RNil (SumChangeSet RNil) = pure RNil
chgAccumIter (iter' :& xs) (SumChangeSet (csel' :& ys)) =
    (chgAccumIter' iter' csel' :&) <$> chgAccumIter xs (SumChangeSet ys)
  where
    chgAccumIter' :: (OrdHKey t, Monad m2) => IterAction m2 t -> HChangeSetEl t -> IterAction m2 t
    chgAccumIter' iter ac'@(HChangeSetEl accum) = IterAction $ \initB foldF -> do
        let extractMin i m = do
                (k, v) <- M.lookupMin m
                (k, v) <$ guard (k <= i)
            newFoldF (b, newKeys) (i, val) =
                case (M.lookup i accum, extractMin i newKeys) of
                    (_,   Just (i', v')) -> newFoldF (foldF b (i', v'), M.deleteMin newKeys) (i, val)
                    (Nothing,         _) -> (foldF b (i, val) ,newKeys)
                    (Just Rem,        _) -> (b, newKeys)
                    (Just newV@(Upd _), _) -> (foldF b (i, newV), newKeys)

                    (Just NotExisted, _) -> (b, newKeys) -- TODO shall we throw error here ?
                    (Just (New _),    _) -> (b, newKeys) -- TODO shall we throw error here ?
        (b, remainedNewKeys) <- runIterAction iter (initB, M.map New $ csNew ac') newFoldF
        pure $ M.foldlWithKey' (curry . foldF) b remainedNewKeys

chgAccumGetter
    ::
      ( Applicative m
      , HIntersectable xs xs
      , Semigroup (HMap ValueOp xs)
      , RecAll HSetEl xs IsEmpty
      , RecAll (HMapEl ValueOp) xs IsEmpty
      )
    => DGetter' xs m
    -> SumChangeSet xs
    -> DGetter' xs m
chgAccumGetter getter accum reqIds =
    bool (unionStateP resp <$> getter reqIds') (pure resp) (rAllEmpty reqIds')
  where
    (reqIds', resp) = querySumChSet accum reqIds
    unionStateP x y =
        if rAllEmpty (x `hintersect` y) then x <> y
        else error "chgAccumGetter: unexpected overlap of keys"
