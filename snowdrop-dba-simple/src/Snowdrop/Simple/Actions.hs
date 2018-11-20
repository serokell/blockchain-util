{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Simple.Actions
    ( simpleDbActions
    , HMapLensEl (..)
    ) where

import           Universum

import           Control.Concurrent.STM.TVar (modifyTVar)
import           Control.Lens (at)
import qualified Data.Map as M
import           Data.Vinyl.Core (Rec (..))
import           Data.Vinyl.TypeLevel (AllConstrained, RecAll)

import           Snowdrop.Core (ChgAccum, HChangeSet, HChangeSetEl (..), Undo, ValueOp (..))
import           Snowdrop.Simple.SumChangeSet (SumChangeSet (..), sumChangeSetDaa,
                                               sumChangeSetDaaU)
import           Snowdrop.Dba (DIter', DbActionsException (..), DbApplyProof,
                                                     DbComponents, DbModifyActions (..),
                                                     IterAction (..))
import           Snowdrop.Hetero (ExnHKey, HIntersectable, HKey, HMap, HMapEl (..),
                                  HSet, HSetEl (..), HVal, OrdHKey)
import           Snowdrop.Util (IsEmpty (..), toDummyMap)

simpleDbActions
    :: forall st conf xs.
      ( DbComponents conf ~ xs
      , DbApplyProof conf ~ ()
      , ChgAccum conf ~ SumChangeSet xs
      , Undo conf ~ HChangeSet xs
      , AllConstrained ExnHKey xs
      , AllConstrained OrdHKey xs
      , Semigroup (HMap xs)
      , RecAll HMapEl xs IsEmpty
      , RecAll HSetEl xs IsEmpty
      , HIntersectable xs xs
      )
    => st
    -> Rec (HMapLensEl st) xs
    -> STM (DbModifyActions conf STM)
simpleDbActions initSt lenses =
    fmap mkActions $ newTVar initSt
  where
    mkActions var = DbModifyActions (mkAccessActions var) (applyImpl var lenses)
    mkAccessActions var =
        sumChangeSetDaaU $ sumChangeSetDaa (queryImpl var lenses) (iterImpl var lenses)

    -- Queries
    queryImpl
      :: forall xs'.
        ( AllConstrained ExnHKey xs'
        )
       =>  TVar st -> Rec (HMapLensEl st) xs' -> HSet xs' -> STM (HMap xs')
    queryImpl _ RNil RNil = pure RNil
    queryImpl tvar (HMapLensEl lens :& lensRest) (q :& qRest) = do
        st <- readTVar tvar
        let subSt = st ^. lens
        let resp1 = queryFromMap q subSt
        (resp1 :&) <$> queryImpl tvar lensRest qRest

    iterImpl
      :: forall xs'.
        ( AllConstrained ExnHKey xs'
        )
       => TVar st -> Rec (HMapLensEl st) xs' -> DIter' xs' STM
    iterImpl _ RNil                        = RNil
    iterImpl var (HMapLensEl lens :& rest) = iterAction var lens :& iterImpl var rest

    -- Modifies
    applyImpl
      :: forall xs'.
        ( AllConstrained ExnHKey xs'
        )
       => TVar st -> Rec (HMapLensEl st) xs' -> SumChangeSet xs' -> STM ()
    applyImpl _ RNil (SumChangeSet RNil) = pure ()
    applyImpl var (HMapLensEl lens :& lensRest) (SumChangeSet (cs :& csRest)) = do
        applyChSetEl var lens cs
        applyImpl var lensRest (SumChangeSet csRest)

newtype HMapLensEl st t =
  HMapLensEl { unHMapLensEl :: Lens' st (Map (HKey t) (HVal t)) }

iterAction :: TVar st -> Lens' st (Map (HKey t) (HVal t)) -> IterAction STM t
iterAction var lens = IterAction $ \b foldF -> foldl' foldF b . M.toList . view lens <$> readTVar var

queryFromMap :: OrdHKey t => HSetEl t -> Map (HKey t) (HVal t) -> HMapEl t
queryFromMap (HSetEl s) mp = HMapEl $ mp `M.intersection` (toDummyMap s)

applyChSetEl
    :: forall t st . (ExnHKey t)
    => TVar st
    -> Lens' st (Map (HKey t) (HVal t)) -> HChangeSetEl t -> STM ()
applyChSetEl var ls =
    mapM_ (\(k, v) -> performActionWithTVar var (ls . at k) (applyException k) v) . M.toList . unHChangeSetEl

applyException :: (Show key, MonadThrow m) => key -> ValueOp v -> Maybe v -> m void
applyException key vOp val =
    throwM $ DbApplyException $ "Error applying operation " <> vOpShow vOp <> " to value " <> valShown <> " (key: " <> show key <> ")"
  where
    valShown = maybe "Nothing" (const "Just _") val
    vOpShow (Upd _)    = "Upd _"
    vOpShow (New _)    = "New _"
    vOpShow Rem        = "Rem"
    vOpShow NotExisted = "NotExisted"

performActionWithTVar
    :: TVar var
    -> Lens' var (Maybe value)
    -> (ValueOp value -> Maybe value -> STM ())
    -> ValueOp value
    -> STM ()
performActionWithTVar tvar ln onEx valop = (valop,) . view ln <$> readTVar tvar >>= \case
    (Rem, Just _)         -> setVal Nothing
    (New v, Nothing)      -> setVal (Just v)
    (Upd v, Just _)       -> setVal (Just v)
    (NotExisted, Nothing) -> pure ()
    (_, val)              -> onEx valop val
  where
    setVal = modifyTVar tvar . set ln
