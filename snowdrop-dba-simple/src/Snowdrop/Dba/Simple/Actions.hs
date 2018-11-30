{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Dba.Simple.Actions
    ( simpleDbActions
    , simpleDbActions'
    , HMapLensEl (..)
    ) where

import           Universum hiding (Compose)

import           Control.Concurrent.STM.TVar (modifyTVar)
import           Control.Lens (at, iso)
import qualified Data.Map as M
import           Data.Vinyl (RApply (..), RMap (..), RPureConstrained (..), Rec (..), rlens,
                             rtraverse, (<<*>>))
import           Data.Vinyl.Lens (type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

import           Snowdrop.Core (ChgAccum, HChangeSet, HChangeSetEl (..), SumChangeSet (..),
                                Undo, ValueOp (..))
import           Snowdrop.Dba.Simple.SumChangeSet (sumChangeSetDaa, sumChangeSetDaaU)
import           Snowdrop.Dba.Base (DbActionsException (..), DbApplyProof,
                                    DbComponents, DbModifyActions (..), IterAction (..))
import           Snowdrop.Hetero (ExnHKey, ExnHKeyConstr, HElemFlipped, HIntersectable, HKey, HMap, HMapEl (..),
                                  HSetEl (..), HVal, OrdHKey, rliftA2)
import           Snowdrop.Util (IsEmpty (..), toDummyMap)

type SimpleDbActionsConstr conf xs =
      ( DbComponents conf ~ xs
      , DbApplyProof conf ~ ()
      , ChgAccum conf ~ SumChangeSet xs
      , Undo conf ~ HChangeSet xs
      , Semigroup (HMap xs)
      , RecAll HMapEl xs IsEmpty
      , RecAll HSetEl xs IsEmpty
      , HIntersectable xs xs
      , RMap xs
      , RPureConstrained OrdHKey xs
      , RPureConstrained ExnHKey xs
      , RApply xs
      , ExnHKeyConstr xs
      )

simpleDbActions'
    :: forall conf xs.
      ( SimpleDbActionsConstr conf xs
      , RPureConstrained (HElemFlipped xs) xs
      )
    => HMap xs
    -> STM (DbModifyActions conf STM)
simpleDbActions' = flip simpleDbActions $
    rpureConstrained @(HElemFlipped xs) mkHMapLensEl
  where
    mkHMapLensEl :: forall t . t ∈ xs => HMapLensEl (HMap xs) t
    mkHMapLensEl = HMapLensEl $ rlens @t . iso unHMapEl HMapEl

simpleDbActions
    :: forall st conf xs.
      SimpleDbActionsConstr conf xs
    => st
    -> Rec (HMapLensEl st) xs
    -> STM (DbModifyActions conf STM)
simpleDbActions initSt lenses =
    fmap mkActions $ newTVar initSt
  where
    mkActions var = DbModifyActions (mkAccessActions var) (applyImpl var)
    mkAccessActions var =
        sumChangeSetDaaU $ sumChangeSetDaa (queryImpl var) (iterImpl var)

    queryImpl tvar q = (rliftA2 @OrdHKey queryFromMap <<*>> q <<*>>) <$> rtraverse (readSubSt tvar) lenses
    iterImpl tvar = rmap (iterAction . readSubSt tvar) lenses

    readSubSt tvar (HMapLensEl lens) = HMapEl . view lens <$> readTVar tvar

    applyImpl var (SumChangeSet css) =
        void $ rtraverse (($> Const ()) . getConst) $
        rliftA2 @ExnHKey (Const ... applyChSetEl var) <<*>> lenses <<*>> css

newtype HMapLensEl st t =
    HMapLensEl { unHMapLensEl :: Lens' st (Map (HKey t) (HVal t)) }

iterAction :: STM (HMapEl t) -> IterAction STM t
iterAction readHMap = IterAction $ \b foldF -> foldl' foldF b . M.toList . unHMapEl <$> readHMap

queryFromMap :: OrdHKey t => HSetEl t -> HMapEl t -> HMapEl t
queryFromMap (HSetEl s) (HMapEl mp) = HMapEl $ mp `M.intersection` (toDummyMap s)

applyChSetEl
    :: forall t st . (ExnHKey t)
    => TVar st
    -> HMapLensEl st t -> HChangeSetEl t -> STM ()
applyChSetEl var (HMapLensEl ls) =
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
