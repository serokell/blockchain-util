module Snowdrop.Execution.DbActions.Composite
       ( constructCompositeActions
       , CompositeChgAccum (..)
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.Default (Default (def))
import           Data.Vinyl.TypeLevel (type (++))

import           Snowdrop.Core (ChgAccumModifier (..), Undo (..))

import           Snowdrop.Execution.DbActions.Types (DbAccessActions (..), DbActionsException (..))
import           Snowdrop.Util

data CompositeChgAccum chgAccumPrimary chgAccumSecondary = CompositeChgAccum
    { ccaPrimary   :: chgAccumPrimary
    , ccaSecondary :: chgAccumSecondary
    }

instance (Default chgAccumPrimary, Default chgAccumSecondary)
         => Default (CompositeChgAccum chgAccumPrimary chgAccumSecondary) where
    def = CompositeChgAccum def def

constructCompositeActions
    :: forall caPrimary caSecondary components1 components2 m .
    ( MonadThrow m
    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbAccessActions caPrimary components1 m
    -> DbAccessActions caSecondary components2 m
    -> DbAccessActions (CompositeChgAccum caPrimary caSecondary) (components1 ++ components2) m
constructCompositeActions dbaP dbaS = DbAccessActions {
    daaGetter = \(CompositeChgAccum prim sec) reqs ->
          liftA2 happend (daaGetter dbaP prim (hdowncast reqs)) (daaGetter dbaS sec (hdowncast reqs))
  , daaIter = \(CompositeChgAccum prim sec) -> daaIter dbaP prim `happend` daaIter dbaS sec
  , daaModifyAccum = \compChgAcc cm -> runExceptT $ case cm of
        CAMChange cs           -> processCS compChgAcc (CAMChange . hdowncast) (CAMChange . hdowncast) cs
        CAMRevert (Undo cs sn) ->
            processCS compChgAcc (CAMRevert . flip Undo sn . hdowncast) (CAMRevert . flip Undo sn . hdowncast) cs
  }
  where
    processCS (CompositeChgAccum prim sec) f1 f2 cs = do
        (accP, undoP) <- ExceptT $ daaModifyAccum dbaP prim (f1 cs)
        (accS, undoS) <- ExceptT $ daaModifyAccum dbaS sec (f2 cs)
        (CompositeChgAccum accP accS,) <$> mergeUndos undoP undoS

    mergeUndos (Undo csP snP) (Undo csS snS) =
        if BS.null snP || BS.null snS
        then flip Undo (snP `BS.append` snS)
            <$> (pure $ csP `happend` csS)
        else throwM $ DbApplyException "Both undo contain non-empty snapshot in composite actions"
