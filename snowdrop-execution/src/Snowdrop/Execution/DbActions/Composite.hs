module Snowdrop.Execution.DbActions.Composite
       ( constructCompositeActions
       , CompositeChgAccum (..)
       ) where

import           Universum

import           Data.Default (Default (def))
import           Data.Vinyl.TypeLevel (type (++))

import           Snowdrop.Core (ChgAccumModifier (..), Undo (..), downcastUndo)

import           Snowdrop.Execution.DbActions.Types (DbAccessActions (..))
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
    ( Monad m
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
  , daaIter = \(CompositeChgAccum prim sec) -> liftA2 happend (daaIter dbaP prim) (daaIter dbaS sec)
  , daaModifyAccum = \compChgAcc cm -> runExceptT $ case cm of
        CAMChange cs           -> processCS compChgAcc (CAMChange $ hdowncast cs) (CAMChange $ hdowncast cs)
        CAMRevert undo ->
            processCS compChgAcc
                      (CAMRevert $ downcastUndo undo)
                      (CAMRevert $ downcastUndo undo)
  }
  where
    processCS (CompositeChgAccum prim sec) md1 md2 = do
        (accP, undoP) <- ExceptT $ daaModifyAccum dbaP prim md1
        (accS, undoS) <- ExceptT $ daaModifyAccum dbaS sec md2
        pure (CompositeChgAccum accP accS, mergeUndos undoP undoS)

    mergeUndos (Undo csP snP) (Undo csS snS) = Undo (csP `happend` csS) (snP `happend` snS)
