module Snowdrop.Execution.DbActions.Composite
       (
         constructCompositeDaa
       , constructCompositeDaaM
       , constructCompositeDaaU
       , CompositeChgAccum (..)
       ) where

import           Universum

import           Data.Default (Default (def))
import           Data.Vinyl.TypeLevel (type (++))

import           Snowdrop.Core (CSMappendException)
import           Snowdrop.Execution.DbActions.Types
import           Snowdrop.Util (HDownCastable, NewestFirst (..), NotIntersect, OldestFirst (..),
                                happend, hdowncast)

data CompositeChgAccum chgAccumPrimary chgAccumSecondary = CompositeChgAccum
    { ccaPrimary   :: chgAccumPrimary
    , ccaSecondary :: chgAccumSecondary
    }

instance (Default chgAccumPrimary, Default chgAccumSecondary)
         => Default (CompositeChgAccum chgAccumPrimary chgAccumSecondary) where
    def = CompositeChgAccum def def

constructCompositeDaa
    :: forall caPrimary caSecondary components1 components2 m .
    ( Monad m
    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbAccessActions caPrimary components1 m
    -> DbAccessActions caSecondary components2 m
    -> DbAccessActions (CompositeChgAccum caPrimary caSecondary) (components1 ++ components2) m
constructCompositeDaa dbaP dbaS = DbAccessActions {
    daaGetter = \(CompositeChgAccum prim sec) reqs ->
          liftA2 happend (daaGetter dbaP prim (hdowncast reqs)) (daaGetter dbaS sec (hdowncast reqs))
  , daaIter = \(CompositeChgAccum prim sec) -> liftA2 happend (daaIter dbaP prim) (daaIter dbaS sec)
  }

constructCompositeDaaM
    :: forall caPrimary caSecondary components1 components2 m .
    ( Monad m
    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbAccessActionsM caPrimary components1 m
    -> DbAccessActionsM caSecondary components2 m
    -> DbAccessActionsM (CompositeChgAccum caPrimary caSecondary) (components1 ++ components2) m
constructCompositeDaaM dbaP dbaS = DbAccessActionsM {
    daaAccess = constructCompositeDaa (daaAccess dbaP) (daaAccess dbaS)
  , daaModifyAccum = modifyAccum
  }
  where
    modifyAccum (CompositeChgAccum prim sec) (OldestFirst css) =
        liftA2 glue <$> daaModifyAccum dbaP prim cssP <*> daaModifyAccum dbaS sec cssS
      where
        cssP = OldestFirst $ hdowncast <$> css
        cssS = OldestFirst $ hdowncast <$> css
        glue (OldestFirst accsP) (OldestFirst accsS) =
            OldestFirst $ uncurry CompositeChgAccum <$> zip accsP accsS

-- TODO re-consider how undo is constructed (remove constructUndo/splitUndo)
constructCompositeDaaU
  :: forall caPrimary caSecondary undoPrimary undoSecondary undo components1 components2 m .
    ( Monad m
    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbAccessActionsU caPrimary undoPrimary components1 m
    -> DbAccessActionsU caSecondary undoSecondary components2 m
    -> (undoPrimary -> undoSecondary -> undo)
    -> (undo -> (undoPrimary, undoSecondary))
    -> DbAccessActionsU (CompositeChgAccum caPrimary caSecondary) undo (components1 ++ components2) m
constructCompositeDaaU dbaP dbaS constructUndo splitUndo = DbAccessActionsU {
    daaAccessM = constructCompositeDaaM (daaAccessM dbaP) (daaAccessM dbaS)
  , daaModifyAccumUndo = modifyAccumU
  , daaComputeUndo = computeUndo
  }
  where

    modifyAccumU
      :: CompositeChgAccum caPrimary caSecondary
      -> NewestFirst [] undo
      -> m (Either CSMappendException (CompositeChgAccum caPrimary caSecondary))
    modifyAccumU (CompositeChgAccum cP cS) (NewestFirst us) =
        liftA2 CompositeChgAccum <$> daaModifyAccumUndo dbaP cP usP <*> daaModifyAccumUndo dbaS cS usS
      where
        (usP, usS) = bimap NewestFirst NewestFirst $ unzip $ splitUndo <$> us

    computeUndo
      :: CompositeChgAccum caPrimary caSecondary
      -> CompositeChgAccum caPrimary caSecondary
      -> m (Either CSMappendException undo)
    computeUndo (CompositeChgAccum cP cS) (CompositeChgAccum cP' cS') =
        liftA2 constructUndo <$> daaComputeUndo dbaP cP cP' <*> daaComputeUndo dbaS cS cS'
