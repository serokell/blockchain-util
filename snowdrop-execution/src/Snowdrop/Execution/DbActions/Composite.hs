module Snowdrop.Execution.DbActions.Composite
       (
         constructCompositeDaa
       , constructCompositeDaaM
       , constructCompositeDaaU
       , constructCompositeDma
       , CompositeChgAccum (..)
       , CompositeUndo (..)
       , CompositeConf
       , Conf1
       , Conf2
       ) where

import           Universum

import           Data.Default (Default (def))
import           Data.Vinyl.TypeLevel (type (++))

import           Snowdrop.Core (CSMappendException, ChgAccum, Undo)
import           Snowdrop.Execution.DbActions.Types
import           Snowdrop.Hetero (HDownCastable, NotIntersect, happend, hdowncast)
import           Snowdrop.Util (NewestFirst (..), OldestFirst (..))

data CompositeConf conf1 conf2

type instance Undo (CompositeConf conf1 conf2) = CompositeUndo conf1 conf2
type instance ChgAccum (CompositeConf conf1 conf2) = CompositeChgAccum conf1 conf2
type instance DbComponents (CompositeConf conf1 conf2) = DbComponents conf1 ++ DbComponents conf2
type instance DbApplyProof (CompositeConf conf1 conf2) = (DbApplyProof conf1, DbApplyProof conf2)

type family Conf1 a where
    Conf1 (CompositeConf conf1 conf2) = conf1

type family Conf2 a where
    Conf2 (CompositeConf conf1 conf2) = conf2

data CompositeUndo conf1 conf2 = CompositeUndo
    { cuPrimary   :: Undo conf1
    , cuSecondary :: Undo conf2
    }

data CompositeChgAccum conf1 conf2 = CompositeChgAccum
    { ccaPrimary   :: ChgAccum conf1
    , ccaSecondary :: ChgAccum conf2
    }

instance (Default (ChgAccum conf1), Default (ChgAccum conf2))
         => Default (CompositeChgAccum conf1 conf2) where
    def = CompositeChgAccum def def

constructCompositeDaa
    :: forall conf1 conf2 m components1 components2 .
    ( Monad m
    , components1 ~ DbComponents conf1
    , components2 ~ DbComponents conf2

    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbAccessActions conf1 m
    -> DbAccessActions conf2 m
    -> DbAccessActions (CompositeConf conf1 conf2) m
constructCompositeDaa dbaP dbaS = DbAccessActions {
    daaGetter = \(CompositeChgAccum prim sec) reqs ->
          liftA2 happend (daaGetter dbaP prim (hdowncast reqs)) (daaGetter dbaS sec (hdowncast reqs))
  , daaIter = \(CompositeChgAccum prim sec) -> liftA2 happend (daaIter dbaP prim) (daaIter dbaS sec)
  }

constructCompositeDaaM
    :: forall conf1 conf2 m components1 components2 .
    ( Monad m
    , components1 ~ DbComponents conf1
    , components2 ~ DbComponents conf2

    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbAccessActionsM conf1 m
    -> DbAccessActionsM conf2 m
    -> DbAccessActionsM (CompositeConf conf1 conf2) m
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

constructCompositeDaaU
    :: forall conf1 conf2 m components1 components2 .
    ( Monad m
    , components1 ~ DbComponents conf1
    , components2 ~ DbComponents conf2

    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbAccessActionsU conf1 m
    -> DbAccessActionsU conf2 m
    -> DbAccessActionsU (CompositeConf conf1 conf2) m
constructCompositeDaaU dbaP dbaS = DbAccessActionsU {
    daaAccessM = constructCompositeDaaM (daaAccessM dbaP) (daaAccessM dbaS)
  , daaModifyAccumUndo = modifyAccumU
  , daaComputeUndo = computeUndo
  }
  where

    modifyAccumU
      :: CompositeChgAccum conf1 conf2
      -> NewestFirst [] (CompositeUndo conf1 conf2)
      -> m (Either CSMappendException (CompositeChgAccum conf1 conf2))
    modifyAccumU (CompositeChgAccum cP cS) (NewestFirst us) =
        liftA2 CompositeChgAccum <$> daaModifyAccumUndo dbaP cP usP <*> daaModifyAccumUndo dbaS cS usS
      where
        (usP, usS) = bimap NewestFirst NewestFirst $ unzip $ splitUndo <$> us
        splitUndo (CompositeUndo uP uS) = (uP, uS)

    computeUndo
      :: CompositeChgAccum conf1 conf2
      -> CompositeChgAccum conf1 conf2
      -> m (Either CSMappendException (CompositeUndo conf1 conf2))
    computeUndo (CompositeChgAccum cP cS) (CompositeChgAccum cP' cS') =
        liftA2 CompositeUndo <$> daaComputeUndo dbaP cP cP' <*> daaComputeUndo dbaS cS cS'

constructCompositeDma
    :: forall conf1 conf2 m components1 components2 .
    ( Monad m
    , components1 ~ DbComponents conf1
    , components2 ~ DbComponents conf2

    , NotIntersect components1 components2

    , HDownCastable (components1 ++ components2) components1
    , HDownCastable (components1 ++ components2) components2
    )
    => DbModifyActions conf1 m
    -> DbModifyActions conf2 m
    -> DbModifyActions (CompositeConf conf1 conf2) m
constructCompositeDma dbaP dbaS = DbModifyActions {
    dmaAccess = constructCompositeDaaU (dmaAccess dbaP) (dmaAccess dbaS)
  , dmaApply = apply
  }
  where
    apply
      :: CompositeChgAccum conf1 conf2
      -> m (DbApplyProof conf1, DbApplyProof conf2)
    apply (CompositeChgAccum cP cS) =
        liftA2 (,) (dmaApply dbaP cP) (dmaApply dbaS cS)

