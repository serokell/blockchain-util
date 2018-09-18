module Snowdrop.Execution.DbActions.Composite
       (
         constructCompositeDaa
       , constructCompositeDaaM
       , constructCompositeDaaU
       , CompositeChgAccum (..)
       , CompositeUndo (..)
       ) where

import           Universum

import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Reflection (Reifies, reflect)
import qualified Data.Set as S

import           Snowdrop.Core (CSMappendException, ChangeSet (..), IdSumPrefixed (..), Prefix (..),
                                filterByPrefixPred, filterSetByPrefixPred)
import           Snowdrop.Execution.DbActions.Types
import           Snowdrop.Util (NewestFirst (..), OldestFirst (..))

data CompositeUndo undoPrimary undoSecondary ps = CompositeUndo
    { cuPrimary   :: undoPrimary
    , cuSecondary :: undoSecondary
    }

data CompositeChgAccum chgAccumPrimary chgAccumSecondary ps = CompositeChgAccum
    { ccaPrimary   :: chgAccumPrimary
    , ccaSecondary :: chgAccumSecondary
    }

instance (Default chgAccumPrimary, Default chgAccumSecondary)
        => Default (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps) where
    def = CompositeChgAccum def def

constructCompositeDaa
    :: forall ps chgAccumPrimary chgAccumSecondary id value m .
    (Reifies ps (Set Prefix), Ord id, Applicative m, IdSumPrefixed id)
    => DbAccessActions chgAccumPrimary id value m
    -> DbAccessActions chgAccumSecondary id value m
    -> DbAccessActions (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps) id value m
constructCompositeDaa dbaP dbaS =
    DbAccessActions cGetter $ \(CompositeChgAccum caP caS) p ->
        bool (daaIter dbaS caS p) (daaIter dbaP caP p) $ p `S.member` prefixes
  where
    prefixes = reflect (Proxy @ps)
    cGetter (CompositeChgAccum caP caS) reqIds =
        -- We throw error as responses for sets of keys with divergent
        -- prefixes are not expected to ever overlap
        M.unionWith (error "constructCompositeDaa: responses overlap")
          <$> daaGetter dbaP caP reqIdsP
          <*> daaGetter dbaS caS reqIdsS
      where
        reqIdsP = filterSetByPrefixPred (`S.member` prefixes) reqIds
        reqIdsS = reqIds S.\\ reqIdsP

constructCompositeDaaM
    :: forall ps chgAccumPrimary chgAccumSecondary id value m .
    (Reifies ps (Set Prefix), Ord id, Monad m, IdSumPrefixed id)
    => DbAccessActionsM chgAccumPrimary id value m
    -> DbAccessActionsM chgAccumSecondary id value m
    -> DbAccessActionsM (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps) id value m
constructCompositeDaaM dbaP dbaS = DbAccessActionsM daa' modifyAccum
  where
    daa' = constructCompositeDaa (daaAccess dbaP) (daaAccess dbaS)
    prefixes = reflect (Proxy @ps)
    modifyAccum (CompositeChgAccum cP cS) (OldestFirst css) =
        liftA2 glue <$> daaModifyAccum dbaP cP cssP <*> daaModifyAccum dbaS cS cssS
      where
        (cssP, cssS) = bimap OldestFirst OldestFirst $ unzip $ splitCS prefixes <$> css
        glue (OldestFirst accsP) (OldestFirst accsS) =
            OldestFirst $ uncurry CompositeChgAccum <$> zip accsP accsS

splitCS
    :: forall id value . (Ord id, IdSumPrefixed id)
    => Set Prefix
    -> ChangeSet id value
    -> (ChangeSet id value, ChangeSet id value)
splitCS prefixes cs = (csP, csS)
  where
    csP = filterByPrefixPred @id (`S.member` prefixes) cs
    csS = ChangeSet $ changeSet cs M.\\ changeSet csP

constructCompositeDaaU
  :: forall ps chgAccumPrimary chgAccumSecondary undoPrimary undoSecondary id value m .
    (Reifies ps (Set Prefix), Ord id, Monad m, IdSumPrefixed id)
    => DbAccessActionsU chgAccumPrimary undoPrimary id value m
    -> DbAccessActionsU chgAccumSecondary undoSecondary id value m
    -> DbAccessActionsU (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps)
                        (CompositeUndo undoPrimary undoSecondary ps) id value m
constructCompositeDaaU dbaP dbaS = DbAccessActionsU daaM' modifyAccumU computeUndo
  where
    daaM' = constructCompositeDaaM (daaAccessM dbaP) (daaAccessM dbaS)

    modifyAccumU
      :: CompositeChgAccum chgAccumPrimary chgAccumSecondary ps
      -> NewestFirst [] (CompositeUndo undoPrimary undoSecondary ps)
      -> m (Either (CSMappendException id) (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps))
    modifyAccumU (CompositeChgAccum cP cS) (NewestFirst us) =
        liftA2 CompositeChgAccum <$> daaModifyAccumUndo dbaP cP usP <*> daaModifyAccumUndo dbaS cS usS
      where
        (usP, usS) = bimap NewestFirst NewestFirst $ unzip $ splitUndo <$> us

    splitUndo (CompositeUndo undoP undoS) = (undoP, undoS)

    computeUndo
      :: CompositeChgAccum chgAccumPrimary chgAccumSecondary ps
      -> CompositeChgAccum chgAccumPrimary chgAccumSecondary ps
      -> m (Either (CSMappendException id) (CompositeUndo undoPrimary undoSecondary ps))
    computeUndo (CompositeChgAccum cP cS) (CompositeChgAccum cP' cS') =
        liftA2 CompositeUndo <$> daaComputeUndo dbaP cP cP' <*> daaComputeUndo dbaS cS cS'
