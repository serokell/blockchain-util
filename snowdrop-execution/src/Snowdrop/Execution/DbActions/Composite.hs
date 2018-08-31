module Snowdrop.Execution.DbActions.Composite
       (
         constructCompositeActions
       , CompositeChgAccum (..)
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import           Data.Reflection (Reifies, reflect)
import qualified Data.Set as S

import           Snowdrop.Core (CSMappendException (..), ChangeSet (..), ChgAccumModifier (..),
                                ChgAccumOps (..), IdSumPrefixed (..), Prefix (..), Undo (..),
                                filterByPrefixPred, filterSetByPrefixPred, mappendChangeSet)

import           Snowdrop.Execution.DbActions.Types

data CompositeChgAccum chgAccumPrimary chgAccumSecondary ps = CompositeChgAccum
    { ccaPrimary   :: chgAccumPrimary
    , ccaSecondary :: chgAccumSecondary
    }

instance (Default chgAccumPrimary, Default chgAccumSecondary)
        => Default (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps) where
    def = CompositeChgAccum def def

instance ( Reifies ps (Set Prefix), IdSumPrefixed id, Ord id
         , ChgAccumOps id value chgAccumPrimary, ChgAccumOps id value chgAccumSecondary) =>
    ChgAccumOps id value (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps)
  where
    modifyAccum (CompositeChgAccum cP cS) chgAccMod = do
        (accP, undoP) <- modifyAccum cP camP
        (accS, undoS) <- modifyAccum cS camS
        (,) (CompositeChgAccum accP accS) <$> mergeUndos undoP undoS
      where
        prefixes = reflect (Proxy @ps)
        processCS f = bimap f f . splitCS
        (camP, camS) = case chgAccMod of
            CAMChange cs           -> processCS CAMChange cs
            CAMRevert (Undo cs sn) -> processCS (CAMRevert . flip Undo sn) cs
        mergeUndos (Undo csP snP) (Undo csS snS) =
            if BS.null snP || BS.null snS
            then flip Undo (snP `BS.append` snS)
                <$> (csP `mappendChangeSet` csS)
            else Left $ CSMappendException $
                     error "Dunno what to pass here. Leave it now"
        splitCS cs =
            let csP = filterByPrefixPred (`S.member` prefixes) cs in
            let csS = ChangeSet $ changeSet cs M.\\ changeSet csP in
            (csP, csS)

constructCompositeActions
    :: forall ps chgAccumPrimary chgAccumSecondary id value m .
    (Reifies ps (Set Prefix), Ord id, MonadCatch m, IdSumPrefixed id)
    => DbAccessActions chgAccumPrimary id value m
    -> DbAccessActions chgAccumSecondary id value m
    -> DbAccessActions (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps) id value m
constructCompositeActions dbaP dbaS =
    DbAccessActions cGetter $
      \(CompositeChgAccum caP caS) p ->
          bool (daaIter dbaS caS p) (daaIter dbaP caP p) $ p `S.member` prefixes
  where
    prefixes = reflect (Proxy @ps)
    cGetter (CompositeChgAccum caP caS) reqIds =
        let reqIdsP = filterSetByPrefixPred (`S.member` prefixes) reqIds in
        let reqIdsS = reqIds S.\\ reqIdsP in
        -- We throw error as responses for sets of keys with divergent
        -- prefixes are not expected to ever overlap
        M.unionWith (error "constructCompositeActions: responses overlap")
          <$> daaGetter dbaP caP reqIdsP
          <*> daaGetter dbaS caS reqIdsS
