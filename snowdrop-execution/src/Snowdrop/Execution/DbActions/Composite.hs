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

import           Snowdrop.Core (ChangeSet (..), ChgAccumModifier (..), IdSumPrefixed (..),
                                Prefix (..), Undo (..), filterByPrefixPred, filterSetByPrefixPred,
                                mappendChangeSet)

import           Snowdrop.Execution.DbActions.Types

data CompositeChgAccum chgAccumPrimary chgAccumSecondary ps = CompositeChgAccum
    { ccaPrimary   :: chgAccumPrimary
    , ccaSecondary :: chgAccumSecondary
    }

instance (Default chgAccumPrimary, Default chgAccumSecondary)
        => Default (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps) where
    def = CompositeChgAccum def def

constructCompositeActions
    :: forall ps chgAccumPrimary chgAccumSecondary id value m .
    (Reifies ps (Set Prefix), Ord id, MonadCatch m, IdSumPrefixed id)
    => DbAccessActions chgAccumPrimary id value m
    -> DbAccessActions chgAccumSecondary id value m
    -> DbAccessActions (CompositeChgAccum chgAccumPrimary chgAccumSecondary ps) id value m
constructCompositeActions dbaP dbaS =
    DbAccessActions cGetter cModifyAccum $ \(CompositeChgAccum caP caS) p ->
        bool (daaIter dbaS caS p) (daaIter dbaP caP p) $ p `S.member` prefixes
  where
    cModifyAccum (CompositeChgAccum cP cS) chgAccMod = runExceptT $ do
        (accP, undoP) <- ExceptT $ daaModifyAccum dbaP cP camP
        (accS, undoS) <- ExceptT $ daaModifyAccum dbaS cS camS
        (,) (CompositeChgAccum accP accS) <$> mergeUndos undoP undoS
      where
        processCS f = bimap f f . splitCS
        (camP, camS) =
          case chgAccMod of
            CAMChange cs         -> processCS CAMChange cs
            CAMRevert (Undo cs sn) -> processCS (CAMRevert . flip Undo sn) cs
    mergeUndos (Undo csP snP) (Undo csS snS) =
      if BS.null snP || BS.null snS
      then flip Undo (snP `BS.append` snS)
           <$> (ExceptT $ pure $ csP `mappendChangeSet` csS)
      else throwM $ DbApplyException "Both undo contain non-empty snapshot in composite actions"
    splitCS cs = (csP, csS)
      where
        csP = filterByPrefixPred (`S.member` prefixes) cs
        csS = ChangeSet $ changeSet cs M.\\ changeSet csP
    prefixes = reflect (Proxy @ps)
    cGetter (CompositeChgAccum caP caS) reqIds =
        -- We throw error as responses for sets of keys with divergent
        -- prefixes are not expected to ever overlap
        M.unionWith (error "constructCompositeActions: responses overlap")
          <$> daaGetter dbaP caP reqIdsP
          <*> daaGetter dbaS caS reqIdsS
      where
        reqIdsP = filterSetByPrefixPred (`S.member` prefixes) reqIds
        reqIdsS = reqIds S.\\ reqIdsP
