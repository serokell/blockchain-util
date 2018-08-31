module Snowdrop.Execution.Restrict
       (
         RestrictionInOutException
       , restrictDbAccess
       , restrictCS
       , RestrictCtx (..)

       ) where

import           Universum

import           Control.Monad.Except (MonadError (..))
import           Data.Default (Default (def))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))

import           Snowdrop.Core (ChangeSet (..), ERoComp, IdSumPrefixed (..), Prefix (..))
import           Snowdrop.Util (HasException, HasLens (lensFor), listF, throwLocalError)

data RestrictionInOutException
    = InputRestrictionException (Set Prefix)
    | OutRestrictionException (Set Prefix)
    deriving (Show)

instance Buildable RestrictionInOutException where
    build = \case
        InputRestrictionException vio ->
            bprint ("Access to prefixes beyond restricted ones \
                    \(violating prefixes: "%listF ", " build%")") vio
        OutRestrictionException vio ->
            bprint ("Produced ChangeSet affects prefixes beyond restricted ones \
                    \(violating prefixes: "%listF ", " build%")") vio

newtype RestrictCtx = RestrictCtx { unRestrictCtx :: Maybe (Set Prefix) }

instance Default RestrictCtx where
    def = RestrictCtx Nothing

restrictDbAccess
    :: forall e id value ctx a .
    ( HasLens ctx RestrictCtx
    )
    => Set Prefix
    -> ERoComp e id value ctx a
    -> ERoComp e id value ctx a
restrictDbAccess ps = local (lensFor @ctx @RestrictCtx %~ modCtx)
  where
    modCtx (RestrictCtx Nothing)    = RestrictCtx $ Just ps
    modCtx (RestrictCtx (Just ps')) = RestrictCtx $ Just $ ps `S.intersection` ps'

restrictCS
    :: forall e id value m .
    ( MonadError e m
    , HasException e RestrictionInOutException
    , IdSumPrefixed id
    )
    => Set Prefix
    -> ChangeSet id value
    -> m ()
restrictCS out cs = throwResInpOutEx OutRestrictionException out (M.keysSet $ changeSet cs)

throwResInpOutEx
    :: forall e id m .
    ( MonadError e m
    , HasException e RestrictionInOutException
    , IdSumPrefixed id
    )
    => (Set Prefix -> RestrictionInOutException)
    -> Set Prefix
    -> Set id
    -> m ()
throwResInpOutEx ex restSet sids = do
    let prefixes = S.fromList $ map idSumPrefix $ S.toList sids
    let diff = prefixes `S.difference` restSet
    if S.null diff
    then pure ()
    else throwLocalError $ ex diff
