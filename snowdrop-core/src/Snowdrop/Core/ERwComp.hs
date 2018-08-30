module Snowdrop.Core.ERwComp
       ( ERwComp
       , runERwComp
       , modifyRwCompChgAccum
       , liftERoComp
       ) where

import           Universum

import           Control.Lens ((.=))
import           Control.Monad.Except (MonadError)

import           Snowdrop.Core.ChangeSet (CSMappendException (..), Undo)
import           Snowdrop.Core.ERoComp (ChgAccum, ChgAccumCtx (..), ChgAccumModifier, ERoComp,
                                        StatePException (..), initAccumCtx, modifyAccum)
import           Snowdrop.Core.ERoComp.Helpers (ChgAccumOps (..))
import           Snowdrop.Util

newtype ERwComp e id value ctx s a = ERwComp { unERwComp :: StateT s (ERoComp e id value ctx) a }
    deriving (Functor, Applicative, Monad, MonadError e, MonadState s)

runERwComp
  :: forall e id value ctx s a.
  ( HasException e StatePException
  , HasGetter ctx (ChgAccumCtx ctx)
  )
  => ERwComp e id value ctx s a
  -> s
  -> ERoComp e id value ctx (a, s)
runERwComp stComp initS = do
    mChgAccum <- asks (gett @_ @(ChgAccumCtx ctx))
    case mChgAccum of
        CANotInitialized -> pure ()
        CAInitialized _  -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
    runStateT (unERwComp stComp) initS

liftERoComp
    :: forall e id value ctx s a.
    ( HasException e StatePException
    , HasLens ctx (ChgAccumCtx ctx)
    , HasGetter s (ChgAccum ctx)
    )
    => ERoComp e id value ctx a
    -> ERwComp e id value ctx s a
liftERoComp comp = gets (gett @_ @(ChgAccum ctx)) >>= ERwComp . lift . flip initAccumCtx comp

modifyRwCompChgAccum
    :: forall e id value ctx s .
    ( HasException e (CSMappendException id)
    , HasLens s (ChgAccum ctx)
    , ChgAccumOps id value (ChgAccum ctx)
    )
    => ChgAccumModifier id value
    -> ERwComp e id value ctx s (Undo id value)
modifyRwCompChgAccum chgSet = do
    chgAcc <- gets (gett @_ @(ChgAccum ctx))
    let newChgAccOrE = modifyAccum chgAcc chgSet
    flip (either $ ERwComp . throwLocalError) newChgAccOrE $
      \(chgAcc', undo) -> (lensFor @s @(ChgAccum ctx) .= chgAcc') $> undo
