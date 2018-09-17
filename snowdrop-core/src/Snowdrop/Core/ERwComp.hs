module Snowdrop.Core.ERwComp
       ( ERwComp
       , runERwComp
       , modifyRwCompChgAccum
       , liftERoComp
       , upcastEffERwComp
       ) where

import           Universum

import           Control.Lens ((.=))
import           Control.Monad.Except (MonadError)

import           Snowdrop.Core.ChangeSet (CSMappendException (..), Undo)
import           Snowdrop.Core.ERoComp (ChgAccum, ChgAccumCtx (..), ChgAccumModifier, ERoComp,
                                        StatePException (..), UpCastableERo, initAccumCtx,
                                        modifyAccum, upcastEffERoComp)
import           Snowdrop.Util

newtype ERwComp e ctx s xs a = ERwComp { unERwComp :: StateT s (ERoComp e ctx xs) a }
    deriving (Functor, Applicative, Monad, MonadError e, MonadState s)

runERwComp
  :: forall e ctx s xs a.
  ( HasException e StatePException
  , HasGetter ctx (ChgAccumCtx ctx)
  )
  => ERwComp e ctx s xs a
  -> s
  -> ERoComp e ctx xs (a, s)
runERwComp stComp initS = do
    mChgAccum <- asks (gett @_ @(ChgAccumCtx ctx))
    case mChgAccum of
        CANotInitialized -> pure ()
        CAInitialized _  -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
    runStateT (unERwComp stComp) initS

liftERoComp
    :: forall e ctx xs s a .
    ( HasException e StatePException
    , HasLens ctx (ChgAccumCtx ctx)
    , HasGetter s (ChgAccum ctx)
    )
    => ERoComp e ctx xs a
    -> ERwComp e ctx s xs a
liftERoComp comp = gets (gett @_ @(ChgAccum ctx)) >>= ERwComp . lift . flip initAccumCtx comp

modifyRwCompChgAccum
    :: forall e xs s ctx .
    ( HasException e CSMappendException
    , HasLens s (ChgAccum ctx)
    )
    => ChgAccumModifier xs
    -> ERwComp e ctx s xs (Undo xs)
modifyRwCompChgAccum chgSet = do
    chgAcc <- gets (gett @_ @(ChgAccum ctx))
    newChgAccOrE <- ERwComp $ lift $ modifyAccum chgAcc chgSet
    flip (either $ ERwComp . throwLocalError) newChgAccOrE $
        \(chgAcc', undo) -> (lensFor @s @(ChgAccum ctx) .= chgAcc') $> undo
------------------------
-- Cast and hoist
------------------------

upcastEffERwComp
    :: forall xs supxs e ctx s a . UpCastableERo xs supxs
    => ERwComp e ctx s xs a
    -> ERwComp e ctx s supxs a
upcastEffERwComp (ERwComp comp) = ERwComp $ StateT $ upcastEffERoComp . runStateT comp
