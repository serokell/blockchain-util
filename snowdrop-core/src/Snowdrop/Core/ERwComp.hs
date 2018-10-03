-- | Basic types and functions for Exceptionable Read-Write Computation.

module Snowdrop.Core.ERwComp
       ( ERwComp
       , runERwComp
       , liftERoComp
       , convertERwComp
       ) where

import           Universum

import           Control.Monad.Except (MonadError)

import           Snowdrop.Core.BaseM (BaseM)
import           Snowdrop.Core.ERoComp (ChgAccum, ChgAccumM (..), ConvertEffect (..),
                                        StatePException (..), initAccumCtx)
import           Snowdrop.Util

-- | StateT over ERoComp.
-- ERoComp can be lifted to ERwComp with regarding a current state.
newtype ERwComp e eff ctx s a = ERwComp { unERwComp :: StateT s (BaseM e eff ctx) a }
    deriving (Functor, Applicative, Monad, MonadError e, MonadState s)

-- | Run a ERwComp with a passed initial state.
runERwComp
  :: forall e eff ctx s a.
    ( HasException e StatePException
    , HasGetter ctx (ChgAccumM (ChgAccum ctx))
    )
  => ERwComp e eff ctx s a
  -> s
  -> BaseM e eff ctx (a, s)
runERwComp stComp initS = do
    mChgAccum <- asks (gett @_ @(ChgAccumM (ChgAccum ctx)))
    case mChgAccum of
        CANotInitialized -> pure ()
        CAInitialized _  -> throwLocalError ChgAccumMUnexpectedlyInitialized
    runStateT (unERwComp stComp) initS

-- | Lift passed ERoComp to ERwComp.
-- Set initial state of ERoComp as ChgAccum from state from ERwComp.
liftERoComp
    :: forall e eff2 ctx s eff1 a.
    ( HasException e StatePException
    , HasLens ctx (ChgAccumM (ChgAccum ctx))
    , HasGetter s (ChgAccum ctx)
    , ConvertEffect e ctx eff1 eff2
    )
    => BaseM e eff1 ctx a
    -> ERwComp e eff2 ctx s a
liftERoComp comp =
    gets (gett @_ @(ChgAccum ctx)) >>= ERwComp . lift . flip initAccumCtx (convertEffect comp)


convertERwComp :: (BaseM e eff1 ctx (a, s) -> BaseM e eff2 ctx (a, s)) -> ERwComp e eff1 ctx s a -> ERwComp e eff2 ctx s a
convertERwComp f (ERwComp (StateT act)) = ERwComp $ StateT $ \s -> f (act s)
