{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic types and functions for Exceptionable Read-Write Computation.

module Snowdrop.Core.ERwComp
       ( ERwComp
       , runERwComp
       , liftERoComp
       , convertERwComp
       , upcastEffERwCompM
       ) where

import           Universum

import           Control.Monad.Except (MonadError)

import           Snowdrop.Core.BaseM (BaseM)
import           Snowdrop.Core.ERoComp (BException, ChgAccum, ChgAccumCtx (..), ConvertEffect (..),
                                        Ctx, DbAccessM, HasBException, StatePException (..),
                                        UpCastableERoM, initAccumCtx, upcastEffERoCompM)
import           Snowdrop.Util

-- | StateT over ERoComp.
-- ERoComp can be lifted to ERwComp with regarding a current state.
newtype ERwComp conf eff s a = ERwComp { unERwComp :: StateT s (BaseM (BException conf) eff (Ctx conf)) a }
    deriving (Functor, Applicative, Monad, MonadState s)

deriving instance e ~ BException conf => MonadError e (ERwComp conf eff s)

-- | Run a ERwComp with a passed initial state.
runERwComp
  :: forall conf eff s a.
    ( HasBException conf StatePException
    , HasGetter (Ctx conf) (ChgAccumCtx conf)
    )
  => ERwComp conf eff s a
  -> s
  -> BaseM (BException conf) eff (Ctx conf) (a, s)
runERwComp stComp initS = do
    mChgAccum <- asks (gett @_ @(ChgAccumCtx conf))
    case mChgAccum of
        CANotInitialized -> pure ()
        CAInitialized _  -> throwLocalError ChgAccumCtxUnexpectedlyInitialized
    runStateT (unERwComp stComp) initS

-- | Lift passed ERoComp to ERwComp.
-- Set initial state of ERoComp as ChgAccum from state from ERwComp.
liftERoComp
    :: forall conf eff2 s eff1 a.
    ( HasBException conf StatePException
    , HasLens (Ctx conf) (ChgAccumCtx conf)
    , HasGetter s (ChgAccum conf)
    , ConvertEffect conf eff1 eff2
    )
    => BaseM (BException conf) eff1 (Ctx conf) a
    -> ERwComp conf eff2 s a
liftERoComp comp =
    gets (gett @_ @(ChgAccum conf)) >>=
        ERwComp . lift . flip (initAccumCtx @_ @conf) (convertEffect @conf comp)

convertERwComp
  :: forall conf eff1 eff2 s a .
     ( BaseM (BException conf) eff1 (Ctx conf) (a, s)
        -> BaseM (BException conf) eff2 (Ctx conf) (a, s) )
  -> ERwComp conf eff1 s a -> ERwComp conf eff2 s a
convertERwComp f (ERwComp (StateT act)) = ERwComp $ StateT $ \s -> f (act s)

upcastEffERwCompM
    :: forall xs supxs conf s a . UpCastableERoM xs supxs
    => ERwComp conf (DbAccessM conf xs) s a
    -> ERwComp conf (DbAccessM conf supxs) s a
upcastEffERwCompM (ERwComp comp) = ERwComp $ StateT $ upcastEffERoCompM . runStateT comp

