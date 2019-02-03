module Snowdrop.Mempool.ERwComp.IOExecutor
       ( runERwCompIO
       ) where

import           Universum

import           Data.Default (Default)

import           Snowdrop.Core (BException, ChgAccum, Ctx, HasBException, StatePException)
import           Snowdrop.Dba.Base (DbActions (..), IOCtx (..), runERoCompIO)
import           Snowdrop.Mempool.ERwComp.Type (ERwComp, runERwComp)
import           Snowdrop.Util (ExecM)
import qualified Snowdrop.Util as Log

runERwCompIO
  :: forall s conf da daa m a.
    ( Show (BException conf)
    , Typeable (BException conf)
    , Default (ChgAccum conf)
    , HasBException conf StatePException
    , MonadIO m
    , MonadReader Log.LoggingIO m
    , DbActions da daa (ChgAccum conf) ExecM
    , Ctx conf ~ IOCtx da (ChgAccum conf)
    )
    => daa ExecM
    -> s
    -> ERwComp conf da s a
    -> m (a, s)
runERwCompIO daa initS comp = runERoCompIO daa Nothing (runERwComp comp initS)
