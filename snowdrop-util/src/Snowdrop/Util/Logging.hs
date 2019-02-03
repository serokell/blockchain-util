{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Util.Logging
       (
         RIO (..)
       , ExecM
       , runRIO

       , mkLogger
       , withLogger
       , withLogSuffix
       , LoggingIO (..)
       , MonadLogging (..)

       -- * Loggers
       , logDebug
       , logError
       , logWarning
       , logInfo

       , defLog
       ) where

import           Universum hiding (log)

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Trans.Control (MonadBaseControl (..))

import           System.FilePath ((</>), (<.>))
import           Formatting ((%), format, shown, string)
import           Formatting.Time (datetime)
import           Data.Time.Clock (getCurrentTime)

-- | Conventional RIO monad, being used as a base monad for logging in Snowdrop
newtype RIO ctx a = RIO (ReaderT ctx IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader ctx,
              MonadThrow, MonadCatch, MonadMask, MonadBase IO)

deriving instance MonadBase IO (RIO ctx) => MonadBaseControl IO (RIO ctx)
deriving instance Monoid a => Monoid (ReaderT ctx IO a)
deriving instance Monoid a => Monoid (RIO ctx a)
deriving instance Semigroup a => Semigroup (ReaderT ctx IO a)
deriving instance Semigroup a => Semigroup (RIO ctx a)

-- | Executor for 'RIO' monad
runRIO :: MonadIO m => ctx -> RIO ctx a -> m a
runRIO ctx (RIO act) = liftIO $ runReaderT act ctx

-- Copypaste (edited) from Loot
-- | Logging level.
data Level
    = Debug     -- ^ Things nobody should see unless it's explicitly stated.
    | Info      -- ^ Regular information for user.
    | Warning   -- ^ Suspicious warning conditions.
    | Error     -- ^ Errors.
    deriving (Eq, Generic, Show)

class MonadLogging m where
    log :: Level -> String -> m ()

data LoggingIO = LoggingIO {
    lioLog :: Maybe String -> Level -> String -> IO ()
  , lioSuffix :: Maybe String
  }

logDebug, logInfo, logWarning, logError :: MonadLogging m => String -> m ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning
logError   = log Error

-- Don't bother with typeclasses in these
defLog :: (MonadReader ctx m, MonadIO m) => (ctx -> LoggingIO) -> Level -> String -> m ()
defLog f l s = do
  lg <- f <$> ask
  liftIO $ lioLog lg (lioSuffix lg) l s

----------------------------------------

-- | Default execution monad for Snowdrop's execution code.
-- Provides this simplest logging.
type ExecM = RIO LoggingIO

instance MonadLogging ExecM where
  log l s = defLog id l s

mkLogger :: String -> LoggingIO
mkLogger fp = LoggingIO doLog Nothing
  where
    doLog suff l s = do
      ct <- getCurrentTime
      let msg = toStrict $
                  format ("[" % string % string % ":" % shown % "] [" % datetime % "] " % string % "\n")
                           fp (maybe "" ('.':) suff) l ct s
      appendFile ("log" </> fp <.> "log") msg >> putStr msg

withLogger :: String -> ExecM () -> ExecM ()
withLogger fp action = local (const $ mkLogger fp) action

withLogSuffix :: String -> ExecM () -> ExecM ()
withLogSuffix s action = local (\lio -> lio {lioSuffix = Just s}) action
