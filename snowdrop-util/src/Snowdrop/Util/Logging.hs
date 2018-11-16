{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Util.Logging
    (
      RIO (..)
    , ExecM
    , runRIO

    , LogEvent
    , withLogger
    , LoggingIO
    , lensOf
    , MonadLogging (..)
    , ModifyLogName (..)
    , NameSelector (..)

    -- * Loggers
    , logDebug
    , logError
    , logWarning
    , logInfo
    , modifyLogName
    ) where

import           Universum

import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Data.Yaml (decodeFileEither, encode)
import           Fmt (format)
import           Loot.Base.HasLens (HasLens, lensOf)
import           Loot.Log (ModifyLogName (..), MonadLogging (..), NameSelector (..),
                           logDebug, logError, logInfo, logWarning, modifyLogName,
                           LogEvent, allocateLogging, Severity (Debug), LoggingIO,
                           LogConfig (..), BackendConfig (..))
import qualified Loot.Log.Rio as Rio
import qualified System.Console.ANSI as Term
import Control.Monad.Component (runComponentM)
-- Copied from Disciplina:

{- | Conventional "ReaderT over IO" monad stack.

Lootbox bases on 'caps' library which allows the only 'ReaderT' instance for
used typeclasses, e.g.
@instance (r ~ Capabilities caps) => MonadLogging (ReaderT r IO)@.
To avoid instances overlapping, we use this wrapper.

This also allows us to remorselessly define one global
@instance HasLens Smth ctx Smth => MonadSmth (RIO ctx)@ per every @Smth@.
-}
newtype RIO ctx a = RIO (ReaderT ctx IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader ctx,
              MonadThrow, MonadCatch, MonadMask, MonadBase IO)

deriving instance MonadBase IO (RIO ctx) => MonadBaseControl IO (RIO ctx)

runRIO :: MonadIO m => ctx -> RIO ctx a -> m a
runRIO ctx (RIO act) = liftIO $ runReaderT act ctx

instance (HasLens LoggingIO ctx LoggingIO) =>
         MonadLogging (RIO ctx) where
    log = Rio.defaultLog
    logName = Rio.defaultLogName

instance (HasLens LoggingIO ctx LoggingIO) =>
         ModifyLogName (RIO ctx) where
    modifyLogNameSel = Rio.defaultModifyLogNameSel

type ExecM = RIO LoggingIO

----------------------------------------------------------------------------
-- Configuration and initiation
----------------------------------------------------------------------------

defaultLogCfg :: LogConfig
defaultLogCfg = LogConfig {..}
  where
    minSeverity = Debug
    backends =
      [ StdErr
      , File "logs/out.log"
      ]


-- TODO: finalise config from `ConfigPart`s
withLogger :: Maybe FilePath -> ExecM () -> IO ()
withLogger mConfigPath action = do
    cfg <- case mConfigPath of
        Nothing -> withColor Term.Yellow (putTextLn "Using the default logger configuration") >> return defaultLogCfg
        Just path -> decodeFileEither path >>= \case
          Right cfgFromYmal -> return cfgFromYmal
          Left err -> do
            withColor Term.Red (putTextLn "Error: ") >> print err
            withColor Term.Red (putTextLn "Default log config will be used.")
            return defaultLogCfg
    runComponentM "blockchain-util" (allocateLogging cfg (GivenName $ fromString "")) $ \logging ->
        runRIO logging $ modifyLogName (const "Executor") $ do
          logInfo $ format "Used config:\n{}" (decodeUtf8 (encode cfg) :: Text)
          action
  where
    withColor :: Term.Color -> IO () -> IO ()
    withColor col act = do
      Term.setSGR [Term.SetColor Term.Foreground Term.Vivid col]
      act
      Term.setSGR [Term.Reset]
