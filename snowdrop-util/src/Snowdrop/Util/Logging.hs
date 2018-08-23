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
    , CanLog

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
import           Loot.Log (ModifyLogName (..), MonadLogging (..), NameSelector (..), logDebug,
                           logError, logInfo, logWarning, modifyLogName)
import           Loot.Log.Internal (LogEvent)
import           Loot.Log.Rio (LoggingIO)
import qualified Loot.Log.Rio as Rio
import           Loot.Log.Warper (prepareLogWarper)
import           System.Wlog (CanLog (..), removeAllHandlers)

import qualified Data.HashMap.Strict as HM
import qualified System.Console.ANSI as Term
import qualified System.Wlog as LW

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

defaultLogCfg :: LW.LoggerConfig
defaultLogCfg = LW.productionB & LW.lcTermSeverityOut .~ Just mempty
                               & LW.lcTermSeverityErr .~ Just LW.allSeverities
                               & LW.lcTree .~ defaultTree
                               & LW.lcLogsDirectory .~ Just "logs"
  where
    defaultTree :: LW.LoggerTree
    defaultTree = mempty & LW.ltSeverity .~ Just LW.infoPlus -- TODO: make it more reasonable
      & LW.ltSubloggers .~ subloggersMap

    mkTree :: FilePath -> LW.LoggerTree
    mkTree logPath =
      mempty & LW.ltSeverity .~ Just LW.infoPlus
             & LW.ltSubloggers .~ mempty
             & LW.ltFiles .~ [LW.HandlerWrap logPath Nothing]

    subloggersMap :: LW.LoggerMap
    subloggersMap = HM.fromList
      [ (LW.LoggerName "Server", mkTree "Server.log")
      , (LW.LoggerName "Client", mkTree "Client.log")
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
    bracket
      (prepareLogWarper cfg (GivenName $ fromString ""))
      (const removeAllHandlers)
      (\(finalConfig, logging) ->
        runRIO logging $ modifyLogName (const "Executor") $ do
          logInfo $ format "Used config:\n{}" (decodeUtf8 (encode finalConfig) :: Text)
          action)
  where
    withColor :: Term.Color -> IO () -> IO ()
    withColor col act = do
      Term.setSGR [Term.SetColor Term.Foreground Term.Vivid col]
      act
      Term.setSGR [Term.Reset]
