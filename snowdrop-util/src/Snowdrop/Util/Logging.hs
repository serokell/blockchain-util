{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}

module Snowdrop.Util.Logging
       ( Name
       , MonadLogging
       , logDebug
       , logError
       , logWarning
       , logInfo
       , modifyLogName
       , LogAction (..)
       , HasLog (..)
       ) where

import           Universum hiding (log)

import           Colog.Core (LogAction (..), Severity (..))
import qualified Data.DList as DL
import qualified Data.Text as T
import           Data.Text.Buildable (build)
import           Fmt ((+|), (|+))
import qualified System.Console.ANSI as Term

import           Snowdrop.Util.Lens (HasGetter (..), HasLens (..))

newtype Name = Name { _unName :: DL.DList Text }
  deriving (Semigroup, Monoid, Show)

instance IsString Name where
    fromString s  =
      Name $ DL.fromList $ case s of
        "" -> []
        s' -> T.splitOn "." (fromString s')

instance Buildable Name where
    build (Name parts) = ""+| T.intercalate "." (DL.toList parts) |+""

data Message = Message
    { lmSeverity :: Severity
    , lmName     :: Name
    , lmText     :: Text
    }
  deriving Show

instance Buildable Message where
    build Message {..} = "["+|lmName|+":"+|lmSeverity|+"] "+|lmText|+""

setColor :: Severity -> Text
setColor = toText . Term.setSGRCode . pure . Term.SetColor Term.Foreground Term.Vivid . toColor

toColor :: Severity -> Term.Color
toColor Debug   = Term.Green
toColor Info    = Term.White
toColor Warning = Term.Yellow
toColor Error   = Term.Red

resetColor :: Text
resetColor = toText (Term.setSGRCode [Term.Reset])

instance Buildable Severity where
  build sev = ""+|setColor sev|+ show sev +|resetColor|+""

class HasLog env msg m where
    getLogAction :: env -> LogAction m msg

type MonadLogging env m =
    ( HasLog env Message m
    , HasGetter env Name
    , MonadReader env m
    )

log
  :: MonadLogging env m
  => Severity -> Text -> m ()
log sev text = do
  name <- asks gett
  action <- asks getLogAction
  unLogAction action $ Message sev name text

logDebug
  :: MonadLogging env m
  => Text -> m ()
logDebug = log Debug

logInfo
  :: MonadLogging env m
  => Text -> m ()
logInfo = log Info

logWarning
  :: MonadLogging env m
  => Text -> m ()
logWarning = log Warning

logError
  :: MonadLogging env m
  => Text -> m ()
logError = log Error

modifyLogName
  :: (HasLens env Name, MonadReader env m)
  => Name -> m a -> m a
modifyLogName subname = local (over lensFor (<> subname))
