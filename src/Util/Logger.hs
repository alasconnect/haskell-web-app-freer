module Util.Logger where

-------------------------------------------------------------------------------
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Log
import Data.Text (Text)
-------------------------------------------------------------------------------

data Logger r where
  LoggerDebug :: Text -> Logger ()
  LoggerInfo :: Text -> Logger ()
  LoggerNotice :: Text -> Logger ()
  LoggerWarning :: Text -> Logger ()
  LoggerError :: Text -> Logger ()
  LoggerCritical :: Text -> Logger ()
  LoggerAlert :: Text -> Logger ()
  LoggerEmergency :: Text -> Logger ()

makeEffect ''Logger

runLogger ::
  ( LastMember IO r
  , Member IO r
  )
  => Eff (Logger ': r) ~> Eff r
runLogger =
  interpret $ \case
    LoggerDebug msg -> sendM $ runLoggingT (logDebug msg) print
    LoggerInfo msg -> sendM $ runLoggingT (logInfo msg) print
    LoggerNotice msg -> sendM $ runLoggingT (logNotice msg) print
    LoggerWarning msg -> sendM $ runLoggingT (logWarning msg) print
    LoggerError msg -> sendM $ runLoggingT (logError msg) print
    LoggerCritical msg -> sendM $ runLoggingT (logCritical msg) print
    LoggerAlert msg -> sendM $ runLoggingT (logAlert msg) print
    LoggerEmergency msg -> sendM $ runLoggingT (logEmergency msg) print
