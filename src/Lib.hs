module Lib
 where


import qualified Colog as Log

import qualified Ext.Logger.Colog as Log
import qualified Ext.Logger.Config as Log



runDefaultExample :: IO ()
runDefaultExample =
   Log.usingLoggerT (Log.mkLogActionIO logConf) $ do
    --  config <- liftIO C.retrieveConfig
     runLogExample
     Log.logInfo "Finishing application..."

logConf :: Log.LoggerConfig
logConf =
   Log.LoggerConfig
     { appInstanceName = "AppName"
     , logToStdout = True
     , logLevel = Log.Debug
     }

runLogExample :: Log.WithLog env Log.Message m => m ()
runLogExample = do
   Log.logInfo "Starting application..."
   Log.logDebug "Here is how we work!"

