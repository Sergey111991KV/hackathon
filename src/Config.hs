{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Config
  
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (fromMaybe)

import Ext.Logger.Colog (Severity (Debug))
import Ext.Logger.Config (LoggerConfig (..))
import Text.Read (readMaybe)
import qualified Servant.Auth.Server as SAS

type GetConfig cfg = forall m. MonadIO m => C.Config -> m cfg
type Config = C.Config

getJWTSettings :: GetConfig SAS.JWTSettings
getJWTSettings config = do
  filePath <- getKeysFilePath config
  authKey <- liftIO $ SAS.readKey filePath
  pure $ SAS.defaultJWTSettings authKey


retrieveConfig :: MonadIO m => m C.Config
retrieveConfig = do
  let configPath = "./config/local.conf"
  let templateConfigPath = "./config/template.conf"
  liftIO $ C.load [C.Required templateConfigPath, C.Optional configPath]

getKeysFilePath :: MonadIO m => C.Config -> m FilePath
getKeysFilePath config = liftIO $ C.require config "auth.key_path"

getPort :: MonadIO m => C.Config -> m Int
getPort config = liftIO $ C.require config "web_server.port"

getPoolLimit :: MonadIO m => C.Config -> m Int
getPoolLimit config = liftIO $ C.require config "database.pool_limit"

getLoggerConfig :: MonadIO m => C.Config -> m LoggerConfig
getLoggerConfig config = liftIO $ do
   appInstanceName <- C.require config "log.app_instance_name"
   logToStdout <- C.require config "log.log_to_stdout"
   logLevelRaw <- C.require config "log.log_level"
   pure $
     LoggerConfig
       { appInstanceName = appInstanceName,
         logToStdout = logToStdout,
         logLevel = fromMaybe Debug (readMaybe logLevelRaw)
       }