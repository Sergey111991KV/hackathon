{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module AppHandle
  
where

import qualified Config as C
import Database (withDbPool)
-- import Control.Monad.IO.Class (liftIO)

import Control.Monad.Logger (NoLoggingT)
import Data.Pool (Pool)

import Database.Persist.Sql (SqlBackend)
import Control.Monad.Reader
import Ext.Logger.Config (LoggerConfig)
import qualified Ext.Logger.Colog as Log
import qualified Utils.CryptoRandomGen as CryptoRandomGen
import Control.Exception.Safe (MonadThrow)

data AppHandle = AppHandle
  { appHandleDbPool :: Pool SqlBackend,
    appHandleConfig :: C.Config,
    appHandleLogger :: LoggerConfig,
    appHandleRandomGen :: CryptoRandomGen.Ref
  }

withAppHandle :: (AppHandle -> NoLoggingT IO b) -> IO b
withAppHandle action = do
  config <- C.retrieveConfig
  loggerConfig <- C.getLoggerConfig config
  randomGen <- CryptoRandomGen.newRef
  liftIO . withDbPool config $ \pool ->
    action $ AppHandle pool config loggerConfig randomGen

type MonadHandler m = (MonadIO m, MonadReader (Log.LogAction m Log.Message) m, MonadThrow m)