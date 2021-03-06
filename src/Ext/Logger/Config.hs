module Ext.Logger.Config
  ( LoggerConfig (..),
  )
where

import qualified Colog as Log
import qualified Data.Text as T

data LoggerConfig = LoggerConfig
  { appInstanceName :: T.Text,
    logToStdout :: Bool,
    logLevel :: Log.Severity
  }
  deriving (Show, Eq)