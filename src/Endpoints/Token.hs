{-# LANGUAGE RecordWildCards #-}

module Endpoints.Token where
import AppHandle (AppHandle (..))
-- import Database


import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Postgresql
-- import qualified Database.Persist.Postgresql as P
-- import Servant (err404)
import qualified Data.Text as T
-- import Database.Tables.Token 
import Database.Tables.PaidToken
-- import qualified Ext.HTTP.Error as Web
import qualified Ext.HTTP.Response as Web


getPaidTokenEndpoint ::
   (MonadIO m, MonadThrow m) => AppHandle -> T.Text -> m (Web.WebApiHttpResponse Bool)
getPaidTokenEndpoint AppHandle {..} token = do
    _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
       loadPaidTokenEntity token
    -- case tokenEnt of

    pure $ Web.result True
    -- where
        