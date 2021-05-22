{-# LANGUAGE RecordWildCards #-}

module Endpoints.PlaidToken where
import AppHandle 
-- import Database


-- import Control.Exception.Safe (MonadThrow)

import Database.Persist.Postgresql
-- import qualified Database.Persist.Postgresql as P
-- import Servant (err404)
import qualified Data.Text as T
-- import Database.Tables.Token 
import Database.Tables.PaidToken
import qualified Ext.HTTP.Error as Web
import qualified Ext.HTTP.Response as Web
import Database.Tables.User
import qualified Database.Persist.Postgresql as P
import Model.TypePaidAction
import Database.Tables.UserEvents
import Database.Tables.UserSubsriptions
import Utils.CryptoRandomGen ( getRandomByteString )
import Control.Monad.IO.Unlift (liftIO)
import qualified Data.Text.Encoding as T
import Model.PlaidTokenRequest
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
-- import qualified Database.Tables.Events as DB
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB





            
getPaidTokenEndpoint :: 
    (MonadIO m, MonadThrow m) => AppHandle -> PlaidTokenGet -> m (Web.WebApiHttpResponse T.Text)
getPaidTokenEndpoint AppHandle {..} PlaidTokenGet {..} = do
    newToken <- liftIO $ getRandomByteString appHandleRandomGen  30
    _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                    createPaidTokenEntity typePay idPayAction (toHexText newToken) amount
    pure $ Web.result (toHexText newToken)
    where
        toHexText = T.decodeLatin1 . LB.toStrict . B.toLazyByteString . B.byteStringHex
    
