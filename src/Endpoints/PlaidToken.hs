{-# LANGUAGE RecordWildCards #-}

module Endpoints.PlaidToken where

import AppHandle
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (liftIO)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Persist.Postgresql
import qualified Database.Persist.Postgresql as P
import Database.Tables.PaidToken
import Database.Tables.User

import qualified Ext.HTTP.Error as Web
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest
import Model.TypePaidAction
import Utils.CryptoRandomGen (getRandomByteString)

getPaidTokenEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> PlaidTokenGet -> m (Web.WebApiHttpResponse T.Text)
getPaidTokenEndpoint AppHandle {..} PlaidTokenGet {..} = do
  newToken <- liftIO $ getRandomByteString appHandleRandomGen 30
  _ <-
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      createPaidTokenEntity typePay idPayAction (toHexText newToken) amount
  pure $ Web.result (toHexText newToken)
  where
    toHexText = T.decodeLatin1 . LB.toStrict . B.toLazyByteString . B.byteStringHex
