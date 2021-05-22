{-# LANGUAGE RecordWildCards #-}

module Endpoints.Token where

import AppHandle
-- import Database

-- import Control.Exception.Safe (MonadThrow)

-- import qualified Database.Persist.Postgresql as P
-- import Servant (err404)

-- import Database.Tables.Token

import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (liftIO)
-- import qualified Database.Tables.Events as DB
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database
import Database.Persist.Postgresql
import qualified Database.Persist.Postgresql as P
import Database.Tables.PaidToken
import Database.Tables.Token
import Database.Tables.User
import Database.Tables.UserEvents
import Database.Tables.UserSubsriptions
import qualified Ext.HTTP.Error as Web
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest
import Model.TokenRequest
import Model.TypePaidAction
import Utils.CryptoRandomGen
import Utils.CryptoRandomGen (getRandomByteString)

exchangeTokenEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> ChangePlaidToken -> m (Web.WebApiHttpResponse UserToken)
exchangeTokenEndpoint AppHandle {..} ChangePlaidToken {..} = do
  dataLoad <- liftIO . flip runSqlPersistMPool appHandleDbPool $ do
    user <- loadUserById userKey
    plaidToken <- loadPaidTokenEntity payToken
    pure (user, plaidToken)
  case dataLoad of
    (Just (Entity _ User {..}), Just (Entity _ PaidToken {..})) -> do
      maybeUser <-
        liftIO . flip runSqlPersistMPool appHandleDbPool $
          loadUserById userKey
      case maybeUser of
        Nothing -> pure $ Web.failWith (Web.mkWebApiHttpError "Not found User " "UndefinedUserId")
        Just (Entity _ User {..}) -> do
          if userBillT < paidTokenAmount
            then pure $ Web.failWith (Web.mkWebApiHttpError "Not enough money " "NotEnoughMoney ")
            else do
              newToken <- liftIO $ getRandomByteString appHandleRandomGen 30
              _ <-
                liftIO . flip runSqlPersistMPool appHandleDbPool $
                  updateUserBill userKey (userBillT - paidTokenAmount)
              case paidTokenTypeAction of
                Events -> do
                  _ <- liftIO . flip runSqlPersistMPool appHandleDbPool $ do
                    updateUserBonusBill userKey (userBillT - paidTokenAmount)
                    _ <- creatUserEvents userIdChange paidTokenIdAction
                    createUserTokenEntity
                      CreateToken
                        { userId = userIdChange,
                          typeEvents = Events,
                          textToken = toHexText newToken,
                          isActive = True
                        }
                  pure $ Web.result $ UserToken (toHexText newToken) userIdChange
                Subscriptions -> do
                  _ <- liftIO . flip runSqlPersistMPool appHandleDbPool $ do
                    updateUserBonusBill userKey (userBillT - paidTokenAmount)
                    _ <- creatUserSubsriptions userIdChange paidTokenIdAction
                    createUserTokenEntity
                      CreateToken
                        { userId = userIdChange,
                          typeEvents = Subscriptions,
                          textToken = toHexText newToken,
                          isActive = True
                        }
                  pure $ Web.result $ UserToken (toHexText newToken) userIdChange
    _ -> pure $ Web.failWith (Web.mkWebApiHttpError "Not found Entity " "NotFoundEntity ")
  where
    userKey = P.toSqlKey $ fromIntegral userIdChange
    toHexText = T.decodeLatin1 . LB.toStrict . B.toLazyByteString . B.byteStringHex

deactivateTokenEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> UserToken -> m (Web.WebApiHttpResponse ())
deactivateTokenEndpoint AppHandle {..} UserToken {..} = do
  _ <-
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      activateUserToken userToken
  pure $ Web.result ()
