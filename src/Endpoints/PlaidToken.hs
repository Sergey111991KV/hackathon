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
import Utils.CryptoRandomGen
import Control.Monad.IO.Unlift (liftIO)
import qualified Data.Text.Encoding as T
import Model.PlaidTokenRequest
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
-- import qualified Database.Tables.Events as DB



sendPaidTokenEndpoint ::
    (MonadIO m, MonadThrow m) => AppHandle -> PlaidTokenSend -> m (Web.WebApiHttpResponse ())
sendPaidTokenEndpoint AppHandle {..} PlaidTokenSend {..} = do
    tokenEnt <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
       loadPaidTokenEntity token
    case tokenEnt of
        Nothing -> pure $ Web.failWith (Web.mkWebApiHttpError "Cann't find token " "UndefinedToken")
        Just (Entity _ PaidToken {..}) -> do
            maybeUser <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                loadUserById  userKey
            case maybeUser of
                Nothing -> pure $ Web.failWith (Web.mkWebApiHttpError "Not found User " "UndefinedUserId")
                Just (Entity _ User {..}) -> do
                    if userBill < paidTokenAmount 
                        then pure $ Web.failWith (Web.mkWebApiHttpError "Not enough money " "NotEnoughMoney ")
                        else do
                            _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                                updateUserBill userKey (userBill - paidTokenAmount )
                            case paidTokenTypeAction of
                                Events  -> do
                                    _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                                        creatUserEvents userId paidTokenIdAction
                                    pure $ Web.result ()
                                Subscriptions -> do
                                    _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                                        creatUserSubsriptions userId paidTokenIdAction
                                    pure $ Web.result ()                         
    where
        userKey = P.toSqlKey $ fromIntegral userId

            
getPaidTokenEndpoint :: 
    (MonadIO m, MonadThrow m) => AppHandle -> PlaidTokenGet -> m (Web.WebApiHttpResponse T.Text)
getPaidTokenEndpoint AppHandle {..} PlaidTokenGet {..} = do
    newToken <- liftIO $ getRandomByteString appHandleRandomGen  15
    _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                    createPaidTokenEntity typePay idPayAction (T.decodeUtf8 newToken) amount
    pure $ Web.result (T.decodeUtf8 newToken)
    
