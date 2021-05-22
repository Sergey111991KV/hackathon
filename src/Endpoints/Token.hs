{-# LANGUAGE RecordWildCards #-}

module Endpoints.Token where
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
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import Model.TokenRequest
import Database.Tables.Token 
import Database
import qualified Ext.HTTP.Error as Web
import qualified Ext.HTTP.Response as Web
import Database.Persist.Postgresql
import Utils.CryptoRandomGen ( getRandomByteString )




exchangeTokenEndpoint ::
    (MonadIO m, MonadThrow m) => AppHandle -> ChangePlaidToken -> m (Web.WebApiHttpResponse UserToken)
exchangeTokenEndpoint AppHandle {..} ChangePlaidToken {..} = do
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
                    if userBillT < paidTokenAmount 
                        then pure $ Web.failWith (Web.mkWebApiHttpError "Not enough money " "NotEnoughMoney ")
                        else do
                            _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                                updateUserBill userKey (userBillT - paidTokenAmount )
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


-- exchangeTokenEndpoint ::
--     (MonadIO m, MonadThrow m) => AppHandle -> ChangePlaidToken -> m (Web.WebApiHttpResponse UserToken)
-- exchangeTokenEndpoint AppHandle {..} ChangePlaidToken {..} = do
--     dataLoad <-  liftIO . flip runSqlPersistMPool appHandleDbPool $ do
--                     user <- loadUserById userKey
--                     plaidToken <- loadPaidTokenEntity payToken
--                     pure (user,plaidToken)
--     case dataLoad of
--         (Just ( Entity _ User {..}), Just ( Entity _ PaidToken {..})) ->  do
--             if (userBillT - paidTokenAmount) >= 0 
--                 then do
--                     newToken <- liftIO $ getRandomByteString appHandleRandomGen  30
--                     _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $ do
--                             updateUserBonusBill userKey (userBillT - paidTokenAmount)
--                             createUserTokenEntity CreateToken { 
--                                     userId = userIdChange,
--                                     typeEvents = paidTokenTypeAction,
--                                     textToken = toHexText newToken ,
--                                     isActive = True
--                             }
--                     pure $ Web.result $ UserToken (toHexText newToken) userIdChange
--                 else pure $ Web.failWith (Web.mkWebApiHttpError "Not enough money for operation" "NotEnoughMoney ")

--         _ -> pure $ Web.failWith (Web.mkWebApiHttpError "Not found Entity " "NotFoundEntity ")

--     where
--         userKey = UserKey $ fromIntegral userIdChange
--         toHexText = T.decodeLatin1 . LB.toStrict . B.toLazyByteString . B.byteStringHex
     
deactivateTokenEndpoint :: 
    (MonadIO m, MonadThrow m) => AppHandle -> UserToken -> m (Web.WebApiHttpResponse ())
deactivateTokenEndpoint AppHandle {..} UserToken {..} = do
    _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                    activateUserToken userToken
    pure $ Web.result ()



