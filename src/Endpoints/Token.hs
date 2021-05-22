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


sendTokenEndpoint ::
    (MonadIO m, MonadThrow m) => AppHandle -> ChangePlaidToken -> m (Web.WebApiHttpResponse (Maybe UserToken))
sendTokenEndpoint AppHandle {..} ChangePlaidToken {..} = do
    (user,plaidToken) <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                    loadUserById userKey
    --    loadPaidTokenEntity token
    undefined
    where
        userKey = (UserKey $ fromIntegral userId)
     



--     data CreateToken = CreateToken {
--      userId :: Int,dlcn--      typeEvents :: T.Text,
--      textToken :: T.Text ,
--      isActive :: Bool
-- }

-- createUserTokenEntity :: MonadUnliftIO m =>
--   CreateToken ->
--   SqlPersistT m (P.Key Token, Time.UTCTime)
    -- tokenEnt <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
    --    loadPaidTokenEntity token
    -- case tokenEnt of
    --     Nothing -> pure $ Web.failWith (Web.mkWebApiHttpError "Cann't find token " "UndefinedToken")
    --     Just (Entity _ PaidToken {..}) -> do
    --         maybeUser <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
    --             loadUserById  userKey
    --         case maybeUser of
    --             Nothing -> pure $ Web.failWith (Web.mkWebApiHttpError "Not found User " "UndefinedUserId")
    --             Just (Entity _ User {..}) -> do
    --                 if userBillT < paidTokenAmount 
    --                     then pure $ Web.failWith (Web.mkWebApiHttpError "Not enough money " "NotEnoughMoney ")
    --                     else do
    --                         _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
    --                             updateUserBill userKey (userBillT - paidTokenAmount )
    --                         case paidTokenTypeAction of
    --                             Events  -> do
    --                                 _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
    --                                     creatUserEvents userId paidTokenIdAction
    --                                 pure $ Web.result ()
    --                             Subscriptions -> do
    --                                 _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
    --                                     creatUserSubsriptions userId paidTokenIdAction
    --                                 pure $ Web.result ()                         
    -- where
    --     userKey = P.toSqlKey $ fromIntegral userId

            
getTokenEndpoint :: 
    (MonadIO m, MonadThrow m) => AppHandle -> DeactivateUserToken -> m (Web.WebApiHttpResponse ())
getTokenEndpoint AppHandle {..} DeactivateUserToken {..} = do
    _ <-  liftIO . flip runSqlPersistMPool appHandleDbPool $
                    activateUserToken token
    pure $ Web.result ()

    

-- type TokenAPI = "token" :> (GetToken :<|> SendToken)

-- type GetToken = "get" :> ReqBody '[JSON] ChangePlaidToken:> Get '[JSON] (Web.WebApiHttpResponse (Maybe UserToken))

-- type SendToken = "send" :> ReqBody  '[JSON] DeactivateUserToken :>  Post '[JSON] (Web.WebApiHttpResponse ())
-- module Model.TokenRequest where

-- import qualified Data.Aeson as J

-- import GHC.Generics (Generic)

-- import qualified Data.Text as T

-- data ChangePlaidToken = ChangePlaidToken {
--     payToken :: T.Text,
--     userIdChange :: Int
-- }
--   deriving (Show, Eq, Generic)

-- instance J.ToJSON ChangePlaidToken

-- instance J.FromJSON ChangePlaidToken

-- data DeactivateUserToken = DeactivateUserToken {
--     token :: T.Text
-- }
--   deriving (Show, Eq, Generic)

-- instance J.ToJSON DeactivateUserToken

-- instance J.FromJSON DeactivateUserToken

-- data UserToken = UserToken {
--     userToken :: T.Text,
--     userId :: Int
-- }
--  deriving (Show, Eq, Generic)

-- instance J.ToJSON UserToken

-- instance J.FromJSON UserToken