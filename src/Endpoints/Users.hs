{-# LANGUAGE RecordWildCards #-}

module Endpoints.Users where

import AppHandle (AppHandle (..))
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Database  as DB
import Database.Persist.Postgresql
import Model.UserSerializer (UserSerializer (..))
import Model.AllInformation
-- import qualified Database.Tables.Transaction as DB
import Model.Transaction 
import Model.UserSubsriptions
import Model.UserEvents
import qualified Database.Tables.UserEvent as DB
import qualified Database.Tables.UserSubsription as DB
import qualified Ext.HTTP.Error as Web
import qualified Ext.HTTP.Response as Web

getUserByIdEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Maybe UserSerializer)
getUserByIdEndpoint AppHandle {..} userId =
  fmap (fmap mapUser) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      DB.loadUserById (DB.UserKey $ fromIntegral userId)

saveUserEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> DB.UserCreation -> m ()
saveUserEndpoint AppHandle {..} userCreation = do
  _ <-
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      DB.createUserRecord userCreation
  return ()


getAllInformation :: 
  (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Web.WebApiHttpResponse AllInformation)
getAllInformation AppHandle {..} userId = do
  
  (userA, transactionA, eventsA, subsriptionsA) <-  liftIO . flip runSqlPersistMPool appHandleDbPool $ 
      do
        user <- DB.loadUserById (DB.UserKey $ fromIntegral userId)
        transaction <- DB.loadAllTransaction userId
        events <- DB.loadAllUserEvents userId
        subsriptions <-  DB.loadAllUserSubsriptions userId
        pure (user, transaction, events, subsriptions)
  case userA of
   
    Just user -> do
      pure $ Web.result $ AllInformation {
        user = (mapUser user),
        transaction = fmap mapTransaction transactionA,
        userEvents = fmap mapEvents eventsA,
        userSubsriptions = fmap mapSubsriptions subsriptionsA
      } 
    Nothing -> pure $ Web.failWith (Web.mkWebApiHttpError "Not found user " "NotFoundUser")
  -- pure allInformation

    

 



mapUser :: Entity DB.User -> UserSerializer
mapUser entity =
      let DB.User {..} = entityVal entity
       in UserSerializer
            { userId = fromIntegral . fromSqlKey $ entityKey entity,
              createdAt = userCreatedAt,
              phone = userPhoneT,
              nativeCity = userNativeCityT,
              firstName = userFirstNameT,
              secondName = userSecondNameT,
              age = userAgeT,
              bill = userBillT,
              bonusBill = userBonusBillT,
              interests = userInterestsT,
              achievements = userAchievementsT,
              isOrganization = userIsOrganization
            }

mapTransaction :: Entity DB.Transaction  -> Transaction
mapTransaction = 
  undefined 

mapEvents :: Entity DB.UserEvent  -> UserEvents
mapEvents = undefined 

mapSubsriptions :: Entity DB.UserSubsription  -> UserSubsriptions
mapSubsriptions = undefined 