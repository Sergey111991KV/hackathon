{-# LANGUAGE RecordWildCards #-}

module Endpoints.Users where

import AppHandle (AppHandle (..))
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database
import Database.Persist.Postgresql
import Model.UserSerializer (UserSerializer (..))
-- import Model.AllInformation

getUserByIdEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Maybe UserSerializer)
getUserByIdEndpoint AppHandle {..} userId =
  fmap (fmap mapUser) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      loadUserById (UserKey $ fromIntegral userId)
  where
    mapUser entity =
      let User {..} = entityVal entity
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

saveUserEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> UserCreation -> m ()
saveUserEndpoint AppHandle {..} userCreation = do
  _ <-
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      createUserRecord userCreation
  return ()


-- getAllInformation :: 
--   (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (AllInformation)
-- getAllInformation userId = do
--   user <-  fmap (fmap mapUser) $
--     liftIO . flip runSqlPersistMPool appHandleDbPool $
--       loadUserById (UserKey $ fromIntegral userId)

--     -- loadAllUserEvents