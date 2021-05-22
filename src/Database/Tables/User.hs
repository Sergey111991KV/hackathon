{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Tables.User where

import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
    ( (=.),
      (==.),
      (^.),
      from,
      select,
      set,
      update,
      val,
      where_,
      BackendKey(SqlBackendKey),
      PersistStoreWrite(insert),
      SqlPersistT )
 
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH


import Model.City
import Model.Achievements
import Model.Interests

import qualified Data.Aeson as J

import GHC.Generics (Generic)


share
  [mkPersist sqlSettings, mkMigrate "migrateUser"]
  [persistLowerCase|
 User
     createdAt Time.UTCTime
     phoneT T.Text
     nativeCityT City
     firstNameT T.Text
     secondNameT T.Text
     ageT Int
     billT Int
     bonusBillT Int
     interestsT [Interests] Maybe 
     achievementsT [Achievements] Maybe
     isOrganization Bool Maybe
     deriving Show
 |]

data UserCreation = UserCreation {
    createdAt :: Time.UTCTime,
    phone :: T.Text,
    nativeCity :: City,
    firstName :: T.Text,
    secondName :: T.Text,
    age :: Int,
    bill :: Int,
    bonusBill :: Int,
    interests :: Maybe [Interests],
    achievements :: Maybe [Achievements],
    isOrganizationCreat :: Maybe Bool 
} deriving (Show, Eq, Generic)

instance J.ToJSON UserCreation

instance J.FromJSON UserCreation


createUserRecord ::
  (MonadUnliftIO m) => UserCreation ->
  SqlPersistT m (P.Key User, Time.UTCTime)
createUserRecord UserCreation{..} = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      User
        now
        phone
        nativeCity
        firstName
        secondName
        age
        bill
        bonusBill
        interests
        achievements
        isOrganizationCreat
  pure (rowOrderId, now)

loadUserById ::
  MonadUnliftIO m =>
  P.Key User ->
  SqlPersistT m (Maybe (P.Entity User))
loadUserById userId =
  fmap listToMaybe . select $
    from $ \user -> do
      where_ $ user ^. UserId ==. val userId
      pure user

updateUserBonusBill ::
  MonadUnliftIO m =>
  P.Key User ->
  Int ->
  SqlPersistT m ()
updateUserBonusBill keyUser bonusBill = do
  update $ \user -> do
    set user [UserBonusBillT =. val bonusBill]
    where_ $ user ^. UserId ==. val keyUser

updateUserAchievements ::
  MonadUnliftIO m =>
  P.Key User ->
  [Achievements] ->
  SqlPersistT m ()
updateUserAchievements keyUser bonusBill = do
  update $ \user -> do
    set user [UserAchievementsT =. val (Just bonusBill)]
    where_ $ user ^. UserId ==. val keyUser

updateUserBill ::
  MonadUnliftIO m =>
  P.Key User ->
  Int ->
  SqlPersistT m ()
updateUserBill keyUser bill = do
  update $ \user -> do
    set user [UserBillT =. val bill]
    where_ $ user ^. UserId ==. val keyUser
