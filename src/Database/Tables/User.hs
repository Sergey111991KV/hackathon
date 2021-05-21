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
-- import Model.Achievements


share
  [mkPersist sqlSettings, mkMigrate "migrateUser"]
  [persistLowerCase|
 User
     createdAt Time.UTCTime
     phone T.Text
     nativeCity City
     firstName T.Text
     secondName T.Text
     age Int
     achievements [Int]
     bill Int
     bonusBill Int
     deriving Show
 |]

createUserRecord ::
  (MonadUnliftIO m) =>
  T.Text ->
  City ->
  T.Text ->
  T.Text ->
  Int ->
  SqlPersistT m (P.Key User, Time.UTCTime)
createUserRecord phone city fstName sndName age = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      User
        now
        phone
        city
        fstName
        sndName
        age
        []
        0
        0
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
    set user [UserBonusBill =. val bonusBill]
    where_ $ user ^. UserId ==. val keyUser

updateUserAchievements ::
  MonadUnliftIO m =>
  P.Key User ->
  [Int] ->
  SqlPersistT m ()
updateUserAchievements keyUser bonusBill = do
  update $ \user -> do
    set user [UserAchievements =. val bonusBill]
    where_ $ user ^. UserId ==. val keyUser

updateUserBill ::
  MonadUnliftIO m =>
  P.Key User ->
  Int ->
  SqlPersistT m ()
updateUserBill keyUser bill = do
  update $ \user -> do
    set user [UserBill =. val bill]
    where_ $ user ^. UserId ==. val keyUser
