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

module Database.Tables.UserEvent where

import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)

import qualified Data.Time as Time
import Database.Esqueleto
 
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH




share
  [mkPersist sqlSettings, mkMigrate "migrateUserEvent"]
  [persistLowerCase|
 UserEvent
     createdAt Time.UTCTime
     idEvents Int 
     userId Int
     endDate Time.Day
     deriving Show
 |]

creatUserEvents :: (MonadUnliftIO m) =>
  Int ->
  Int ->
    Time.Day ->
  SqlPersistT m (P.Key UserEvent, Time.UTCTime)
creatUserEvents userId idEvents endDate = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      UserEvent
        now
        userId
        idEvents
        endDate
  pure (rowOrderId, now)

loadAllUserEvents :: (MonadUnliftIO m) =>
  Int ->
  SqlPersistT m [P.Entity UserEvent]
loadAllUserEvents userId = 
   select $ from $ \a -> do
    where_ (a ^. UserEventUserId ==. val userId)
    pure a
