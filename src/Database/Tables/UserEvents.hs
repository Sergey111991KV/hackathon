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

module Database.Tables.UserEvents where

import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)

import qualified Data.Time as Time
import Database.Esqueleto
 
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH




share
  [mkPersist sqlSettings, mkMigrate "migrateUserEvents"]
  [persistLowerCase|
 UserEvents
     createdAt Time.UTCTime
     idEvents Int 
     userId Int
     deriving Show
 |]

creatUserEvents :: (MonadUnliftIO m) =>
  Int ->
  Int ->
  SqlPersistT m (P.Key UserEvents, Time.UTCTime)
creatUserEvents userId idEvents = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      UserEvents
        now
        userId
        idEvents
  pure (rowOrderId, now)

loadAllUserEvents :: (MonadUnliftIO m) =>
  Int ->
  SqlPersistT m [P.Entity UserEvents]
loadAllUserEvents userId = 
   select $ from $ \a -> do
    where_ (a ^. UserEventsUserId ==. val userId)
    pure a