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

module Database.Tables.UserSubsriptions where

import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)


import qualified Data.Time as Time
import Database.Esqueleto
 
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH




share
  [mkPersist sqlSettings, mkMigrate "migrateUserSubsriptions"]
  [persistLowerCase|
 UserSubsriptions
     createdAt Time.UTCTime
     idEvents Int 
     userId Int
     deriving Show
 |]

creatUserSubsriptions :: (MonadUnliftIO m) =>
  Int ->
  Int ->
  SqlPersistT m (P.Key UserSubsriptions, Time.UTCTime)
creatUserSubsriptions userId idEvents = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      UserSubsriptions
        now
        userId
        idEvents
  pure (rowOrderId, now)

loadAllUserSubsriptions :: (MonadUnliftIO m) =>
  Int ->
  SqlPersistT m [P.Entity UserSubsriptions]
loadAllUserSubsriptions userId = 
   select $ from $ \a -> do
    where_ (a ^. UserSubsriptionsUserId ==. val userId)
    pure a