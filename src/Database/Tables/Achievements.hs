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

module Database.Tables.Achievements where

import qualified Data.Text as T 


import Database.Esqueleto

import Database.Persist.TH
import Control.Monad.IO.Unlift 
import qualified Database.Persist.Postgresql as P
import Data.Maybe (listToMaybe)
import qualified Data.Time as Time


share
  [mkPersist sqlSettings, mkMigrate "migrateAchievements"]
  [persistLowerCase|
    Achievements
     name Int
     logo T.Text
     deriving Show
 |]

createSubscriptionsRecord ::
  (MonadUnliftIO m) => Achievements ->
  SqlPersistT m (P.Key Achievements, Time.UTCTime)
createSubscriptionsRecord subscriptions = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert subscriptions
      
  pure (rowOrderId, now)

loadSubscriptionsById ::
  MonadUnliftIO m =>
  P.Key Achievements ->
  SqlPersistT m (Maybe (P.Entity Achievements))
loadSubscriptionsById achievementsId =
  fmap listToMaybe . select $
    from $ \s  -> do
      where_ $ s ^. AchievementsId ==. val achievementsId
      pure s