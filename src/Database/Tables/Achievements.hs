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

import Control.Monad.IO.Unlift
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH


share
  [mkPersist sqlSettings, mkMigrate "migrateAchievement"]
  [persistLowerCase|
    Achievement
     name Int
     logo T.Text
     deriving Show
 |]

createAchievementsRecord ::
  (MonadUnliftIO m) =>
  Achievement ->
  SqlPersistT m (P.Key Achievement, Time.UTCTime)
createAchievementsRecord subscriptions = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert subscriptions

  pure (rowOrderId, now)

loadAchievementsById ::
  MonadUnliftIO m =>
  P.Key Achievement ->
  SqlPersistT m (Maybe (P.Entity Achievement))
loadAchievementsById achievementId =
  fmap listToMaybe . select $
    from $ \s -> do
      where_ $ s ^. AchievementId ==. val achievementId
      pure s

