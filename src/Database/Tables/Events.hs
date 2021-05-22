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

module Database.Tables.Events where


import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )
import qualified Database.Persist.Postgresql as P
import Control.Monad.IO.Unlift 
import Data.Maybe (listToMaybe)

share
  [mkPersist sqlSettings, mkMigrate "migrateEvents"]
  [persistLowerCase|
    Events
     created Time.UTCTime
     name Int
     type T.Text
     url T.Text
     dateEvents Time.UTCTime
     prizeFirstType T.Text Maybe 
     prizeFirstCategories Int Maybe
     prizeSecondType T.Text Maybe
     prizeSecondCategories Int Maybe
     prizeTrirdType T.Text Maybe
     prizeTrirdCategories Int Maybe
     price Int
     deriving Show
 |]

createSubscriptionsRecord ::
  (MonadUnliftIO m) => Events ->
  SqlPersistT m (P.Key Events, Time.UTCTime)
createSubscriptionsRecord subscriptions = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert subscriptions
      
  pure (rowOrderId, now)

loadSubscriptionsById ::
  MonadUnliftIO m =>
  P.Key Events ->
  SqlPersistT m (Maybe (P.Entity Events))
loadSubscriptionsById eventsId =
  fmap listToMaybe . select $
    from $ \s  -> do
      where_ $ s ^. EventsId ==. val eventsId
      pure s