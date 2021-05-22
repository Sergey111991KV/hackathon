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

import Control.Monad.IO.Unlift
import qualified Data.Aeson as J
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateEvents"]
  [persistLowerCase|
    Events
     created Time.UTCTime
     name T.Text
     type T.Text
     url T.Text
     dateEventsStart Time.Day 
     dateEventsEnd Time.Day 
     prizeFirstType T.Text Maybe 
     prizeFirstCategories Int Maybe
     prizeSecondType T.Text Maybe
     prizeSecondCategories Int Maybe
     prizeTrirdType T.Text Maybe
     prizeTrirdCategories Int Maybe
     price Int
     deriving Show
 |]

data EventsCreation = EventsCreation
  { creationName :: T.Text,
    creationType :: T.Text,
    creationUrl :: T.Text,
    creationDateEventsStart :: Time.Day,
    creationDateEventsEnd :: Time.Day,
    creationPrizeFirstType :: Maybe T.Text,
    creationPrizeFirstCategories :: Maybe Int,
    creationPrizeSecondType :: Maybe T.Text,
    creationPrizeSecondCategories :: Maybe Int,
    creationPrizeTrirdType :: Maybe T.Text,
    creationPrizeTrirdCategories :: Maybe Int,
    creationPrice :: Int
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON EventsCreation

instance J.FromJSON EventsCreation

createEventsRecord ::
  (MonadUnliftIO m) =>
  EventsCreation ->
  SqlPersistT m (P.Key Events, Time.UTCTime)
createEventsRecord EventsCreation{..} = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $ 
      Events 
        now
        creationName
        creationType 
        creationUrl
        creationDateEventsStart 
        creationDateEventsEnd 
        creationPrizeFirstType 
        creationPrizeFirstCategories 
        creationPrizeSecondType 
        creationPrizeSecondCategories 
        creationPrizeTrirdType 
        creationPrizeTrirdCategories 
        creationPrice 
  pure (rowOrderId, now)

loadEventsById ::
  MonadUnliftIO m =>
  P.Key Events ->
  SqlPersistT m (Maybe (P.Entity Events))
loadEventsById eventId =
  fmap listToMaybe . select $
    from $ \s -> do
      where_ $ s ^. EventsId ==. val eventId
      pure s

loadAllEvents ::
  (MonadUnliftIO m) =>
  SqlPersistT m [P.Entity Events]
loadAllEvents =
  select $ from pure