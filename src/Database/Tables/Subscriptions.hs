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

module Database.Tables.Subscriptions where


import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
import Database.Persist.TH
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import qualified Database.Persist.Postgresql as P
import Data.Maybe (listToMaybe)



share
  [mkPersist sqlSettings, mkMigrate "migrateSubscriptions"]
  [persistLowerCase|
    Subscriptions
     created Time.UTCTime
     nameOrganization T.Text 
     userUUID T.Text 
     description T.Text 
 |]

data SubscriptionsCreate = SubscriptionsCreate {
      nameOrganization :: T.Text,
      userUUID :: T.Text,
      description :: T.Text 
}

createSubscriptionsRecord ::
  (MonadUnliftIO m) => SubscriptionsCreate ->
  SqlPersistT m (P.Key Subscriptions, Time.UTCTime)
createSubscriptionsRecord SubscriptionsCreate {..} = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      Subscriptions 
        now
        nameOrganization
        userUUID
        description
  pure (rowOrderId, now)

loadSubscriptionsById ::
  MonadUnliftIO m =>
  P.Key Subscriptions ->
  SqlPersistT m (Maybe (P.Entity Subscriptions))
loadSubscriptionsById subscriptionsId =
  fmap listToMaybe . select $
    from $ \s  -> do
      where_ $ s ^. SubscriptionsId ==. val subscriptionsId
      pure s

loadAllSubscriptions :: (MonadUnliftIO m) =>
  SqlPersistT m [P.Entity Subscriptions]
loadAllSubscriptions  = 
   select $ from pure