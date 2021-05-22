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
     createdAt Time.UTCTime
     userId Int
     type T.Text
     dateEnd Time.UTCTime
 |]

data SubscriptionsCreate = SubscriptionsCreate {

}

createSubscriptionsRecord ::
  (MonadUnliftIO m) => Subscriptions ->
  SqlPersistT m (P.Key Subscriptions, Time.UTCTime)
createSubscriptionsRecord subscriptions = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert subscriptions
      
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