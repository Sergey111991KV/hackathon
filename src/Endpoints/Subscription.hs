module Endpoints.Subscription where

import AppHandle (AppHandle (..))
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Database
import Database.Persist.Postgresql
import Database.Tables.Subscriptions
import qualified Database.Persist.Postgresql as P
-- import Model.AllInformation

getAllSubscriptionEndpoint ::  (MonadIO m, MonadThrow m) =>  AppHandle -> m [Subscriptions]
getAllSubscriptionEndpoint AppHandle {..} = do
    events <- liftIO . flip runSqlPersistMPool appHandleDbPool $
      loadAllSubscriptions
    return $ fmap entityVal events

getSubscriptionEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Maybe Subscriptions)
getSubscriptionEndpoint AppHandle {..} eventsId =
  fmap (fmap entityVal) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      loadSubscriptionsById ( P.toSqlKey $ fromIntegral eventsId)
  
     
saveSubscriptionEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> SubscriptionsCreate -> m ()
saveSubscriptionEndpoint AppHandle {..} subscriptionsCreate = do
  _ <-
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      createSubscriptionsRecord subscriptionsCreate
  return ()
