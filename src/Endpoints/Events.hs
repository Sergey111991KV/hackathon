module Endpoints.Events where

import AppHandle (AppHandle (..))
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Database
import Database.Persist.Postgresql
    ( Entity(entityVal), runSqlPersistMPool )
import qualified Database.Tables.Events as E
import qualified Database.Persist.Postgresql as P
import Model.Events 

-- import Model.AllInformation

getAllEventsEndpoint ::  (MonadIO m, MonadThrow m) =>  AppHandle -> m [Events]
getAllEventsEndpoint AppHandle {..} = do
    events <- liftIO . flip runSqlPersistMPool appHandleDbPool $
      E.loadAllEvents 
    return $ fmap convertEvents events

getEventsEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Maybe Events)
getEventsEndpoint AppHandle {..} eventsId =
  fmap (fmap convertEvents) $
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      E.loadEventsById ( P.toSqlKey $ fromIntegral eventsId)
  
     
saveEventsEndpoint ::
  (MonadIO m, MonadThrow m) => AppHandle -> E.EventsCreation -> m ()
saveEventsEndpoint AppHandle {..} eventsCreation = do
  _ <-
    liftIO . flip runSqlPersistMPool appHandleDbPool $
      E.createEventsRecord eventsCreation
  return ()

convertEvents :: Entity E.Events -> Events
convertEvents ( P.Entity _ E.Events {..}) = Events 
    { createdE = eventsCreated,
    nameE = eventsName,
    typeE = eventsType,
    urlE = eventsUrl,
    dateEventsStartE  = eventsDateEventsStart,
    dateEventsEndE = eventsDateEventsEnd,
    prizeFirstTypeE = eventsPrizeFirstType,
    prizeFirstCategoriesE = eventsPrizeFirstCategories,
    prizeSecondTypeE = eventsPrizeSecondType,
    prizeSecondCategoriesE = eventsPrizeSecondCategories,
    prizeTrirdTypeE = eventsPrizeTrirdType ,
    prizeTrirdCategoriesE = eventsPrizeTrirdCategories,
    priceE = eventsPrice
  }
-- created Time.UTCTime
--      name T.Text
--      type T.Text
--      url T.Text
--      dateEventsStart Time.Day 
--      dateEventsEnd Time.Day 
--      prizeFirstType T.Text Maybe 
--      prizeFirstCategories Int Maybe
--      prizeSecondType T.Text Maybe
--      prizeSecondCategories Int Maybe
--      prizeTrirdType T.Text Maybe
--      prizeTrirdCategories Int Maybe
--      price Int


-- createEventsRecord ::
--   (MonadUnliftIO m) =>
--   EventsCreation -> ()

-- loadEventsById ::
--   MonadUnliftIO m =>
--   P.Key Events ->
--   SqlPersistT m (Maybe (P.Entity Events))

-- loadAllEvents ::
--   (MonadUnliftIO m) =>
--   SqlPersistT m [P.Entity Events]
-- loadAllEvents =
--   select $ from pure