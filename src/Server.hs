{-# LANGUAGE DataKinds #-}

module Server
  ( runDevServer,
  )
where

import API (API, apiType)
import AppHandle (AppHandle (..), withAppHandle)
import qualified Config as C
import Control.Exception.Safe (MonadThrow, try)
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Endpoints.PlaidToken
import Endpoints.Token
import Endpoints.Users
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
  ( cors,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
import Endpoints.Events

-- getAllEventsEndpoint ::  (MonadIO m, MonadThrow m) =>  AppHandle -> m [Events]
-- getAllEventsEndpoint AppHandle {..} = do
--     events <- liftIO . flip runSqlPersistMPool appHandleDbPool $
--       loadAllEvents 
--     return $ fmap entityVal events

-- getEventsEndpoint ::
--   (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Maybe Events)
-- getEventsEndpoint AppHandle {..} eventsId =
--   fmap (fmap entityVal) $
--     liftIO . flip runSqlPersistMPool appHandleDbPool $
--       loadEventsById ( P.toSqlKey $ fromIntegral eventsId)
  
     
-- saveEventsEndpoint ::


handler ::
  (MonadIO m, MonadThrow m) =>
  AppHandle ->
  ServerT API m
handler h = paidToken :<|> user :<|> token :<|>  events
  where
    paidToken = getPaidTokenEndpoint h
    user =
      getUserByIdEndpoint h
        :<|> saveUserEndpoint h
    token =
      exchangeTokenEndpoint h
        :<|> deactivateTokenEndpoint h
    events = 
      getEventsEndpoint h
        :<|> saveEventsEndpoint h
          :<|> getAllEventsEndpoint h
catchServantErrorsFromIO :: ServerT API IO -> Server API
catchServantErrorsFromIO = hoistServer apiType (Handler . ExceptT . try)

runServer :: C.Config -> IO ()
runServer config = do
  port <- C.getPort config
  let serverSettings =
        setPort port $
          setBeforeMainLoop
            (putStrLn ("listening on port " <> show port))
            defaultSettings
      policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}
      server :: MonadIO (m IO) => Settings -> AppHandle -> m IO ()
      server settings ah =
        liftIO
          . runSettings settings
          . cors (const $ Just policy)
          . provideOptions apiType
          . serve apiType
          . catchServantErrorsFromIO
          . handler
          $ ah
  liftIO $ withAppHandle $ server serverSettings

runDevServer :: IO ()
runDevServer = do
  config <- C.retrieveConfig
  runServer config