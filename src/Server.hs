{-# LANGUAGE DataKinds #-}

 module Server
   ( runDevServer,
   )
 where

 import API (API, apiType)
 import AppHandle (AppHandle (..), withAppHandle)
 import qualified Config as C
 import Database (withDbPool, withDbPoolDebug)
 import Endpoints.GetUsers
   ( getUserByIdEndpoint,
   )
 import Control.Exception.Safe (MonadThrow, throw, try)
 import Control.Monad (unless)
 import Control.Monad.Except (ExceptT (ExceptT))
 import Control.Monad.IO.Class (MonadIO, liftIO)
 import qualified Data.Pool as Pool
 import Data.Proxy (Proxy (Proxy))
 import Database.Persist.Sql (SqlBackend)
 import Ext.Data.Env (Env (..))
 import Ext.Logger.Colog (logTextStdout)
 import Network.Wai.Handler.Warp
   ( Settings,
     defaultSettings,
     runSettings,
     setBeforeMainLoop,
     setLogger,
     setPort,
   )
 import Network.Wai.Logger (withStdoutLogger)
 import Network.Wai.Middleware.Cors
   ( cors,
     corsRequestHeaders,
     simpleCorsResourcePolicy,
   )
 import Network.Wai.Middleware.Servant.Options (provideOptions)
 import Servant
   ( Handler (Handler),
     Server,
     ServerT,
     err404,
     hoistServer,
     serve,
     serveWithContext,
     (:<|>) (..),
   )

 handler ::
   (MonadIO m, MonadThrow m) =>
   AppHandle ->
   ServerT API m
 handler h = testEndpoint :<|> getUserByIdEndpoint h
   where
     testEndpoint param =
       pure $ "You sent me " <> param

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