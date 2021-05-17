{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Lib

type DBConnectionString = ByteString

type Message = String

type API = ReqBody '[PlainText] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]

initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"
  return ()

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server conns = postMessage :<|> getMessages

  where postMessage :: Message -> Handler NoContent
        postMessage msg = do
          liftIO . withResource conns $ \conn ->
            execute conn
                    "INSERT INTO messages VALUES (?)"
                    (Only msg)
          return NoContent

        getMessages :: Handler [Message]
        getMessages = fmap (map fromOnly) . liftIO $
         withResource conns $ \conn ->
            query_ conn "SELECT msg FROM messages"


initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)

postMsg :: Message -> ClientM NoContent
getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

main :: IO ()
main = do
  -- you could read this from some configuration file,
  -- environment variable or somewhere else instead.
  -- you will need to either change this connection string OR
  -- set some environment variables (see
  -- https://www.postgresql.org/docs/9.5/static/libpq-envars.html)
  -- to point to a running PostgreSQL server for this example to work.
  let connStr = ""
  pool <- initConnectionPool connStr
  initDB connStr
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp pool) killThread $ \_ -> do
    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      postMsg "hello"
      postMsg "world"
      getMsgs
    print ms