module Ext.HTTP.Client where

import Data.Aeson (FromJSON, eitherDecode')
import Network.HTTP.Client (Manager, Request, Response (..), httpLbs)

httpJson :: FromJSON a => Request -> Manager -> IO (Response a)
httpJson req man = do
  resp <- httpLbs req man
  let body = responseBody resp
  parsed <- either (parseError body) pure $ eitherDecode' body
  pure resp {responseBody = parsed}
  where
    parseError body = error . (("parse json error:\n" <> show body <> "\n") <>)
