{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.PlaidToken where

import qualified Data.Text as T
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest
import Servant

type PlaidTokenAPI = "plaid-token" :> GetPlaidToken

type GetPlaidToken = "get" :> ReqBody '[JSON] PlaidTokenGet :> Get '[JSON] (Web.WebApiHttpResponse T.Text)
