{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.Token where

import qualified Data.Text as T
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest
import Model.TokenRequest
import Servant

type TokenAPI = "token" :> (GetToken :<|> SendToken)

type GetToken = "exchange" :> ReqBody '[JSON] ChangePlaidToken :> Get '[JSON] (Web.WebApiHttpResponse UserToken)

type SendToken = "deactivate" :> ReqBody '[JSON] UserToken :> Post '[JSON] (Web.WebApiHttpResponse ())