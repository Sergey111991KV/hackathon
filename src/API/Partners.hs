{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.Partners where

import qualified Data.Text as T
import Database.Tables.Events
import qualified Ext.HTTP.Response as Web
import Servant

type PartnersAPI = "Partners" :> (GetPartnersToken :<|> SendPartnersToken)

type GetPartnersToken = "get" :> Capture "id" Int :> Get '[JSON] (Web.WebApiHttpResponse T.Text)

type SendPartnersToken = "save" :> ReqBody '[JSON] Events :> Post '[JSON] (Web.WebApiHttpResponse ())