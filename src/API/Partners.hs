{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.Partners
  
 where

-- import qualified Config as C
-- import Model.User (UserSerializer)
-- import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant 
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest
import Database.Tables.Events

-- data TestResponse = TestResponse
--    { responseStatus :: Bool,
--      responseText :: T.Text
--    }

type PartnersAPI = "Partners" :> (GetPartnersToken :<|> SendPartnersToken)

type GetPartnersToken = "get" :> Capture "id" Int :> Get '[JSON] (Web.WebApiHttpResponse T.Text)

type SendPartnersToken = "save" :> ReqBody  '[JSON] Events :>  Post '[JSON] (Web.WebApiHttpResponse ())