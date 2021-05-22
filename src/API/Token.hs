{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.Token
  
 where

-- import qualified Config as C
-- import Model.User (UserSerializer)
-- import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant 
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest
import Model.TokenRequest

-- data TestResponse = TestResponse
--    { responseStatus :: Bool,
--      responseText :: T.Text
--    }

type TokenAPI = "token" :> (GetToken :<|> SendToken)

type GetToken = "exchange" :> ReqBody '[JSON] ChangePlaidToken:> Get '[JSON] (Web.WebApiHttpResponse  UserToken)

type SendToken = "deactivate" :> ReqBody  '[JSON] UserToken :>  Post '[JSON] (Web.WebApiHttpResponse ())