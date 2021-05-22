{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.PlaidToken
  
 where

-- import qualified Config as C
-- import Model.User (UserSerializer)
-- import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant 
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest

-- data TestResponse = TestResponse
--    { responseStatus :: Bool,
--      responseText :: T.Text
--    }

type PlaidTokenAPI = "plaid-token" :> GetPlaidToken

type GetPlaidToken = "get" :> ReqBody '[JSON] PlaidTokenGet :> Get '[JSON] (Web.WebApiHttpResponse T.Text)

