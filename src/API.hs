{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API
   ( API,
     apiType,
   )
 where

-- import qualified Config as C
import Model.User (UserSerializer)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant 
import API.PlaidToken
import qualified Ext.HTTP.Response as Web
-- data TestResponse = TestResponse
--    { responseStatus :: Bool,
--      responseText :: T.Text
--    }
import API.User


type API = 
  PlaidTokenAPI :<|> UserAPI 
  --  "test-endpoint-with" :> Capture "echotext" T.Text :> Get '[JSON] T.Text 
  --   :<|> "get-user-by-id" :> Capture "userId" Int :> Get '[JSON] (Maybe UserSerializer)
  --   :<|> "send" :> Capture "userId" Int :> Capture "userId" T.Text :>  Get '[JSON] (Web.WebApiHttpResponse ())

apiType :: Proxy API
apiType = Proxy