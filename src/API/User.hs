{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.User
  
 where

-- import qualified Config as C
import Model.User (UserSerializer)
-- import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Servant 
import qualified Ext.HTTP.Response as Web
import Model.PlaidTokenRequest

-- data TestResponse = TestResponse
--    { responseStatus :: Bool,
--      responseText :: T.Text
--    }

type UserAPI = "user" :> (GetUser :<|> SaveUser)

type GetUser = "get" :> Capture "id" Int :> Get '[JSON] (Maybe UserSerializer)

type SaveUser = "save" :> ReqBody  '[JSON] UserSerializer :>  Post '[JSON]  ()