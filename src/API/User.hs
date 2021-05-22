{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.User
  
 where

-- import qualified Config as C
import Model.UserSerializer (UserSerializer)
-- import Data.Proxy (Proxy (..))
-- import qualified Data.Text as T
import Servant 
import Database.Tables.User
-- import qualified Ext.HTTP.Response as Web


-- data TestResponse = TestResponse
--    { responseStatus :: Bool,
--      responseText :: T.Text
--    }

type UserAPI = "user" :> (GetUser :<|> SaveUser)

type GetUser = "get" :> Capture "id" Int :> Get '[JSON] (Maybe UserSerializer)

type SaveUser = "save" :> ReqBody  '[JSON] UserCreation :>  Post '[JSON]  ()