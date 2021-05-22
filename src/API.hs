{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API
   ( API,
     apiType,
   )
 where


import Servant 
import API.PlaidToken ( PlaidTokenAPI )
import API.Token
  

import API.User ( UserAPI )


type API = 
  PlaidTokenAPI :<|> UserAPI :<|> TokenAPI
 

apiType :: Proxy API
apiType = Proxy