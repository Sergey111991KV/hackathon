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
import API.Events
  

import API.User ( UserAPI )


type API = 
  PlaidTokenAPI :<|> UserAPI :<|> TokenAPI :<|> EventsAPI 
 

apiType :: Proxy API
apiType = Proxy