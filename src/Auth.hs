{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auth
 
where


import Data.Proxy



import qualified Servant.Auth.Server as SAS



type ProtectedServantJWTCtx = Proxy '[SAS.CookieSettings, SAS.JWTSettings]

