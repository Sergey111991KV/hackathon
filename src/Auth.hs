{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auth
  -- ( SAS.defaultJWTSettings,
  --   retrieveKey,
  --   module Export,
  --   ProtectedServantJWTCtx,
  --   servantCtx,
  --   getAuthUser,
  --   checkUserClientId,
  --   ProtectedWithJWT,
  -- )
where

-- import AppHandle
-- import AppName.Auth.ServantInstances ()
-- import AppName.Auth.User as Export
-- import Control.Exception.Safe (throw)
-- import Control.Monad.Except (MonadIO (liftIO))
-- import Crypto.JOSE.JWK (JWK)
import Data.Proxy


-- import qualified Ext.Logger.Colog as Log
-- import Lens.Micro.Platform ((&), (<>~))
-- import Servant
import qualified Servant.Auth.Server as SAS


-- retrieveKey :: FilePath -> IO JWK
-- retrieveKey = SAS.readKey

-- type ProtectedWithJWT = SAS.Auth '[SAS.JWT] AuthenticatedUser

type ProtectedServantJWTCtx = Proxy '[SAS.CookieSettings, SAS.JWTSettings]

-- servantCtx :: JWK -> Context '[SAS.CookieSettings, SAS.JWTSettings]
-- servantCtx authKey =
--   let jwtSettings = SAS.defaultJWTSettings authKey
--    in SAS.defaultCookieSettings :. jwtSettings :. EmptyContext

-- getAuthUser :: SAS.AuthResult AuthenticatedUser -> Maybe AuthenticatedUser
-- getAuthUser (SAS.Authenticated user) = Just user
-- getAuthUser _ = Nothing

-- getClientUserId :: SAS.AuthResult AuthenticatedUser -> Maybe Int
-- getClientUserId authUser = do
--   user <- getAuthUser authUser
--   case user of
--     AuthenticatedClient userId -> pure userId
--     _ -> Nothing

-- checkUserClientId :: (MonadHandler m) => SAS.AuthResult AuthenticatedUser -> m Int
-- checkUserClientId authUser = maybe throwAuthError pure $ getClientUserId authUser
--   where
--     throwAuthError = do
--       Log.logError "Unauthorized access"
--       liftIO $ throw err401


