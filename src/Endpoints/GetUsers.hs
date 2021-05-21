{-# LANGUAGE RecordWildCards #-}

module Endpoints.GetUsers
   ( getUserByIdEndpoint,
   )
 where
import AppHandle (AppHandle (..))
import Database
   ( Key (UserKey),
     User (..),
     loadUserById,
   )
import Model.User (UserSerializer (..))
import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Postgresql
-- import qualified Database.Persist.Postgresql as P
-- import Servant (err404)

getUserByIdEndpoint ::
   (MonadIO m, MonadThrow m) => AppHandle -> Int -> m (Maybe UserSerializer)
getUserByIdEndpoint AppHandle {..} userId =
   fmap (fmap mapUser) $
     liftIO . flip runSqlPersistMPool appHandleDbPool $
       loadUserById (UserKey $ fromIntegral userId)
   where
     mapUser entity =
       let User {..} = entityVal entity
        in UserSerializer
             { userId = fromIntegral . fromSqlKey $ entityKey entity,
               userCreatedAt = userCreatedAt,
               userPhone = userPhone
             }