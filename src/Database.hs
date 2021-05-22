module Database
  ( module Exports,
    runAllMigrations,
  )
where

import Database.Connection as Exports
import Database.Setup as Exports
import Database.Tables.User as Exports
import qualified Config as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Postgresql
import  Database.Tables.Transaction  as Exports
import Database.Tables.Subscriptions 
import Database.Tables.Achievements  as Exports
import Database.Tables.Events
import Database.Tables.PaidToken 
import Database.Tables.Partners 
import Database.Tables.Token
import Database.Tables.UserEvent
import Database.Tables.UserSubsription




allMigrations :: [Migration]
allMigrations =
  [ migrateUser,
    migrateTransaction,
    migrateAchievement,
    migrateEvents,
    migrateSubscriptions,
    migratePartners,
    migratePaidToken,
    migrateToken,
    migrateUserEvent,
    migrateUserSubsription
  ]


migrateAll :: (MonadIO m) => SqlPersistT m ()
migrateAll = mapM_ runMigration allMigrations

runAllMigrations :: IO ()
runAllMigrations = do
  conf <- C.retrieveConfig
  withDbPoolDebug conf $ \pool -> liftIO $ runSqlPersistMPool migrateAll pool