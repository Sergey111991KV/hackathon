{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)

-- rawSql imports.
import Database.Persist.Sql (rawQuery, insert)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Tutorial
   title    Text
   url      Text
   school   Bool
   deriving Show
|]
-- show
main = do
    sql <- liftIO $ BS.readFile "./NewFile.sql"
    print "sql"
    runSqlite "NewFile.sql" $ do
    buildDb
    dumpTable

buildDb = do
    runMigrationSilent migrateTables
    insert $ Tutorial "Basic Haskell" "https://fpcomplete.com/school/basic-haskell-1" True
    insert $ Tutorial "A monad tutorial" "https://fpcomplete.com/user/anne/monads" False
    insert $ Tutorial "Yesod usage" "https://fpcomplete.com/school/basic-yesod" True
    insert $ Tutorial "Putting the FUN in functors" "https://fpcomplete.com/user/anne/functors" False
    insert $ Tutorial "Basic Haskell" "https://fpcomplete/user/anne/basics" False

-- /show

dumpTable = rawQuery "select * from Tutorial" [] $$ CL.mapM_ (liftIO . print)