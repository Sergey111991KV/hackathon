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

module Database.Tables.Events where


import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )


share
  [mkPersist sqlSettings, mkMigrate "migrateEvents"]
  [persistLowerCase|
    Events
     created Time.UTCTime
     name Int
     type T.Text
     url T.Text
     dateEvents Time.UTCTime
     prizeFirstCategories Int
     prizeSecondCategories Int
     prizeTrirdCategories Int
     price Int
     deriving Show
 |]