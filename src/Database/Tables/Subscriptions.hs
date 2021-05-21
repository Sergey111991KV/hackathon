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

module Database.Tables.Subscriptions where


import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
import Database.Persist.TH



share
  [mkPersist sqlSettings, mkMigrate "migrateSubscriptions"]
  [persistLowerCase|
    Subscriptions
     createdAt Time.UTCTime
     userId Int
     type T.Text
     dateEnd Time.UTCTime
 |]