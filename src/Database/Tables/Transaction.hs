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

module Database.Tables.Transaction where


import qualified Data.Time as Time
import Database.Esqueleto
 

import Database.Persist.TH


share
  [mkPersist sqlSettings, mkMigrate "migrateTransaction"]
  [persistLowerCase|
    Transaction
     createdAt Time.UTCTime
     userId Int
     fromId Int
     toId Int
     amount Int
     deriving Show
 |]

-- createTransaction