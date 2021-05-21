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

module Database.Tables.PaidToken where

import Control.Monad.IO.Unlift 
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
 
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
share
  [mkPersist sqlSettings, mkMigrate "migratePaidToken"]
  [persistLowerCase|
    PaidToken
     createdAt Time.UTCTime
     userId Int
     typeEvents T.Text
     textPaidToken T.Text
 |]

loadPaidTokenEntity ::
  MonadUnliftIO m =>
  T.Text ->
  SqlPersistT m (Maybe (P.Entity PaidToken))
loadPaidTokenEntity token =
  fmap listToMaybe . select $
    from $ \user -> do
      where_ $ user ^. PaidTokenTextPaidToken ==. val token
      pure user