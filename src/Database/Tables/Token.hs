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

module Database.Tables.Token where

import Control.Monad.IO.Unlift 
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.Esqueleto
 
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
share
  [mkPersist sqlSettings, mkMigrate "migrateToken"]
  [persistLowerCase|
    Token
     createdAt Time.UTCTime
     userId Int
     typeEvents T.Text
     textToken T.Text
     isActive Bool
 |]

loadServerTokenEntity ::
  MonadUnliftIO m =>
  T.Text ->
  SqlPersistT m (Maybe (P.Entity Token))
loadServerTokenEntity token =
  fmap listToMaybe . select $
    from $ \user -> do
      where_ $ user ^. TokenTextToken ==. val token
      pure user