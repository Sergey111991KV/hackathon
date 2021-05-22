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
import Model.TypePaidAction ( TypePaidAction )


share
  [mkPersist sqlSettings, mkMigrate "migratePaidToken"]
  [persistLowerCase|
    PaidToken
     createdAt Time.UTCTime
     typeAction TypePaidAction
     idAction Int
     textPaidToken T.Text
     amount Int
 |]

createPaidTokenEntity :: MonadUnliftIO m =>
  TypePaidAction ->
  Int ->
  T.Text ->
  Int ->
  SqlPersistT m (P.Key PaidToken, Time.UTCTime)
createPaidTokenEntity typePaidAction idAction textPaidToken amount = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      PaidToken
        now
        typePaidAction
        idAction
        textPaidToken
        amount
  pure (rowOrderId, now)


loadPaidTokenEntity ::
  MonadUnliftIO m =>
  T.Text ->
  SqlPersistT m (Maybe (P.Entity PaidToken))
loadPaidTokenEntity token =
  fmap listToMaybe . select $
    from $ \plaidToken -> do
      where_ $ plaidToken ^. PaidTokenTextPaidToken ==. val token
      pure plaidToken