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
import Model.TypePaidAction ( TypePaidAction )
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import qualified Database.Persist.Postgresql as P
 

import Database.Persist.TH


share
  [mkPersist sqlSettings, mkMigrate "migrateTransaction"]
  [persistLowerCase|
    Transaction
     createdAt Time.UTCTime
     userId Int
     fromId Int
     toIdType TypePaidAction
     toId Int
     amount Int
     deriving Show
 |]



data CreateTransaction = CreateTransaction {
     userId :: Int,
     fromId :: Int,
     toIdType :: TypePaidAction,
     toId :: Int,
     amount :: Int
}

createTransaction :: MonadUnliftIO m =>
  CreateTransaction ->
  SqlPersistT m (P.Key Transaction, Time.UTCTime)
createTransaction CreateTransaction {..} = do
  now <- liftIO Time.getCurrentTime
  rowOrderId <-
    insert $
      Transaction
        now
        userId
        fromId
        toIdType
        toId
        amount
  pure (rowOrderId, now)

loadAllTransaction :: MonadUnliftIO m =>
  Int ->
  SqlPersistT m  [P.Entity Transaction]
loadAllTransaction  userId = 
   select $ from $ \transaction -> do
    where_ (transaction ^. TransactionUserId ==. val userId)
    pure transaction
