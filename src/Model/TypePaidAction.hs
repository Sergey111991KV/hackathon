{-# LANGUAGE TemplateHaskell #-}

module Model.TypePaidAction where

import qualified Data.Aeson as J

import GHC.Generics (Generic)

import Database.Persist.TH
 
data TypePaidAction = Events | Subscriptions
   deriving (Show, Read, Eq, Generic)


instance J.ToJSON TypePaidAction

instance J.FromJSON TypePaidAction


derivePersistField "TypePaidAction"
