{-# LANGUAGE TemplateHaskell #-}

module Model.Interests where

import qualified Data.Aeson as J
-- import qualified Data.Text as T
-- import qualified Data.Time as Time
-- import qualified Ext.Data.Aeson as J
import GHC.Generics (Generic)

import Database.Persist.TH ( derivePersistField )
 
data Interests = Food | Art | Consert | OtherInterest
    deriving (Show, Eq, Generic, Read)

instance J.ToJSON Interests

instance J.FromJSON Interests

derivePersistField "Interests"
