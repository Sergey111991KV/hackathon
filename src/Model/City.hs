{-# LANGUAGE TemplateHaskell #-}

module Model.City where

import qualified Data.Aeson as J
import GHC.Generics (Generic)

import Database.Persist.TH ( derivePersistField )
 
data City = Novorossiysk | Other
  deriving (Show, Eq, Generic, Read)

instance J.ToJSON City

instance J.FromJSON City


derivePersistField "City"
