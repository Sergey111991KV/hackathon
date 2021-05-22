{-# LANGUAGE TemplateHaskell #-}

module Model.Achievements where

import qualified Data.Aeson as J
import GHC.Generics (Generic)

import Database.Persist.TH
 
data Achievements = Travel | CityActivities | Connoisseur | DozenTickets | OtherAchievements
    deriving (Show, Eq, Generic, Read)

instance J.ToJSON Achievements

instance J.FromJSON Achievements

derivePersistField "Achievements"
