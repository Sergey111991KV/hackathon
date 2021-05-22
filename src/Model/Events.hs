module Model.Events where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)

data Events = Events
  { createdE :: Time.UTCTime,
    nameE :: T.Text,
    typeE :: T.Text,
    urlE :: T.Text,
    dateEventsStartE :: Time.Day,
    dateEventsEndE :: Time.Day,
    prizeFirstTypeE :: Maybe T.Text,
    prizeFirstCategoriesE :: Maybe Int,
    prizeSecondTypeE :: Maybe T.Text,
    prizeSecondCategoriesE :: Maybe Int,
    prizeTrirdTypeE :: Maybe T.Text,
    prizeTrirdCategoriesE :: Maybe Int,
    priceE :: Int
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON Events

instance J.FromJSON Events
