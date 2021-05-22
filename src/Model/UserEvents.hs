module Model.UserEvents where


import qualified Data.Time as Time
import GHC.Generics (Generic)

import qualified Data.Aeson as J



data UserEvents = UserEvents {
    reatedAt ::  Time.UTCTime,
     idEvents ::  Int ,
     userId :: Int,
    endDate ::  Time.UTCTime
}
  deriving (Show, Eq, Generic)

instance J.ToJSON UserEvents



instance J.FromJSON UserEvents