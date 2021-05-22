module Model.UserEvents where

import qualified Data.Aeson as J
import qualified Data.Time as Time
import GHC.Generics (Generic)

data UserEvents = UserEvents
  { reatedAt :: Time.UTCTime,
    idEvents :: Int,
    userId :: Int,
    endDate :: Time.Day
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON UserEvents

instance J.FromJSON UserEvents