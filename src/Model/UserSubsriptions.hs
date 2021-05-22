module Model.UserSubsriptions where

import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Model.Achievements
import Model.City
import Model.Interests
import Model.UserSerializer
import qualified Data.Aeson as J



data UserSubsriptions = UserSubsriptions {
    reatedAt ::  Time.UTCTime,
     idEvents ::  Int ,
     userId :: Int
}
  deriving (Show, Eq, Generic)

instance J.ToJSON UserSubsriptions



instance J.FromJSON UserSubsriptions

   