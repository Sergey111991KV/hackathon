module Model.UserSubsriptions where


import qualified Data.Time as Time
import GHC.Generics (Generic)

import qualified Data.Aeson as J



data UserSubsriptions = UserSubsriptions {
    reatedAt ::  Time.UTCTime,
     idEvents ::  Int ,
     userId :: Int
}
  deriving (Show, Eq, Generic)

instance J.ToJSON UserSubsriptions



instance J.FromJSON UserSubsriptions

   