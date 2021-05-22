module Model.AllInformation

where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Model.Achievements
import Model.City
import Model.Interests
import Model.UserSerializer
import Model.UserSubsriptions
import Model.UserEvents



data AllInformation = AllInformation {
    user :: UserSerializer,
    transaction :: Maybe [T.Text],
    userEvents :: UserEvents,
    userSubsriptions :: UserSubsriptions
}
  deriving (Show, Eq, Generic)

instance J.ToJSON AllInformation



instance J.FromJSON AllInformation

