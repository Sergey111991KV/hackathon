module Model.AllInformation where

import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Model.Transaction
import Model.UserEvents
import Model.UserSerializer
import Model.UserSubsriptions

data AllInformation = AllInformation
  { user :: UserSerializer,
    transaction :: [Transaction],
    userEvents :: [UserEvents],
    userSubsriptions :: [UserSubsriptions]
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON AllInformation

instance J.FromJSON AllInformation
