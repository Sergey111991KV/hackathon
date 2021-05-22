module Model.PlaidTokenRequest where

import qualified Data.Aeson as J
import qualified Data.Text as T
import GHC.Generics (Generic)
import Model.TypePaidAction

data PlaidTokenGet = PlaidTokenGet
  { typePay :: TypePaidAction,
    idPayAction :: Int,
    amount :: Int
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON PlaidTokenGet

instance J.FromJSON PlaidTokenGet

data PlaidTokenSend = PlaidTokenSend
  { token :: T.Text,
    userId :: Int
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON PlaidTokenSend

instance J.FromJSON PlaidTokenSend
