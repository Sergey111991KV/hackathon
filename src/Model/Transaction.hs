module Model.Transaction where

import qualified Data.Aeson as J
-- import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Model.TypePaidAction

data Transaction = Transaction
  { userId :: Int,
    createdAt :: Time.UTCTime,
    fromId :: Int,
    toId :: Int,
    amount :: Int,
    transactionToIdType :: TypePaidAction
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON Transaction

instance J.FromJSON Transaction
