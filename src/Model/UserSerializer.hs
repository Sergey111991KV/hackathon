module Model.UserSerializer
  ( UserSerializer (..),
  )
where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time as Time
--  import qualified Ext.Data.Aeson as J
import GHC.Generics (Generic)
import Model.Achievements
import Model.City
import Model.Interests

data UserSerializer = UserSerializer
  { userId :: Int,
    createdAt :: Time.UTCTime,
    phone :: T.Text,
    nativeCity :: City,
    firstName :: T.Text,
    secondName :: T.Text,
    age :: Int,
    bill :: Int,
    bonusBill :: Int,
    interests:: Maybe [Interests],
    achievements :: Maybe [Achievements]
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON UserSerializer

--  toJSON = J.droppedPrefixDecode

instance J.FromJSON UserSerializer

--  toJSON = J.droppedPrefixDecode