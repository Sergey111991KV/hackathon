{-# LANGUAGE TemplateHaskell #-}

module Model.City where

-- import qualified Data.Aeson as J
-- import qualified Data.Text as T
-- import qualified Data.Time as Time
-- import qualified Ext.Data.Aeson as J
-- import GHC.Generics (Generic)

import Database.Persist.TH
 
data City = Novorossiysk | Other
   deriving (Show, Read, Eq)

derivePersistField "City"
