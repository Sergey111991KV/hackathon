{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Ext.HTTP.Error where

import Data.Aeson
import qualified Data.Aeson as J
import Data.Maybe (listToMaybe, maybeToList)


import qualified Data.Text as T



data WebApiHttpError = WebApiHttpError
  { waheMessage :: T.Text,
    waheCode :: T.Text,
    waheData :: Maybe J.Value
  }
  deriving (Show, Eq)

instance J.ToJSON WebApiHttpError where
  toJSON WebApiHttpError {..} =
    let dataField = ("data" .=) <$> maybeToList waheData
     in J.object $ ("code" .= waheCode) : ("message" .= waheMessage) : dataField

instance J.FromJSON WebApiHttpError where
  parseJSON = J.withObject "WebApiHttpError" $ \obj -> do
    code <- obj .: "code"
    message <- obj .: "message"
    dataObject <- listToMaybe <$> obj .: "data"
    pure $ WebApiHttpError code message dataObject

mkWebApiHttpError :: T.Text -> T.Text -> WebApiHttpError
mkWebApiHttpError msg code = WebApiHttpError msg code Nothing


