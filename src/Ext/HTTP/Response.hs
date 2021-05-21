{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ext.HTTP.Response where

import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Data.Aeson
import qualified Data.Aeson as J
import Data.Maybe

import Ext.HTTP.Error (WebApiHttpError (..))
import GHC.Generics (Generic)

data WebApiHttpResponse res
  = FailedWebApiHttpResponse [WebApiHttpError]
  | SuccessfulWebApiHttpResponse res
  deriving (Show, Eq, Generic, Functor)

instance Applicative WebApiHttpResponse where
  pure = SuccessfulWebApiHttpResponse
  (SuccessfulWebApiHttpResponse ab) <*> (SuccessfulWebApiHttpResponse a) = SuccessfulWebApiHttpResponse $ ab a
  (FailedWebApiHttpResponse f) <*> _ = FailedWebApiHttpResponse f
  _ <*> (FailedWebApiHttpResponse f) = FailedWebApiHttpResponse f

instance Monad WebApiHttpResponse where
  (SuccessfulWebApiHttpResponse a) >>= mab = mab a
  (FailedWebApiHttpResponse f) >>= _ = FailedWebApiHttpResponse f

instance MonadError [WebApiHttpError] WebApiHttpResponse where
  throwError e = FailedWebApiHttpResponse e
  catchError (FailedWebApiHttpResponse a) f = f a
  catchError a _ = a



instance J.ToJSON res => J.ToJSON (WebApiHttpResponse res) where
  toJSON (FailedWebApiHttpResponse errors) =
    J.object $ ("success" .= False) : errorsField
    where
      errorsField =
        case errors of
          [] -> []
          [e] -> ["error" .= e]
          es -> ["errors" .= es]
  toJSON (SuccessfulWebApiHttpResponse res) =
    J.object ["success" .= True, "result" .= res]

instance J.FromJSON a => J.FromJSON (WebApiHttpResponse a) where
  parseJSON = J.withObject "WebApiHttpResponse" $ \obj -> do
    isSuccess :: Bool <- obj .: "success"
    if isSuccess
      then SuccessfulWebApiHttpResponse <$> obj .: "result"
      else do
        errors <- (maybeToList <$> obj .: "error") <|> obj .: "errors" <|> pure []
        pure $ FailedWebApiHttpResponse errors

getResult :: WebApiHttpResponse res -> Maybe res
getResult (SuccessfulWebApiHttpResponse res) = Just res
getResult _ = Nothing

isSuccessfulResult :: WebApiHttpResponse res -> Bool
isSuccessfulResult (SuccessfulWebApiHttpResponse _) = True
isSuccessfulResult _ = False

success :: WebApiHttpResponse ()
success = SuccessfulWebApiHttpResponse ()

result :: res -> WebApiHttpResponse res
result = SuccessfulWebApiHttpResponse

failWith :: WebApiHttpError -> WebApiHttpResponse res
failWith = FailedWebApiHttpResponse . (: [])

failWithMany :: [WebApiHttpError] -> WebApiHttpResponse res
failWithMany = FailedWebApiHttpResponse

runT ::
  Monad m =>
  (e -> WebApiHttpResponse res) ->
  ExceptT e m res ->
  m (WebApiHttpResponse res)
runT t = runExceptT >=> either (pure . t) (pure . result)
