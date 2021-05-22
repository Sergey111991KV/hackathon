{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module API.Events where

-- import qualified Ext.HTTP.Response as Web
import qualified Database.Tables.Events as DB
import Model.Events
import Servant
    ( type (:>), ReqBody, JSON, Get, Post, type (:<|>), Capture )

type EventsAPI = "events" :> (GetEventsAPI :<|> SendEventsAPI :<|> GetAllEventsAPI)

type GetEventsAPI = "getOne" :> Capture "id" Int :> Get '[JSON] (Maybe Events)

type SendEventsAPI = "save" :> ReqBody '[JSON] DB.EventsCreation :> Post '[JSON] ()

type GetAllEventsAPI = "getAll" :> Post '[JSON] [Events]
