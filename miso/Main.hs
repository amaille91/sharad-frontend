{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.UTF8 (toString)

import Miso (consoleLog, startApp, defaultEvents, stringify, App(..), View)
import Miso.Types (LogLevel(Off))
import Miso.String (ms)
import Miso.Html (h1_, text, div_, ul_, li_)
import Miso.Html.Property (class_)
import Miso.Effect (Effect, noEff, (<#))
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))

import Data.Aeson (FromJSON, ToJSON, decode)

main :: IO ()
main = startApp misoApp

misoApp :: App Model AppEvent
misoApp = App { model = initialModel 
              , update = updateApp
              , view = appView
              , subs = []
              , events = defaultEvents
              , initialAction = CheckForNotes
              , mountPoint = Nothing
              , logLevel = Off
              }

type Model = [Note]
data AppEvent = 
    NoOp
  | CheckForNotes
  | UpdateNotes [Note]

initialModel :: Model
initialModel = []

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp CheckForNotes model = model <# do
  maybeRequestByteString <- xhrByteString getNotesRequest
  case contents maybeRequestByteString of
    Nothing -> putStrLn "ERROR, No body in response" >> return NoOp
    Just response -> do
      consoleLog (ms $ "Getting response " ++ toString response)
      let
        maybeNotes = decode$ fromStrict response
        in case maybeNotes of
          Nothing -> consoleLog "Unable to parse response" >> return NoOp
          Just notes -> return $ UpdateNotes notes
  where getNotesRequest = Request { reqMethod = GET
                                  , reqURI = "/note"
                                  , reqLogin = Nothing
                                  , reqHeaders = []
                                  , reqWithCredentials = False
                                  , reqData = NoData
                                  }
updateApp NoOp model = noEff model
updateApp (UpdateNotes notes) _ = noEff notes


appView :: Model -> View AppEvent
appView model =
  div_ []
    [ ul_ [ class_ "list-group" ] $ map noteView model ]

noteView :: Note -> View AppEvent
noteView note = 
  li_ [ class_ "list-group-item" ]
    [ h1_ [ class_ "h4" ] [ text _noteTitle ]
    , div_ [] [ text _noteContent ]
    ]
  where
    _noteContent = (ms . content . noteContent) note
    _noteTitle = (ms . fromMaybe "ERROR" . title . noteContent) note

data NoteContent = NoteContent { title :: Maybe String
                               , content :: String
                               } deriving(Show, Generic, Eq)

instance FromJSON NoteContent
instance ToJSON NoteContent

data StorageId = StorageId { id :: String 
                           , version :: String
                           } deriving (Show, Generic, Eq)

instance ToJSON StorageId
instance FromJSON StorageId

data Note = Note { storageId :: StorageId
                 , noteContent :: NoteContent
                 } deriving (Show, Generic, Eq)

instance ToJSON Note
instance FromJSON Note

data NoteUpdate = NoteUpdate { targetId :: StorageId 
                             , newContent :: NoteContent
                             } deriving (Show, Generic)

instance FromJSON NoteUpdate
