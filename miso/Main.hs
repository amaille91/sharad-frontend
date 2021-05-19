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
import Miso.Html (h1_, text, p_, div_, ul_, li_, button_, i_, hr_, form_, legend_, label_, input_, textarea_)
import Miso.Html.Event (onClick, onSubmit, onChange)
import Miso.Html.Property (id_, class_, type_, value_, textProp)
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
              , mountPoint = Just "App"
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
  div_ [ class_ "container" ]
    [ ul_ [ class_ "list-group" ] $ map noteView model ++ [ hr_ [], noteEditView ] ]

noteView :: Note -> View AppEvent
noteView note = 
  li_ [ class_ "list-group-item row" ]
    [ h1_ [ class_ "h4" ] [ text _noteTitle ]
    , p_ [] [ text _noteContent ]
    , button_ [ onClick NoOp, class_ "btn btn-sm btn-outline-danger" ] [ i_ [ class_ "bi bi-trash" ] [] ]
    , button_ [ onClick NoOp, class_ "btn btn-sm btn-outline-info ml-2"] [ i_ [ class_ "bi bi-pen" ] [] ]
    ]
  where
    _noteContent = (ms . content . noteContent) note
    _noteTitle = (ms . fromMaybe "ERROR" . title . noteContent) note

noteEditView :: View AppEvent
noteEditView =
  div_ [ class_ "row justify-content-center", onClick NoOp ]
    [ form_ [ onSubmit NoOp ] 
        [ legend_ [] [ text "Création d'une note :" ]
        , titleInputView
        , contentInputView
        , button_ [ type_ "submit", class_ "btn btn-primary" ] [ text "Submit" ]]
    ]

titleInputView :: View AppEvent
titleInputView =
  div_ [ class_ "mb-3"]
    [ label_ [ textProp "htmlFor" "inputTitle", class_ "form-label" ] [ text "Titre" ]
    , input_ [ type_ "text", id_ "inputTitle", class_ "form-control", onChange (const NoOp), value_ ""]
    ]

contentInputView :: View AppEvent
contentInputView =
  div_ [ class_ "mb-3"]
    [ label_ [ textProp "htmlFor" "inputContent", class_ "form-label" ] [ text "Contenu" ]
    , textarea_ [ id_ "inputContent", class_ "form-control", onChange (const NoOp), value_ ""] []
    ]

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
