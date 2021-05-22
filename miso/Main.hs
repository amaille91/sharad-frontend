{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, fromJust)
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.UTF8 (toString)

import Miso (consoleLog, startApp, defaultEvents, stringify, App(..), View)
import Miso.Types (LogLevel(Off))
import Miso.String (ms, fromMisoString, MisoString)
import Miso.Html (h1_, text, p_, main_, div_, nav_, ul_, li_, button_, i_, hr_, form_, legend_, label_, input_, textarea_)
import Miso.Html.Event (onClick, onSubmit, onChange)
import Miso.Html.Property (id_, class_, type_, value_, textProp, intProp)
import Miso.Effect (Effect, noEff, (<#))
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))

import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.Aeson as Aeson (decode)

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

data Model = Model { notes :: [Note], editing :: Bool, currentlyEditedNoteTitle :: Maybe String, currentlyEditedNoteBody :: Maybe String, errorStr :: Maybe String } deriving(Eq, Show)
data AppEvent = 
    NoOp
  | CheckForNotes
  | UpdateNotes [Note]
  | UpdateCurrentlyEditedNoteTitle String
  | UpdateCurrentlyEditedNoteBody String
  | CreateNewNoteFromEditedNote
  | CreateNoteClicked
  | NoteCreated Note
  | DeleteNote String
  | NoteDeleted String
  | ErrorHappened MisoString
  | NoteEdited

initialModel :: Model
initialModel = Model { notes = [], editing = False, currentlyEditedNoteTitle = Nothing, currentlyEditedNoteBody = Nothing, errorStr = Nothing }

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp CheckForNotes model                                  = model <# do callAndHandleBody getNotesRequest (return . UpdateNotes)
updateApp NoOp model                                           = noEff model
updateApp (UpdateNotes notes) model                            = noEff model { notes = notes }
updateApp (UpdateCurrentlyEditedNoteTitle newTitle) model      = noEff $ model { currentlyEditedNoteTitle = if "" == newTitle then Nothing else Just newTitle }
updateApp (UpdateCurrentlyEditedNoteBody newNoteContent) model = noEff $ model { currentlyEditedNoteBody = if "" == newNoteContent then Nothing else Just newNoteContent }
updateApp CreateNewNoteFromEditedNote model                    = model <# callPostNote model
updateApp NoteEdited model                                     = model <# callPostNote model
updateApp CreateNoteClicked model                              = noEff $ model { editing = True }
updateApp (NoteCreated note) model                             = noEff $ model { notes = notes model ++ [note], currentlyEditedNoteTitle = Nothing, currentlyEditedNoteBody = Nothing }
updateApp (NoteDeleted noteId) model                           = noEff $ model { notes = filter ((/= noteId) . id . storageId) $ notes model }
updateApp (DeleteNote noteId) model                            = model <# do
  response <- xhrByteString $ deleteNoteRequest noteId
  if status response /= 200
    then return (ErrorHappened "Server answer != 200 OK")
    else return (NoteDeleted noteId)
                                    
callPostNote :: Model -> IO AppEvent
callPostNote model = fmap postNoteFromNoteContent (mkUpdateNoteContent model) `orElse` return (ErrorHappened "Cannot create note with empty body")
  where
    handleCreationResponse :: NoteContent -> StorageId -> IO AppEvent
    handleCreationResponse updatedContent storeId = return $ NoteCreated Note { storageId = storeId, noteContent = updatedContent }
    postNoteFromNoteContent :: NoteContent -> IO AppEvent
    postNoteFromNoteContent contentToPost = callAndHandleBody (postNoteRequest contentToPost) (handleCreationResponse contentToPost)

mkUpdateNoteContent :: Model -> Maybe NoteContent
mkUpdateNoteContent model = fmap (\editedBody -> NoteContent { title = currentlyEditedNoteTitle model, content = editedBody }) (currentlyEditedNoteBody model)

getNotesRequest :: Request
getNotesRequest = Request { reqMethod = GET
                          , reqURI = "/note"
                          , reqLogin = Nothing
                          , reqHeaders = []
                          , reqWithCredentials = False
                          , reqData = NoData
                          }

deleteNoteRequest :: String -> Request
deleteNoteRequest noteId = Request { reqMethod = DELETE
                                   , reqURI = ms $ "/note/" ++ noteId
                                   , reqLogin = Nothing
                                   , reqHeaders = []
                                   , reqWithCredentials = False
                                   , reqData = NoData
                                   }

postNoteRequest :: NoteContent -> Request
postNoteRequest noteContent = Request { reqMethod = POST
                                      , reqURI = "/note"
                                      , reqLogin = Nothing
                                      , reqHeaders = []
                                      , reqWithCredentials = False
                                      , reqData = asRequestBody noteContent
                                      }

callAndHandleBody :: FromJSON a => Request -> (a -> IO AppEvent) -> IO AppEvent
callAndHandleBody req handleResponse = do
  maybeResponseByteString <- fmap contents $ xhrByteString req
  fmap (handleRawResponse handleResponse) maybeResponseByteString `orElse` return (ErrorHappened "No body in response while expecting one")

handleRawResponse :: FromJSON fromJson => (fromJson -> IO AppEvent) -> ByteString -> IO AppEvent
handleRawResponse handleResponse rawResponse = fmap handleResponse (decode rawResponse) `orElse` return (ErrorHappened "Unable to parse response")

decode :: FromJSON fromJson => ByteString -> Maybe fromJson
decode = Aeson.decode . fromStrict

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

asRequestBody :: ToJSON requestObj => requestObj -> RequestData
asRequestBody = StringData . ms . toString . toStrict . encode

appView :: Model -> View AppEvent
appView model =
  main_ [ id_ "App", class_ "container" ]
    [ modalNoteEditView model
    , nav_ [ class_ "d-flex py-2" ] [ button_ [ class_ "btn btn-primary", onClick CreateNoteClicked, textProp "data-toggle" "modal", textProp "data-target" "#editing-modal", textProp "data-backdrop" "static", textProp "data-keyboard" "true" ] [ text "Create note" ] ]
    , ul_ [ class_ "list-group" ] $ map noteView (notes model) 
    , hr_ []
    --, noteEditView model
    ]

noteView :: Note -> View AppEvent
noteView note = 
  li_ [ class_ "list-group-item row" ]
    [ h1_ [ class_ "h4" ] [ text _noteTitle ]
    , p_ [] [ text _noteContent ]
    , button_ [ onClick NoOp, class_ "btn btn-sm btn-outline-danger" ] [ i_ [ class_ "bi bi-trash", onClick $ DeleteNote $ (id . storageId) note ] [] ]
    , button_ [ onClick NoOp, class_ "btn btn-sm btn-outline-info ml-2"] [ i_ [ class_ "bi bi-pen" ] [] ]
    ]
  where
    _noteContent = (ms . content . noteContent) note
    _noteTitle = (ms . fromMaybe "" . title . noteContent) note

modalNoteEditView :: Model -> View AppEvent
modalNoteEditView model =
  div_ [ id_ "editing-modal", class_ "modal", intProp "tabindex" (-1) ]
    [ div_ [ class_ "modal-dialog modal-dialog-centered modal-dialog-scrollable" ]
        [ div_ [ class_ "modal-content" ] 
            [ div_ [ class_ "modal-header justify-content-center" ]
                [ div_ [ class_ "modal-title" ] [ text "Création d'une note" ] ]
            , div_ [ class_ "modal-body container-fluid" ] 
                [ form_ [ class_ "row justify-content-center" ] 
                  [ titleInputView $ ms $ fromMaybe "" (currentlyEditedNoteTitle model)
                  , contentInputView $ ms $ fromMaybe "" (currentlyEditedNoteBody model)
                  ]
                ]
            , div_ [ class_ "modal-footer" ]
                [ button_ [ class_ "btn btn-secondary", textProp "data-dismiss" "modal" ] [ text "Cancel" ]
                , button_ [ class_ "btn btn-primary", onClick NoteEdited, textProp "data-dismiss" "modal" ] [ text "Submit" ]
                ]
            ]
        ]
    ]

titleInputView :: MisoString -> View AppEvent
titleInputView titleValue =
  div_ [ class_ "form-group mb-3"]
    [ label_ [ textProp "htmlFor" "inputTitle", class_ "form-label" ] [ text "Titre" ]
    , input_ [ type_ "text", id_ "inputTitle", class_ "form-control", onChange (UpdateCurrentlyEditedNoteTitle . fromMisoString), value_ titleValue ]
    ]

contentInputView :: MisoString -> View AppEvent
contentInputView contentValue =
  div_ [ class_ "form-group mb-3"]
    [ label_ [ textProp "htmlFor" "inputContent", class_ "form-label" ] [ text "Contenu" ]
    , textarea_ [ id_ "inputContent", class_ "form-control", onChange (UpdateCurrentlyEditedNoteBody . fromMisoString), value_ contentValue ] []
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
