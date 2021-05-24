{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SharadFrontend (runSharadFrontend) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, fromJust)
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.UTF8 (toString)
import Data.Map.Strict (singleton)

import Miso (consoleLog, startApp, defaultEvents, stringify, App(..), View, getElementById, addEventListener)
import Miso.Types (LogLevel(Off))
import Miso.String (ms, fromMisoString, MisoString)
import Miso.Html (Attribute, h1_, text, p_, main_, span_, div_, nav_, ul_, li_, button_, i_, hr_, form_, legend_, label_, input_, textarea_)
import Miso.Html.Event (onClick, onSubmit, onChange)
import Miso.Html.Property (id_, class_, type_, value_, textProp, intProp)
import Miso.Effect (Effect, noEff, (<#))
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))

import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.Aeson as Aeson (decode)

import qualified SharadFrontend.Modal as Modal (Event(..), FromModalEvent, fromModalEvent, view)

runSharadFrontend :: IO ()
runSharadFrontend = startApp misoApp

misoApp :: App Model AppEvent
misoApp = App { model = initialModel 
              , update = updateApp
              , view = appView
              , subs = []
              , events = defaultEvents <> (singleton "transitionend" False)
              , initialAction = ModalShown
              , mountPoint = Nothing
              , logLevel = Off
              }

data Model = Model { notes :: [Note], noteEditionState :: NoteEditionState, errorStr :: Maybe String } deriving(Eq, Show)

data NoteEditionState = NotEditing | EditingNewNote NoteContent | EditingExistingNote Note NoteContent deriving (Eq, Show)

updateEditedNoteTitle :: NoteEditionState -> String -> NoteEditionState
updateEditedNoteTitle NotEditing _                                              = NotEditing
updateEditedNoteTitle (EditingNewNote editedContent) newTitle                   = EditingNewNote editedContent { title = stringToMaybe newTitle }
updateEditedNoteTitle (EditingExistingNote originalNote editedContent) newTitle = EditingExistingNote originalNote editedContent { title = stringToMaybe  newTitle }

updateEditedNoteBody :: NoteEditionState -> String -> NoteEditionState
updateEditedNoteBody NotEditing _                                                = NotEditing
updateEditedNoteBody (EditingNewNote editedContent) newContent                   = EditingNewNote editedContent { content = newContent }
updateEditedNoteBody (EditingExistingNote originalNote editedContent) newContent = EditingExistingNote originalNote editedContent { content = newContent }

editedNoteTitle :: NoteEditionState -> Maybe String
editedNoteTitle NotEditing = Nothing
editedNoteTitle (EditingNewNote NoteContent { title = editedTitle, content = _})        = editedTitle
editedNoteTitle (EditingExistingNote _ NoteContent { title = editedTitle, content = _}) = editedTitle

editedNoteBody :: NoteEditionState -> Maybe String
editedNoteBody NotEditing = Nothing
editedNoteBody (EditingNewNote NoteContent { title = _, content = editedContent})       = Just editedContent
editedNoteBody (EditingExistingNote _ NoteContent { title = _, content = editedContent}) = Just editedContent

data AppEvent = 
    NoOp
  | CheckForNotes
  | UpdateNotes [Note]
  | UpdateCurrentlyEditedNoteTitle String
  | UpdateCurrentlyEditedNoteBody String
  | CreateNewNoteFromEditedNote
  | CreateNoteClicked
  | NoteCreated Note
  | DeleteNoteClicked String
  | NoteDeleted String
  | ErrorHappened String
  | NoteEditionFinidhed NoteEditionState
  | EditNoteClicked Note
  | ModalShown

initialModel :: Model
initialModel = Model { notes = [], noteEditionState = NotEditing, errorStr = Nothing }

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp ModalShown model = model <# (consoleLog "Modal shown from event transitionend" >> return NoOp)
updateApp CheckForNotes model                                  = model <# handleCheckForNotes
updateApp (EditNoteClicked note) model                         = noEff $ model { noteEditionState = EditingExistingNote note (noteContent note) }
updateApp NoOp model                                           = noEff model
updateApp (UpdateNotes notes) model                            = noEff model { notes = notes }
updateApp (UpdateCurrentlyEditedNoteTitle newTitle) model      = noEff $ model { noteEditionState = updateEditedNoteTitle (noteEditionState model) newTitle }
updateApp (UpdateCurrentlyEditedNoteBody newBody) model        = noEff $ model { noteEditionState = updateEditedNoteBody (noteEditionState model) newBody }
updateApp (NoteEditionFinidhed finalEditionState) model        = handleNoteEditionFinished finalEditionState model
updateApp CreateNoteClicked model                              = noEff $ model { noteEditionState = EditingNewNote emptyNoteContent }
updateApp (NoteCreated note) model                             = noEff $ model { notes = notes model ++ [note], noteEditionState = NotEditing }
updateApp (NoteDeleted noteId) model                           = noEff $ model { notes = filter ((/= noteId) . id . storageId) $ notes model }
updateApp (ErrorHappened newErrorStr) model                    = noEff model { errorStr = Just newErrorStr }
updateApp (DeleteNoteClicked noteId) model                     = model <# do
  response <- xhrByteString $ deleteNoteRequest noteId
  if status response /= 200
    then return (ErrorHappened "Server answer != 200 OK")
    else return (NoteDeleted noteId)

handleNoteEditionFinished :: NoteEditionState -> Model -> Effect AppEvent Model
handleNoteEditionFinished NotEditing model = model <# return (ErrorHappened "Couldn't be possible to receive NoteEditionFinished event while not editing a note")
handleNoteEditionFinished (EditingNewNote newNoteContent) model = model { noteEditionState = NotEditing } <# callPostNote newNoteContent
handleNoteEditionFinished (EditingExistingNote originalNote newNoteContent) model = model { noteEditionState = NotEditing } <# do
  return (ErrorHappened "NotImplemented")

handleCheckForNotes :: IO AppEvent
handleCheckForNotes = do
  retrievedNotes <- callAndRetrieveBody getNotesRequest
  return $ fmap UpdateNotes retrievedNotes `orElse` (ErrorHappened "No Body in GET /note response")

callPostNote :: NoteContent -> IO AppEvent
callPostNote newContent = do
  maybeStoreId <- callAndRetrieveBody (postNoteRequest newContent)
  return $ fmap (\storeId -> NoteCreated Note { storageId = storeId, noteContent = newContent }) maybeStoreId `orElse` (ErrorHappened "No Body in POST /note response")

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

callAndRetrieveBody :: FromJSON a => Request -> IO (Maybe a)
callAndRetrieveBody req = do 
  maybeBS <- fmap contents $ xhrByteString req
  case maybeBS of
    Nothing -> return Nothing
    Just bs -> return $ decode bs

decode :: FromJSON fromJson => ByteString -> Maybe fromJson
decode = Aeson.decode . fromStrict

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

asRequestBody :: ToJSON requestObj => requestObj -> RequestData
asRequestBody = StringData . ms . toString . toStrict . encode

instance Modal.FromModalEvent AppEvent where
  fromModalEvent Modal.Shown = ModalShown
  fromModalEvent _ = ModalShown

appView :: Model -> View AppEvent
appView model = Modal.view "Arbitrary title" [ (text "Body" :: View AppEvent) ] [ text "Footer" ]
  -- main_ [ id_ "App", class_ "container" ]
  --   ([ modalNoteEditView model ] ++ listViewFromMaybe errorView (errorStr model) ++ [ hr_ []
  --                                                                                   , navigationMenuView (errorStr model)
  --                                                                                   , ul_ [ class_ "list-group" ] $ map noteView (notes model) 
  --                                                                                   , hr_ []
  --                                                                                   ])

navigationMenuView :: Maybe String -> View AppEvent
navigationMenuView errorStr =
  nav_ [ class_ "container py-2" ] 
    [ div_ [ class_ "row justify-content-end" ] [ openNoteCreationModalButton ] ]

listViewFromMaybe :: (a -> View AppEvent) -> Maybe a -> [View AppEvent]
listViewFromMaybe toView maybeA = fmap (\a -> [ toView a ]) maybeA `orElse` []

errorView :: String -> View AppEvent
errorView errorStr =
  div_ [ class_ "container py-2" ]
    [ div_ [ class_ "row justify-content-center" ] 
        [ div_ [ class_ "alert alert-danger mb-0", role_ "role" ] [ text (ms errorStr) ] ]
    ]
  
role_ :: MisoString -> Attribute AppEvent
role_ role = textProp "role" role

openNoteCreationModalButton :: View AppEvent
openNoteCreationModalButton = 
  button_ ([ class_ "btn btn-primary", onClick CreateNoteClicked ] ++ openEditionModalBootstrapProperties) [ text "Create note" ]

openEditionModalBootstrapProperties :: [Attribute AppEvent]
openEditionModalBootstrapProperties =
  [ textProp "data-toggle" "modal", textProp "data-target" "#editing-modal", textProp "data-backdrop" "static", textProp "data-keyboard" "true" ]

noteView :: Note -> View AppEvent
noteView note = 
  li_ [ class_ "list-group-item row" ]
    [ h1_ [ class_ "h4" ] [ text _noteTitle ]
    , p_ [] [ text _noteContent ]
    , button_ [ onClick NoOp, class_ "btn btn-sm btn-outline-danger" ] [ i_ [ class_ "bi bi-trash", onClick $ DeleteNoteClicked $ (id . storageId) note ] [] ]
    , button_ [ onClick NoOp, class_ "btn btn-sm btn-outline-info ml-2"] [ i_ [ class_ "bi bi-pen", onClick (EditNoteClicked note), textProp "data-toggle" "modal", textProp "data-target" "#editing-modal" ] [] ]
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
                  [ titleInputView $ ms $ fromMaybe "" (editedNoteTitle (noteEditionState model))
                  , contentInputView $ ms $ fromMaybe "" (editedNoteBody (noteEditionState model))
                  ]
                ]
            , div_ [ class_ "modal-footer" ]
                [ button_ [ class_ "btn btn-secondary", textProp "data-dismiss" "modal" ] [ text "Cancel" ]
                , button_ [ class_ "btn btn-primary", onClick (NoteEditionFinidhed finalEditionState), textProp "data-dismiss" "modal" ] [ text "Submit" ]
                ]
            ]
        ]
    ]
  where finalEditionState = noteEditionState model

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

emptyNoteContent :: NoteContent
emptyNoteContent = NoteContent { title = Nothing, content = "" }

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

stringToMaybe :: String -> Maybe String
stringToMaybe s = if s == "" then Nothing else Just s
