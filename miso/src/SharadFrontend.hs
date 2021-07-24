{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SharadFrontend (runSharadFrontend) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Function ((&))
import Control.Arrow
import Data.Maybe (fromMaybe, fromJust)
import Data.String (lines)
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.UTF8 (toString)
import Data.Map.Strict (singleton)
import qualified Data.Bifunctor as Bifunctor (first, second, bimap)
import Control.Monad.State.Strict (StateT, mapStateT)

import Miso (consoleLog, startApp, defaultEvents, stringify, App(..), View, getElementById, addEventListener)
import Miso.Types (LogLevel(Off), Transition, toTransition, fromTransition, mapAction)
import Miso.String (ms, fromMisoString, MisoString)
import Miso.Html (Attribute, h1_, text, p_, main_, span_, div_, nav_, ul_, li_, button_, i_, hr_, form_, legend_, label_, input_, textarea_)
import Miso.Html.Event (onClick, onSubmit, onChange)
import Miso.Html.Property (id_, class_, type_, value_, textProp, intProp)
import Miso.Effect (Effect, noEff, (<#))
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))

import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.Aeson as Aeson (decode)

import qualified SharadFrontend.Modal as Modal

import Model

runSharadFrontend :: IO ()
runSharadFrontend = startApp misoApp

misoApp :: App Model AppEvent
misoApp = App { model = initialModel 
              , update = updateApp
              , view = appView
              , subs = []
              , events = defaultEvents
              , initialAction = SharadEvent CheckForNotes
              , mountPoint = Nothing
              , logLevel = Off
              }

data Model = Model { notes :: [Note], noteEditionState :: NoteEditionState, noteEditionModalState :: Modal.State, errorStr :: Maybe String } deriving(Eq, Show)

data NoteEditionState = NotEditing | EditingNewNote NoteContent | EditingExistingNote Note NoteContent deriving (Eq, Show)

updateEditedNoteTitle :: NoteEditionState -> String -> NoteEditionState
updateEditedNoteTitle NotEditing _                                              = NotEditing
updateEditedNoteTitle (EditingNewNote editedContent) newTitle                   = EditingNewNote editedContent { title = stringToMaybe newTitle }
updateEditedNoteTitle (EditingExistingNote originalNote editedContent) newTitle = EditingExistingNote originalNote editedContent { title = stringToMaybe  newTitle }

updateEditedNoteBody :: NoteEditionState -> String -> NoteEditionState
updateEditedNoteBody (EditingNewNote editedContent) newContent                   = EditingNewNote editedContent { content = newContent }
updateEditedNoteBody (EditingExistingNote originalNote editedContent) newContent = EditingExistingNote originalNote editedContent { content = newContent }
updateEditedNoteBody a _                                                         = a

editedNoteTitle :: NoteEditionState -> Maybe String
editedNoteTitle (EditingNewNote NoteContent { title = editedTitle, content = _})        = editedTitle
editedNoteTitle (EditingExistingNote _ NoteContent { title = editedTitle, content = _}) = editedTitle
editedNoteTitle _ = Nothing

editedNoteBody :: NoteEditionState -> Maybe String
editedNoteBody (EditingNewNote NoteContent { title = _, content = editedContent})        = Just editedContent
editedNoteBody (EditingExistingNote _ NoteContent { title = _, content = editedContent}) = Just editedContent
editedNoteBody _ = Nothing

data AppEvent =  NoteModalEvent Modal.Event
               | SharadEvent SharadEventInstance
          
data SharadEventInstance = CheckForNotes
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
                         | NoteEditionAborted
                         | EditNoteClicked Note
                         | NoteChanged Note

initialModel :: Model
initialModel = Model { notes = [], noteEditionState = NotEditing, noteEditionModalState = Modal.Hidden, errorStr = Nothing }

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp (NoteModalEvent event) model = Bifunctor.second (updateModalState model) $ updateModal event (noteEditionModalState model)
updateApp (SharadEvent event)    model = updateSharad event model

updateModal :: Modal.Event -> Modal.State -> Effect AppEvent Modal.State
updateModal event state = Bifunctor.first fromModalEvent $ Modal.update event state

updateSharad :: SharadEventInstance -> Model -> Effect AppEvent Model
updateSharad NoteEditionAborted model                             = noEff model { noteEditionState = NotEditing, noteEditionModalState = Modal.Hidden }
updateSharad (NoteEditionFinidhed finalEditionState) model        = Bifunctor.first fromSharadEvent $ handleNoteEditionFinished finalEditionState model
updateSharad (NoteChanged newNote) model                   = noEff model { notes = changeNote (notes model) newNote }
updateSharad CheckForNotes model                                  = Bifunctor.first fromSharadEvent $ model <# handleCheckForNotes
updateSharad (EditNoteClicked note) model                         = Modal.update Modal.ShowingTriggered (noteEditionModalState model) & Bifunctor.bimap fromModalEvent (\m -> model { noteEditionState = EditingExistingNote note (noteContent note), noteEditionModalState = m })
updateSharad (UpdateNotes notes) model                            = noEff model { notes = notes }
updateSharad (UpdateCurrentlyEditedNoteTitle newTitle) model      = noEff $ model { noteEditionState = updateEditedNoteTitle (noteEditionState model) newTitle }
updateSharad (UpdateCurrentlyEditedNoteBody newBody) model        = noEff $ model { noteEditionState = updateEditedNoteBody (noteEditionState model) newBody }
updateSharad CreateNoteClicked model                              = (noteEditionModalState ^>> (Modal.update Modal.ShowingTriggered) >>^ Bifunctor.bimap fromModalEvent (toEditingNewNote . (updateModalState model))) model
updateSharad (NoteCreated note) model                             = noEff $ model { notes = notes model ++ [note], noteEditionState = NotEditing }
updateSharad (NoteDeleted noteId) model                           = noEff $ model { notes = filter ((/= noteId) . id . storageId) $ notes model }
updateSharad (ErrorHappened newErrorStr) model                    = noEff model { errorStr = Just newErrorStr }
updateSharad (DeleteNoteClicked noteId) model                     = model <# do
  response <- xhrByteString $ deleteNoteRequest noteId
  if status response /= 200
    then return $ SharadEvent (ErrorHappened "Server answer != 200 OK")
    else return $ SharadEvent (NoteDeleted noteId)

modifyState :: (Monad m) => (s -> s) -> StateT s m a -> StateT s m a
modifyState f = mapStateT (\initialAction -> initialAction >>= (\(a, s) -> return (a, f s)))

changeNote :: [Note] -> Note -> [Note]
changeNote (n:otherNotes) newNote = 
  if (id .storageId) n == (id .storageId) newNote 
    then newNote : otherNotes
    else n       : changeNote otherNotes newNote

toEditingNewNote :: Model -> Model
toEditingNewNote model = model { noteEditionState = EditingNewNote emptyNoteContent }

updateModalState :: Model -> Modal.State -> Model
updateModalState model newModalState = model { noteEditionModalState = newModalState }

fromModalEvent :: Modal.Event -> AppEvent
fromModalEvent = NoteModalEvent

fromSharadEvent :: SharadEventInstance -> AppEvent
fromSharadEvent = SharadEvent

handleNoteEditionFinished :: NoteEditionState -> Model -> Effect SharadEventInstance Model
handleNoteEditionFinished NotEditing model = model <# return (ErrorHappened "Couldn't be possible to receive NoteEditionFinished event while not editing a note")
handleNoteEditionFinished (EditingNewNote newNoteContent) model = model { noteEditionState = NotEditing, noteEditionModalState = Modal.Hidden } <# callPostNote newNoteContent
handleNoteEditionFinished (EditingExistingNote originalNote newNoteContent) model = model { noteEditionState = NotEditing, noteEditionModalState = Modal.Hidden } <# do
  newStorageId <- callAndRetrieveBody $ putNoteRequest NoteUpdate { targetId = storageId originalNote, newContent = newNoteContent }
  case newStorageId of
    Just newStorageId -> return $ NoteChanged Note { storageId = newStorageId, noteContent = newNoteContent }
    Nothing           -> return (ErrorHappened "No Body in POST /note response")


handleCheckForNotes :: IO SharadEventInstance
handleCheckForNotes = do
  retrievedNotes <- callAndRetrieveBody getNotesRequest
  return $ fmap UpdateNotes retrievedNotes `orElse` (ErrorHappened "No Body in GET /note response")

callPostNote :: NoteContent -> IO SharadEventInstance
callPostNote newContent = do
  maybeStoreId <- callAndRetrieveBody (postNoteRequest newContent)
  return $ fmap (\storeId -> NoteCreated Note { storageId = storeId, noteContent = newContent }) maybeStoreId `orElse` (ErrorHappened "No Body in POST /note response")

notePath :: String
notePath = "/haskell/note"

getNotesRequest :: Request
getNotesRequest = Request { reqMethod = GET
                          , reqURI = ms notePath
                          , reqLogin = Nothing
                          , reqHeaders = []
                          , reqWithCredentials = False
                          , reqData = NoData
                          }

deleteNoteRequest :: String -> Request
deleteNoteRequest noteId = Request { reqMethod = DELETE
                                   , reqURI = ms $ notePath ++ "/" ++ noteId
                                   , reqLogin = Nothing
                                   , reqHeaders = []
                                   , reqWithCredentials = False
                                   , reqData = NoData
                                   }

postNoteRequest :: NoteContent -> Request
postNoteRequest noteContent = Request { reqMethod = POST
                                      , reqURI = ms notePath
                                      , reqLogin = Nothing
                                      , reqHeaders = []
                                      , reqWithCredentials = False
                                      , reqData = asRequestBody noteContent
                                      }

putNoteRequest :: NoteUpdate -> Request
putNoteRequest noteUpdate = Request { reqMethod = PUT
                                     , reqURI = ms notePath
                                     , reqLogin = Nothing
                                     , reqHeaders = []
                                     , reqWithCredentials = False
                                     , reqData = asRequestBody noteUpdate
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

appView :: Model -> View AppEvent
appView model = 
  main_ [ id_ "App", class_ "container" ]
    (modalNoteEditView model ++ listViewFromMaybe errorView (errorStr model) ++ [ hr_ []
                                                                                    , navigationMenuView (errorStr model)
                                                                                    , ul_ [ class_ "list-group" ] $ map noteView (notes model) 
                                                                                    , hr_ []
                                                                                    ])

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
  button_ ([ class_ "btn btn-primary", onClick (SharadEvent CreateNoteClicked) ]) [ text "Create note" ]

noteView :: Note -> View AppEvent
noteView note = 
  li_ [ class_ "list-group-item row" ]
    (  [ h1_ [ class_ "h4" ] [ text _noteTitle ] ]
    ++ noteContentView _noteContent
    ++ [ div_ [ class_ "text-center" ]
           [ button_ [ onClick (SharadEvent . DeleteNoteClicked $ (id . storageId) note), class_ "btn btn-sm btn-outline-danger" ] [ i_ [ class_ "bi bi-trash" ] [] ]
           , button_ [ onClick (SharadEvent $ EditNoteClicked note), class_ "btn btn-sm btn-outline-info ml-2"] [ i_ [ class_ "bi bi-pen" ] [] ]
           ]
        ]
    )
  where
    _noteContent = content (noteContent note)
    _noteTitle = (ms . fromMaybe "" . title . noteContent) note

noteContentView :: String -> [View a]
noteContentView noteContentStr = map (\t -> p_ [] [ text $ ms t ]) (lines noteContentStr)

modalNoteEditView :: Model -> [View AppEvent]
modalNoteEditView model =
  Modal.view "Création d'une note"
            [ form_ [ class_ "row justify-content-center" ] 
              [ titleInputView $ ms $ fromMaybe "" (editedNoteTitle (noteEditionState model))
              , contentInputView $ ms $ fromMaybe "" (editedNoteBody (noteEditionState model))
              ]
            ]
            [ button_ [ class_ "btn btn-secondary", onClick (SharadEvent NoteEditionAborted) ] [ text "Cancel" ]
            , button_ [ class_ "btn btn-primary", onClick (SharadEvent (NoteEditionFinidhed finalEditionState)) ] [ text "Submit" ]
            ]
            (noteEditionModalState model)
  where finalEditionState = noteEditionState model


titleInputView :: MisoString -> View AppEvent
titleInputView titleValue =
  div_ [ class_ "form-group mb-3"]
    [ label_ [ textProp "htmlFor" "inputTitle", class_ "form-label" ] [ text "Titre" ]
    , input_ [ type_ "text", id_ "inputTitle", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedNoteTitle . fromMisoString), value_ titleValue ]
    ]

contentInputView :: MisoString -> View AppEvent
contentInputView contentValue =
  div_ [ class_ "form-group mb-3"]
    [ label_ [ textProp "htmlFor" "inputContent", class_ "form-label" ] [ text "Contenu" ]
    , textarea_ [ id_ "inputContent", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedNoteBody . fromMisoString), value_ contentValue ] []
    ]

emptyNoteContent :: NoteContent
emptyNoteContent = NoteContent { title = Nothing, content = "" }

stringToMaybe :: String -> Maybe String
stringToMaybe s = if s == "" then Nothing else Just s
