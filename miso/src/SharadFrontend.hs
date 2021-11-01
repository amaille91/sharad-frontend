{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SharadFrontend (runSharadFrontend) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Function ((&))
import Control.Arrow
import Data.List (intercalate)
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

data Model = Model { notes :: [Identifiable NoteContent], editionState :: EditionState, editionModalState :: Modal.State, errorStr :: Maybe String } deriving(Eq, Show)

data EditionState = NotEditing
                  | EditingNewNote NoteContent
                  | EditingExistingNote (Identifiable NoteContent) NoteContent
                  | EditingNewChecklist (ChecklistContent, Maybe Int)
                  | EditingExistingChecklist (Identifiable ChecklistContent) (ChecklistContent, Maybe Int)
                  deriving (Eq, Show)

updateEditedNoteTitle :: EditionState -> String -> EditionState
updateEditedNoteTitle NotEditing _                                              = NotEditing
updateEditedNoteTitle (EditingNewNote editedContent) newTitle                   = EditingNewNote editedContent { title = stringToMaybe newTitle }
updateEditedNoteTitle (EditingExistingNote originalNote editedContent) newTitle = EditingExistingNote originalNote editedContent { title = stringToMaybe newTitle }

updateEditedNoteBody :: EditionState -> String -> EditionState
updateEditedNoteBody (EditingNewNote editedContent) newContent                   = EditingNewNote editedContent { noteContent = newContent }
updateEditedNoteBody (EditingExistingNote originalNote editedContent) newContent = EditingExistingNote originalNote editedContent { noteContent = newContent }
updateEditedNoteBody a _                                                         = a

editedNoteTitle :: EditionState -> Maybe String
editedNoteTitle (EditingNewNote NoteContent { title = editedTitle, noteContent = _})        = editedTitle
editedNoteTitle (EditingExistingNote _ NoteContent { title = editedTitle, noteContent = _}) = editedTitle
editedNoteTitle _ = Nothing

editedNoteBody :: EditionState -> Maybe String
editedNoteBody (EditingNewNote NoteContent { title = _, noteContent = editedContent})        = Just editedContent
editedNoteBody (EditingExistingNote _ NoteContent { title = _, noteContent = editedContent}) = Just editedContent
editedNoteBody _ = Nothing

updateEditedChecklistTitle :: EditionState -> String -> EditionState
updateEditedChecklistTitle NotEditing _                                              = NotEditing
updateEditedChecklistTitle (EditingNewChecklist (editedContent, maybeEditingIdx)) newTitle                   = EditingNewChecklist (editedContent { name = newTitle }, maybeEditingIdx)
updateEditedChecklistTitle (EditingExistingChecklist originalChecklist (editedContent, maybeEditingIdx)) newTitle = EditingExistingChecklist originalChecklist (editedContent { name = newTitle }, maybeEditingIdx)

data AppEvent =  NoteModalEvent Modal.Event
               | SharadEvent SharadEventInstance
          
data SharadEventInstance = CheckForNotes
                         | UpdateNotes [Identifiable NoteContent]
                         | UpdateCurrentlyEditedNoteTitle String
                         | UpdateCurrentlyEditedNoteBody String
                         | CreateNewNoteFromEditedNote
                         | CreateNoteClicked
                         | NoteCreated (Identifiable NoteContent)
                         | DeleteNoteClicked String
                         | NoteDeleted String
                         | CreateChecklistClicked
                         | UpdateCurrentlyEditedChecklistTitle String
                         | ErrorHappened String
                         | NoteEditionFinidhed EditionState
                         | EditionAborted
                         | EditNoteClicked (Identifiable NoteContent)
                         | NoteChanged (Identifiable NoteContent)

initialModel :: Model
initialModel = Model { notes = [], editionState = NotEditing, editionModalState = Modal.Hidden, errorStr = Nothing }

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp (NoteModalEvent event) model = Bifunctor.second (updateModalState model) $ updateModal event (editionModalState model)
updateApp (SharadEvent event)    model = updateSharad event model

updateModal :: Modal.Event -> Modal.State -> Effect AppEvent Modal.State
updateModal event state = Bifunctor.first fromModalEvent $ Modal.update event state

updateSharad :: SharadEventInstance -> Model -> Effect AppEvent Model
updateSharad EditionAborted model                             = noEff model { editionState = NotEditing, editionModalState = Modal.Hidden }
updateSharad (NoteEditionFinidhed finalEditionState) model        = Bifunctor.first fromSharadEvent $ handleNoteEditionFinished finalEditionState model
updateSharad (NoteChanged newNote) model                   = noEff model { notes = changeNote (notes model) newNote }
updateSharad CheckForNotes model                                  = Bifunctor.first fromSharadEvent $ model <# handleCheckForNotes
updateSharad (EditNoteClicked note) model                         = Modal.update Modal.ShowingTriggered (editionModalState model) & Bifunctor.bimap fromModalEvent (\m -> model { editionState = EditingExistingNote note (content note), editionModalState = m })
updateSharad (UpdateNotes notes) model                            = noEff model { notes = notes }
updateSharad (UpdateCurrentlyEditedNoteTitle newTitle) model      = noEff $ model { editionState = updateEditedNoteTitle (editionState model) newTitle }
updateSharad (UpdateCurrentlyEditedNoteBody newBody) model        = noEff $ model { editionState = updateEditedNoteBody (editionState model) newBody }
updateSharad CreateNoteClicked model                              = (editionModalState ^>> (Modal.update Modal.ShowingTriggered) >>^ Bifunctor.bimap fromModalEvent (toEditingNewNote . (updateModalState model))) model
updateSharad (NoteCreated note) model                             = noEff $ model { notes = notes model ++ [note], editionState = NotEditing }
updateSharad (NoteDeleted noteId) model                           = noEff $ model { notes = filter ((/= noteId) . id . storageId) $ notes model }
updateSharad  CreateChecklistClicked model                        = (editionModalState ^>> (Modal.update Modal.ShowingTriggered) >>^ Bifunctor.bimap fromModalEvent (toEditingNewChecklist . (updateModalState model))) model
updateSharad (UpdateCurrentlyEditedChecklistTitle newTitle) model = noEff $ model { editionState = updateEditedChecklistTitle (editionState model) newTitle }
updateSharad (ErrorHappened newErrorStr) model                    = noEff model { errorStr = Just newErrorStr }
updateSharad (DeleteNoteClicked noteId) model                     = model <# do
  response <- xhrByteString $ deleteNoteRequest noteId
  if status response /= 200
    then return $ SharadEvent (ErrorHappened "Server answer != 200 OK")
    else return $ SharadEvent (NoteDeleted noteId)

modifyState :: (Monad m) => (s -> s) -> StateT s m a -> StateT s m a
modifyState f = mapStateT (\initialAction -> initialAction >>= (\(a, s) -> return (a, f s)))

changeNote :: [Identifiable NoteContent] -> Identifiable NoteContent -> [Identifiable NoteContent]
changeNote (n:otherNotes) newNote = 
  if (id .storageId) n == (id .storageId) newNote 
    then newNote : otherNotes
    else n       : changeNote otherNotes newNote

toEditingNewNote :: Model -> Model
toEditingNewNote model = model { editionState = EditingNewNote emptyNoteContent }

toEditingNewChecklist :: Model -> Model
toEditingNewChecklist model = model { editionState = EditingNewChecklist emptyChecklistContent }

updateModalState :: Model -> Modal.State -> Model
updateModalState model newModalState = model { editionModalState = newModalState }

fromModalEvent :: Modal.Event -> AppEvent
fromModalEvent = NoteModalEvent

fromSharadEvent :: SharadEventInstance -> AppEvent
fromSharadEvent = SharadEvent

handleNoteEditionFinished :: EditionState -> Model -> Effect SharadEventInstance Model
handleNoteEditionFinished NotEditing model = model <# return (ErrorHappened "Couldn't be possible to receive NoteEditionFinished event while not editing a note")
handleNoteEditionFinished (EditingNewNote newNoteContent) model = model { editionState = NotEditing, editionModalState = Modal.Hidden } <# callPostNote newNoteContent
handleNoteEditionFinished (EditingExistingNote originalNote newNoteContent) model = model { editionState = NotEditing, editionModalState = Modal.Hidden } <# do
  newStorageId <- callAndRetrieveBody $ putNoteRequest Identifiable { storageId = storageId originalNote, content = newNoteContent }
  case newStorageId of
    Just newStorageId -> return $ NoteChanged Identifiable { storageId = newStorageId, content = newNoteContent }
    Nothing           -> return (ErrorHappened "No Body in POST /note response")
handleNoteEditionFinished _ model = noEff model { editionState = NotEditing, editionModalState = Modal.Hidden, errorStr = Just "handling note edition while not editing note" }


handleCheckForNotes :: IO SharadEventInstance
handleCheckForNotes = do
  retrievedNotes <- callAndRetrieveBody getNotesRequest
  return $ fmap UpdateNotes retrievedNotes `orElse` (ErrorHappened "No Body in GET /note response")

callPostNote :: NoteContent -> IO SharadEventInstance
callPostNote newContent = do
  maybeStoreId <- callAndRetrieveBody (postNoteRequest newContent)
  return $ fmap (\storeId -> NoteCreated Identifiable { storageId = storeId, content = newContent }) maybeStoreId `orElse` (ErrorHappened "No Body in POST /note response")

notePath :: String
notePath = "/note"

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

putNoteRequest :: Identifiable NoteContent -> Request
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
    (modalEditView model ++ listViewFromMaybe errorView (errorStr model) ++ [ hr_ []
                                                                                    , navigationMenuView (errorStr model)
                                                                                    , ul_ [ class_ "list-group" ] $ map noteView (notes model) 
                                                                                    , hr_ []
                                                                                    ])

navigationMenuView :: Maybe String -> View AppEvent
navigationMenuView errorStr =
  nav_ [ class_ "container py-2" ] 
    [ div_ [ class_ "row justify-content-end" ] [ openChecklistCreationModalButton, openNoteCreationModalButton ] ]

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
  button_ [ class_ "btn btn-primary", onClick (SharadEvent CreateNoteClicked) ] [ text "Create note" ]

openChecklistCreationModalButton :: View AppEvent
openChecklistCreationModalButton  = 
  button_ [ class_ "btn btn-primary", onClick (SharadEvent CreateChecklistClicked) ] [ text "Create checklist" ]

noteView :: Identifiable NoteContent -> View AppEvent
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
    _noteContent = noteContent (content note)
    _noteTitle = (ms . fromMaybe "" . title . content) note

noteContentView :: String -> [View a]
noteContentView noteContentStr = map (\t -> p_ [] [ text $ ms t ]) (lines noteContentStr)

modalEditView :: Model -> [View AppEvent]
modalEditView model = case editionState model of
    NotEditing                               -> []
    EditingNewNote editedContent             -> modalNoteEditView (title editedContent) (noteContent editedContent) (editionState model) (editionModalState model)
    EditingExistingNote _ editedContent      -> modalNoteEditView (title editedContent) (noteContent editedContent) (editionState model) (editionModalState model)
    EditingNewChecklist (editedContent, maybeEditingIndex) -> modalChecklistEditView (name editedContent) (makeListOfEditingItems maybeEditingIndex $ items editedContent) (editionState model) (editionModalState model)
    EditingExistingChecklist _ (editedContent, maybeEditingIndex) -> modalChecklistEditView (name editedContent) (makeListOfEditingItems maybeEditingIndex $ items editedContent) (editionState model) (editionModalState model)

makeListOfEditingItems :: Maybe Int -> [ChecklistItem] -> [(ChecklistItem, Bool)]
makeListOfEditingItems Nothing           items = map (\item -> (item, False)) items
makeListOfEditingItems (Just editingIdx) items = mapWithIndex (\currentIdx currentItem -> (currentItem, currentIdx ==  editingIdx)) items

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f l = fst fold_
    where
        fold_ = foldr accumulatingFunction ([], length l - 1) l 
        accumulatingFunction currentItem (accumulatingList, currentIdx) = (newAccumulatingList, currentIdx - 1)
            where newAccumulatingList = f currentIdx currentItem : accumulatingList

toEditingChecklistItem :: Int -> Int -> ChecklistItem -> (ChecklistItem, Bool)
toEditingChecklistItem editingItemIdx currentIdx currentItem
    | editingItemIdx == currentIdx = (currentItem, True)
    | otherwise                    = (currentItem, False)

modalNoteEditView :: Maybe String -> String -> EditionState -> Modal.State -> [View AppEvent]
modalNoteEditView title content editionState modalEditionState =
  Modal.view "Création d'une note"
            [ form_ [ class_ "row justify-content-center" ] 
              [ noteTitleInputView $ ms $ fromMaybe "" title
              , noteContentInputView $ ms $ content
              ]
            ]
            [ button_ [ class_ "btn btn-secondary", onClick (SharadEvent EditionAborted) ] [ text "Cancel" ]
            , button_ [ class_ "btn btn-primary", onClick (SharadEvent (NoteEditionFinidhed editionState)) ] [ text "Submit" ]
            ]
            modalEditionState

modalChecklistEditView :: String -> [(ChecklistItem, Bool)] -> EditionState -> Modal.State -> [View AppEvent]
modalChecklistEditView name items editionState modalEditionState =
  Modal.view "Création d'une checklist"
            [ form_ [ class_ "row justify-content-center" ] 
              [ checklistTitleInputView  $ ms $ name 
              , checklistContentInputView  items 
              ]
            ]
            [ button_ [ class_ "btn btn-secondary", onClick (SharadEvent EditionAborted) ] [ text "Cancel" ]
            , button_ [ class_ "btn btn-primary", onClick (SharadEvent (NoteEditionFinidhed editionState)) ] [ text "Submit checklist" ]
            ]
            modalEditionState

noteTitleInputView :: MisoString -> View AppEvent
noteTitleInputView titleValue =
  div_ [ class_ "form-group mb-3"]
    [ label_ [ textProp "htmlFor" "inputTitle", class_ "form-label" ] [ text "Titre" ]
    , input_ [ type_ "text", id_ "inputTitle", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedNoteTitle . fromMisoString), value_ titleValue ]
    ]

noteContentInputView :: MisoString -> View AppEvent
noteContentInputView contentValue =
  div_ [ class_ "form-group mb-3"]
    [ label_ [ textProp "htmlFor" "inputContent", class_ "form-label" ] [ text "Contenu" ]
    , textarea_ [ id_ "inputContent", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedNoteBody . fromMisoString), value_ contentValue ] []
    ]

checklistTitleInputView :: MisoString -> View AppEvent
checklistTitleInputView name  =
  div_ [ class_ "form-group mb-3" ]
    [ label_ [ textProp "htmlFor" "inputTitle", class_ "form-label" ] [ text "Nom de la liste" ]
    , input_ [ type_ "text", id_ "inputTitle", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedChecklistTitle . fromMisoString), value_ name ]
    ]

checklistContentInputView :: [(ChecklistItem, Bool)] -> View AppEvent
checklistContentInputView items =
  div_ [ class_ "form-group mb-3" ]
    ([ label_ [ textProp "htmlFor" "inputContent", class_ "form-label" ] [ text "Items" ] ] ++
    (map checklistItemView items) ++
    [ button_ [ class_ "btn btn-light" ] [ text "add new item" ] ])

checklistItemView :: (ChecklistItem, Bool) -> View AppEvent
checklistItemView (item, isEditing) =
  div_ [] [ if isEditing then text $ ms (label item) else input_ [ type_ "text", id_ "checklist-item-input", class_ "form-control" ]] 

emptyNoteContent :: NoteContent
emptyNoteContent = NoteContent { title = Nothing, noteContent = "" }

emptyChecklistContent :: (ChecklistContent, Maybe Int)
emptyChecklistContent = (ChecklistContent { name = "", items = [] }, Nothing)

stringToMaybe :: String -> Maybe String
stringToMaybe s = if s == "" then Nothing else Just s
