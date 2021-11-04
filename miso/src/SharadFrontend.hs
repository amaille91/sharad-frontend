{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SharadFrontend (runSharadFrontend) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Function ((&))
import Control.Applicative ((<|>))
import Control.Arrow
import Data.List (intercalate)
import Data.Vector ((!))
import Data.Maybe (fromMaybe, fromJust)
import Data.String (lines)
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.UTF8 (toString)
import Data.Map.Strict (singleton)
import qualified Data.Bifunctor as Bifunctor (first, second, bimap)
import Control.Monad.State.Strict (StateT, mapStateT)

import Miso (consoleLog, startApp, defaultEvents, stringify, App(..), View, getElementById, addEventListener, onCreated)
import Miso.Types (LogLevel(Off), Transition, toTransition, fromTransition, mapAction)
import Miso.String (ms, fromMisoString, MisoString)
import Miso.Html (Attribute, h1_, h2_, text, p_, main_, span_, div_, nav_, ul_, li_, button_, a_, i_, hr_, form_, legend_, label_, input_, textarea_, on)
import Miso.Html.Event (onClick, onSubmit, onChange)
import Miso.Event.Decoder (Decoder(..), DecodeTarget(..), valueDecoder, keycodeDecoder)
import Miso.Event.Types (KeyCode(..))
import Miso.Html.Property (id_, class_, type_, value_, href_, textProp, intProp)
import Miso.Effect (Effect(..), noEff, (<#), Sub)
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))

import Data.Aeson (Value(..), FromJSON, ToJSON, encode, withObject, withArray, Array(..), (.:))
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
              , initialAction = SharadEvent CheckForContent
              , mountPoint = Nothing
              , logLevel = Off
              }

data Model = Model { allItems :: [Items], currentlyDisplayed :: ItemType, editionState :: EditionState, editionModalState :: Modal.State, errorStr :: Maybe String } deriving(Eq, Show)

data Items = Note (Identifiable NoteContent)
           | Checklist (Identifiable ChecklistContent)
           deriving (Eq, Show)

data ItemType = NoteType | ChecklistType deriving (Eq, Show)

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
          
data SharadEventInstance = CheckForContent
                         | UpdateNotes [Identifiable NoteContent]
                         | UpdateCurrentlyEditedNoteTitle String
                         | UpdateCurrentlyEditedNoteBody String
                         | CreateNewNoteFromEditedNote
                         | CreateNoteClicked
                         | NoteCreated (Identifiable NoteContent)
                         | DeleteNoteClicked String
                         | NoteDeleted String
                         | CreateChecklistClicked
                         | AddNewItemToCurrentlyEditedChecklist
                         | UpdateCurrentlyEditedChecklistTitle String
                         | ChecklistEditItemLabelChanged (Maybe String)
                         | ChecklistEditionFinidhed
                         | ChecklistChanged (Identifiable ChecklistContent)
                         | ChecklistCreated (Identifiable ChecklistContent)
                         | UpdateChecklists [Identifiable ChecklistContent]
                         | ErrorHappened String
                         | NoteEditionFinidhed EditionState
                         | EditionAborted
                         | EditNoteClicked (Identifiable NoteContent)
                         | NoteChanged (Identifiable NoteContent)
                         | ChangeDisplayed ItemType
                         | NoEffect
                         deriving (Show)

initialModel :: Model
initialModel = Model { allItems = [], currentlyDisplayed = NoteType, editionState = NotEditing, editionModalState = Modal.Hidden, errorStr = Nothing }

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp (NoteModalEvent event) model = Bifunctor.second (updateModalState model) $ updateModal event (editionModalState model)
updateApp (SharadEvent event)    model =
    let Effect newModel subs = updateSharad event model
    in Effect newModel ((\sink -> do
        consoleLog $ ms ("receiving event: " ++ show event)) : subs)

updateModal :: Modal.Event -> Modal.State -> Effect AppEvent Modal.State
updateModal event state = Bifunctor.first fromModalEvent $ Modal.update event state

updateSharad :: SharadEventInstance -> Model -> Effect AppEvent Model
updateSharad NoEffect model                             = noEff model 
updateSharad (ChangeDisplayed newDisplayed) model                             = noEff model { currentlyDisplayed = newDisplayed } 
updateSharad EditionAborted model                             = noEff model { editionState = NotEditing, editionModalState = Modal.Hidden }
updateSharad (NoteEditionFinidhed finalEditionState) model        = Bifunctor.first fromSharadEvent $ handleNoteEditionFinished finalEditionState model
updateSharad (NoteChanged newNote) model                   = noEff model { allItems = changeNote (allItems model) newNote }
updateSharad CheckForContent model                                  = Bifunctor.first fromSharadEvent $ Effect model (map toSubscription [handleCheckForNotes, handleCheckForChecklists])
updateSharad (EditNoteClicked note) model                         = Modal.update Modal.ShowingTriggered (editionModalState model) & Bifunctor.bimap fromModalEvent (\m -> model { editionState = EditingExistingNote note (content note), editionModalState = m })
updateSharad (UpdateNotes notes) model                            = noEff model { allItems = filter (not . isNoteItem) (allItems model) ++ map Note notes }
updateSharad (UpdateCurrentlyEditedNoteTitle newTitle) model      = noEff $ model { editionState = updateEditedNoteTitle (editionState model) newTitle }
updateSharad (UpdateCurrentlyEditedNoteBody newBody) model        = noEff $ model { editionState = updateEditedNoteBody (editionState model) newBody }
updateSharad CreateNoteClicked model                              = (editionModalState ^>> (Modal.update Modal.ShowingTriggered) >>^ Bifunctor.bimap fromModalEvent (toEditingNewNote . (updateModalState model))) model
updateSharad (NoteCreated note) model                             = noEff $ model { allItems = allItems model ++ [Note note], editionState = NotEditing }
updateSharad (NoteDeleted noteId) model                           = noEff $ model { allItems = deleteNoteFromId (allItems model) noteId }
updateSharad  CreateChecklistClicked model                        = (editionModalState ^>> (Modal.update Modal.ShowingTriggered) >>^ Bifunctor.bimap fromModalEvent (toEditingNewChecklist . (updateModalState model))) model
updateSharad AddNewItemToCurrentlyEditedChecklist model           = let newModel = createNewChecklistItemAndEdit model in Effect newModel [(\sink -> (consoleLog . ms) $ "Editing checklist model: " ++ show (editionState newModel))]
updateSharad (ChecklistEditItemLabelChanged newLabel) model   = noEff $ maybe model (updateItemLabel model) newLabel 
updateSharad (UpdateCurrentlyEditedChecklistTitle newTitle) model = noEff $ model { editionState = updateEditedChecklistTitle (editionState model) newTitle }
updateSharad ChecklistEditionFinidhed model        = Bifunctor.first fromSharadEvent $ handleChecklistEditionFinished model
updateSharad (ChecklistChanged newChecklist) model                   = noEff model { allItems = changeChecklist (allItems model) newChecklist }
updateSharad (ChecklistCreated checklist) model                             = noEff $ model { allItems = allItems model ++ [Checklist checklist], editionState = NotEditing }
updateSharad (UpdateChecklists checklists) model                            = noEff model { allItems = filter (not . isChecklistItem) (allItems model) ++ map Checklist checklists }
updateSharad (ErrorHappened newErrorStr) model                    = noEff model { errorStr = Just newErrorStr }
updateSharad (DeleteNoteClicked noteId) model                     = model <# do
  response <- xhrByteString $ deleteNoteRequest noteId
  if status response /= 200
    then return $ SharadEvent (ErrorHappened "Server answer != 200 OK")
    else return $ SharadEvent (NoteDeleted noteId)

changeNote :: [Items] -> Identifiable NoteContent -> [Items]
changeNote (item:otherItems) newNote = case item of
    Note n      -> if (id . storageId) n == (id . storageId) newNote 
                   then Note newNote : otherItems
                   else Note n       : changeNote otherItems newNote
    Checklist c -> Checklist c  : changeNote otherItems newNote 

changeChecklist :: [Items] -> Identifiable ChecklistContent -> [Items]
changeChecklist (item:otherItems) newChecklist = case item of
    Checklist c -> if (id . storageId) c == (id . storageId) newChecklist 
                   then Checklist newChecklist : otherItems
                   else Checklist c            : changeChecklist otherItems newChecklist
    Note n      -> Note n                      : changeChecklist otherItems newChecklist 

isNoteItem :: Items -> Bool
isNoteItem (Note _) = True
isNoteItem _        = False

isChecklistItem :: Items -> Bool
isChecklistItem (Checklist _) = True
isChecklistItem _        = False

deleteNoteFromId :: [Items] -> String -> [Items]
deleteNoteFromId [] noteId = [] 
deleteNoteFromId ((Note n):rest) noteId = if (id . storageId) n == noteId then rest else (Note n) : deleteNoteFromId rest noteId
deleteNoteFromId  (i:rest) noteId = i:deleteNoteFromId rest noteId

toEditingNewNote :: Model -> Model
toEditingNewNote model = model { editionState = EditingNewNote emptyNoteContent }

toEditingNewChecklist :: Model -> Model
toEditingNewChecklist model = model { editionState = EditingNewChecklist emptyChecklistContent }

createNewChecklistItemAndEdit :: Model -> Model
createNewChecklistItemAndEdit model = 
    case editionState model of
        EditingNewChecklist (editedContent, maybeEditingIndex) -> model { editionState = EditingNewChecklist (editedContent { items = items editedContent ++ [emptyChecklistItem] }, Just (length (items editedContent))) }
        a                                                      -> model { errorStr = Just ("Cannot add new cheklist item while edition state is " ++ show a) } 

updateItemLabel :: Model -> String -> Model
updateItemLabel model newLabel =
    case editionState model of
        EditingNewChecklist (editedContent, Just idx) -> model { editionState = EditingNewChecklist (newEditedContent editedContent idx newLabel, Nothing) }
        EditingExistingChecklist (oldChecklist) (editedContent, Just idx) -> model { editionState = EditingNewChecklist (newEditedContent editedContent idx newLabel, Nothing) }
        anotherEditionState -> model { errorStr = Just $ "Cannot update item label while on edition state " ++ show (editionState model) }

newEditedContent oldContent itemIdxToModify newLabel = oldContent { items = mapWithIndex (\currentIdx currentItem -> if currentIdx == itemIdxToModify then currentItem { label = newLabel } else currentItem) (items oldContent) }

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

handleChecklistEditionFinished :: Model -> Effect SharadEventInstance Model
handleChecklistEditionFinished model = case editionState model of
    EditingNewChecklist (editedContent, maybeEditingIndex) -> model { editionState = NotEditing, editionModalState = Modal.Hidden } <# callPostChecklist editedContent
    EditingExistingChecklist originalChecklist (editedContent, maybeEditingIndex) -> model { editionState = NotEditing, editionModalState = Modal.Hidden } <# do
          newStorageId <- callAndRetrieveBody $ putChecklistRequest Identifiable { storageId = storageId originalChecklist, content = editedContent }
          case newStorageId of
            Just newStorageId -> return $ ChecklistChanged Identifiable { storageId = newStorageId, content = editedContent }
            Nothing           -> return $ ErrorHappened "No Body in POST /note response"
    editState -> noEff model { errorStr = Just ("Unable to post checklist while editionState is " ++ show editState) }

handleCheckForNotes :: IO SharadEventInstance
handleCheckForNotes = do
  retrievedNotes <- callAndRetrieveBody getNotesRequest
  return $ fmap UpdateNotes retrievedNotes `orElse` (ErrorHappened "No Body in GET /note response")

handleCheckForChecklists :: IO SharadEventInstance
handleCheckForChecklists = do
  retrievedChecklists <- callAndRetrieveBody getChecklistsRequest
  return $ fmap UpdateChecklists retrievedChecklists `orElse` (ErrorHappened "No Body in GET /checklist response")

toSubscription :: IO a -> Sub a
toSubscription io = (\sink -> do
    res <- io
    sink res)

callPostChecklist :: ChecklistContent -> IO SharadEventInstance
callPostChecklist newContent = do
  maybeStoreId <- callAndRetrieveBody (postChecklistRequest newContent)
  return $ fmap (\storeId -> ChecklistCreated Identifiable { storageId = storeId, content = newContent }) maybeStoreId `orElse` (ErrorHappened "No Body in POST /note response")

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

checklistPath :: String
checklistPath = "/checklist"

getChecklistsRequest :: Request
getChecklistsRequest = Request { reqMethod = GET
                               , reqURI = ms checklistPath
                               , reqLogin = Nothing
                               , reqHeaders = []
                               , reqWithCredentials = False
                               , reqData = NoData
                               }

deleteChecklistRequest :: String -> Request
deleteChecklistRequest checklistId = Request { reqMethod = DELETE
                                             , reqURI = ms $ checklistPath ++ "/" ++ checklistId
                                             , reqLogin = Nothing
                                             , reqHeaders = []
                                             , reqWithCredentials = False
                                             , reqData = NoData
                                             }

postChecklistRequest :: ChecklistContent -> Request
postChecklistRequest checklistContent = Request { reqMethod = POST
                                                , reqURI = ms checklistPath
                                                , reqLogin = Nothing
                                                , reqHeaders = []
                                                , reqWithCredentials = False
                                                , reqData = asRequestBody checklistContent
                                                }

putChecklistRequest :: Identifiable ChecklistContent -> Request
putChecklistRequest checklistUpdate = Request { reqMethod = PUT
                                              , reqURI = ms checklistPath
                                              , reqLogin = Nothing
                                              , reqHeaders = []
                                              , reqWithCredentials = False
                                              , reqData = asRequestBody checklistUpdate
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
    (modalEditView model ++ listViewFromMaybe errorView (errorStr model) ++
      [ createItemsButtonView (currentlyDisplayed model),  h1_ [ class_ "row h2 justify-content-center" ] [ text "Sharad" ] ] ++
      [ navigationMenuView (currentlyDisplayed model)
      , ul_ [ class_ "list-group" ] contentView
      ]
    )
  where
      contentView = case currentlyDisplayed model of
          NoteType -> map noteView (getNotes $ allItems model)
          ChecklistType -> map checklistView (getChecklists $ allItems model)
      getNotes = foldr (\item accList -> case item of
                           Note n -> n : accList
                           _      -> accList
                       )
                       []
      getChecklists = foldr (\item accList -> case item of
                           Checklist c -> c : accList
                           _      -> accList
                       )
                       []

navigationMenuView :: ItemType -> View AppEvent
navigationMenuView currDisplayed =
     ul_ [ class_ "row nav nav-tabs nav-fill" ]
         [ li_ [ class_ "nav-item" ]
             [ a_ [ class_ (ms $ "nav-link " ++ activePropertyForNotes), href_ "#", onClick (SharadEvent $ ChangeDisplayed NoteType) ] [ text "Notes" ]]
         , li_ [ class_ "nav-item" ]
             [ a_ [ class_ (ms $ "nav-link " ++ activePropertyForChecklists), href_ "#", onClick (SharadEvent $ ChangeDisplayed ChecklistType) ] [text "Checklists" ]]
         ]
  where
     activePropertyForNotes = if currDisplayed == NoteType then "active" else ""
     activePropertyForChecklists = if currDisplayed == ChecklistType then "active" else ""

createItemsButtonView :: ItemType -> View AppEvent
createItemsButtonView NoteType = div_ [ class_ "row justify-content-end px-3" ] [ openNoteCreationModalButton ]
createItemsButtonView ChecklistType = div_ [ class_ "row justify-content-end px-3" ] [ openChecklistCreationModalButton ]

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
  button_ [ class_ "btn btn-primary col-4", onClick (SharadEvent CreateNoteClicked) ] [ text "Create note" ]
  

openChecklistCreationModalButton :: View AppEvent
openChecklistCreationModalButton  = 
  button_ [ class_ "btn btn-primary col-4", onClick (SharadEvent CreateChecklistClicked) ] [ text "Create checklist" ]

noteView :: Identifiable NoteContent -> View AppEvent
noteView note = 
  li_ [ class_ "row list-group-item" ]
    (  [ h2_ [ class_ "h4" ] [ text _noteTitle ] ]
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

checklistView :: Identifiable ChecklistContent -> View AppEvent
checklistView checklist = 
  li_ [ class_ "row list-group-item" ]
    [ h2_ [ class_ "h4" ] [ text _checklistName ] 
     , checklistContentView _checklistContent]
  where
    _checklistContent = (items . content) checklist
    _checklistName    = (ms . name . content) checklist

checklistContentView :: [ChecklistItem] -> View AppEvent
checklistContentView items = ul_ [] (map (\item -> li_ [ class_ "row" ] [ checklistItemView item ]) items)

checklistItemView :: ChecklistItem -> View AppEvent
checklistItemView item = text $ ms (label item)

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
            [ checklistTitleInputView  $ ms $ name 
            , checklistItemsEditView  items 
            , button_ [ type_ "button", class_ "btn btn-light row", onClick (SharadEvent AddNewItemToCurrentlyEditedChecklist) ] [ text "+" ]
            ]
            [ button_ [ class_ "btn btn-secondary", onClick (SharadEvent EditionAborted) ] [ text "Cancel" ]
            , button_ [ class_ "btn btn-primary", onClick (SharadEvent ChecklistEditionFinidhed) ] [ text "Submit checklist" ]
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
  div_ [ class_ "form-group mb-3 row" ]
    [ label_ [ textProp "htmlFor" "inputTitle", class_ "form-label" ] [ text "Nom de la liste" ]
    , input_ [ type_ "text", id_ "inputTitle", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedChecklistTitle . fromMisoString), value_ name ]
    ]

checklistItemsEditView :: [(ChecklistItem, Bool)] -> View AppEvent
checklistItemsEditView items =
  div_ [ class_ "form-group mb-3" ]
    ([ label_ [ class_ "row form-label" ] [ text "Items" ] ] ++ (map checklistItemEditView items))

checklistItemEditView :: (ChecklistItem, Bool) -> View AppEvent
checklistItemEditView (item, isEditing) =
  div_ [ class_ "row"] [ if not isEditing then text $ ms (label item) else checklistItemInputView item ] 

checklistItemInputView :: ChecklistItem -> View AppEvent
checklistItemInputView item = input_ [ type_ "text", id_ "checklist-item-input", class_ "form-control", onBlur (SharadEvent . ChecklistEditItemLabelChanged . Just . fromMisoString), onKeyUp (SharadEvent . ChecklistEditItemLabelChanged . (fmap fromMisoString)), value_ (ms $ label item)  ]

onBlur :: (MisoString -> action) -> Attribute action
onBlur = on "blur" valueDecoder

onKeyUp = on "keyup" onEnterInputDecoder

onEnterInputDecoder :: Decoder (Maybe MisoString)
onEnterInputDecoder = Decoder { decodeAt = DecodeTargets [["target"], []]
                              , decoder = withArray "event" (\array -> do
                                   value <- withObject "target" (\o -> o .: "value") (array ! 0)
                                   KeyCode keyCode <- decoder keycodeDecoder (array ! 1)
                                   if keyCode == enterKeyCode then return $ Just value else return Nothing)
                               }
                               where enterKeyCode = 13 

emptyNoteContent :: NoteContent
emptyNoteContent = NoteContent { title = Nothing, noteContent = "" }

emptyChecklistContent :: (ChecklistContent, Maybe Int)
emptyChecklistContent = (ChecklistContent { name = "", items = [] }, Nothing)

emptyChecklistItem :: ChecklistItem
emptyChecklistItem = ChecklistItem { label = "", checked = False }

stringToMaybe :: String -> Maybe String
stringToMaybe s = if s == "" then Nothing else Just s
