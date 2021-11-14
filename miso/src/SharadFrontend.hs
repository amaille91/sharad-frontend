{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SharadFrontend (runSharadFrontend) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Function ((&))
import Control.Exception (SomeException, catch)
import Control.Applicative ((<|>))
import Control.Arrow
import Data.List (find)
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
import Miso.Html (Attribute, h1_, h2_, text, p_, main_, header_, span_, div_, nav_, ul_, li_, button_, a_, i_, hr_, form_, legend_, label_, input_, textarea_, on, for_)
import Miso.Html.Event (onClick, onDoubleClick, onSubmit, onChange, onChecked)
import Miso.Event.Decoder (Decoder(..), DecodeTarget(..), emptyDecoder, valueDecoder, keycodeDecoder)
import Miso.Event.Types (Checked(..), KeyCode(..))
import Miso.Html.Property (id_, class_, type_, value_, href_, textProp, intProp, checked_)
import Miso.Effect (Effect(..), noEff, (<#), Sub)
import Miso.Effect.DOM (focus, scrollIntoView)
import Miso.Svg.Element (svg_, path_)
import Miso.Svg.Attribute (width_, height_, viewBox_, d_)
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))
import Language.Javascript.JSaddle.Object (jsg1)
import Language.Javascript.JSaddle.Types (JSM)

import Data.Aeson (Value(..), FromJSON, ToJSON, encode, withObject, withArray, Array(..), (.:))
import qualified Data.Aeson as Aeson (decode)

import qualified SharadFrontend.Modal as Modal

import Model

data Model = Model { currentlyDisplayed :: DisplayMode
                   , notesData :: [Identifiable NoteContent]
                   , checklistsData :: [Identifiable ChecklistContent]
                   , editionModalState :: Modal.State
                   , errorStr :: Maybe String
                   } deriving(Eq, Show)

data DisplayMode = NoteType [Identifiable NoteContent] (Maybe NoteEditionState)
                 | ChecklistType [Identifiable ChecklistContent] (Maybe (Identifiable ChecklistContent, ChecklistEditionState)) deriving (Eq, Show)

data NoteEditionState = EditingNewNote NoteContent
                      | EditingExistingNote (Identifiable NoteContent) NoteContent
                      deriving (Eq, Show)

data ChecklistEditionState = EditingTitle
                           | EditingItems ChecklistItemsEditionState
                           deriving (Eq, Show)

data ChecklistItemsEditionState = EditingLabel Int
                                | CheckItemTransitioning Int CheckItemTransition
                           deriving (Eq, Show)

data CheckItemTransition = CheckAppearing
                         | CheckDisappearing
                         | UncheckAppearing
                         | UncheckDisappearing
                         deriving (Eq, Show)

data AppEvent =  AppModalEvent Modal.Event
               | SharadEvent SharadEventInstance
          
data SharadEventInstance = CheckForContent
                         | NotesRetrieved [Identifiable NoteContent]
                         | UpdateCurrentlyEditedNoteTitle String
                         | UpdateCurrentlyEditedNoteBody String
                         | CreateNoteClicked
                         | NoteCreated (Identifiable NoteContent)
                         | DeleteNoteClicked String
                         | NoteDeleted String
                         | CreateChecklistClicked
                         | UpdateCurrentlyEditedChecklistTitle String
                         | ChecklistEditItemLabelChanged String
                         | ChecklistCreated (Identifiable ChecklistContent)
                         | CheckingItem (Identifiable ChecklistContent) Int
                         | UncheckingItem (Identifiable ChecklistContent) Int
                         | RetrivedChecklists [Identifiable ChecklistContent]
                         | DeleteChecklistClicked String
                         | ChecklistDeleted String
                         | ChecklistChanged (Identifiable ChecklistContent)
                         | EditChecklistTitle (Identifiable ChecklistContent)
                         | EditItemLabel Int
                         | AddItemToChecklist (Identifiable ChecklistContent)
                         | ErrorHappened String
                         | NoteEditionFinidhed
                         | NoteEditionAborted
                         | EditNoteClicked (Identifiable NoteContent)
                         | NoteChanged (Identifiable NoteContent)
                         | ChangeDisplayed
                         | ScrollAndFocus String
                         | ErrorDismissed
                         | TransitionCheckboxDisappearingEnd
                         | NoEffect
                         deriving (Show)

misoApp :: App Model AppEvent
misoApp = App { model = initialModel 
              , update = updateApp
              , view = appView
              , subs = []
              , events = defaultEvents <> singleton "animationend" False
              , initialAction = SharadEvent CheckForContent
              , mountPoint = Nothing
              , logLevel = Off
              }

initialModel :: Model
initialModel = Model { currentlyDisplayed = NoteType [] Nothing
                     , notesData = []
                     , checklistsData = []
                     , editionModalState = Modal.Hidden
                     , errorStr = Nothing
                     }

runSharadFrontend :: IO ()
runSharadFrontend = startApp misoApp

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp (AppModalEvent event) model = Bifunctor.second (updateModalState model) $ updateModal event (editionModalState model)
updateApp (SharadEvent event)    model =
    let Effect newModel subs = updateSharad event model
    in Effect newModel ((\sink -> do
        consoleLog $ ms ("receiving event: " ++ show event)) : subs)

updateModal :: Modal.Event -> Modal.State -> Effect AppEvent Modal.State
updateModal event state = Bifunctor.first fromModalEvent $ Modal.update event state

updateSharad :: SharadEventInstance -> Model -> Effect AppEvent Model
updateSharad NoEffect model                             = noEff model 
updateSharad  TransitionCheckboxDisappearingEnd model         = noEff $ switchCheckTransitionMode model 
updateSharad ChangeDisplayed model                             = switchCurrentlyDisplayed model
updateSharad NoteEditionAborted model@Model {currentlyDisplayed = NoteType notes _ } = noEff model { currentlyDisplayed = NoteType notes Nothing, editionModalState = Modal.Hidden }
updateSharad NoteEditionFinidhed model        = Bifunctor.first fromSharadEvent $ handleNoteEditionFinished model
updateSharad (NoteChanged newNote) model                   = noEff $ handleNoteChanged model newNote
updateSharad CheckForContent model                                  = Bifunctor.first fromSharadEvent $ Effect model (map toSubscription [handleCheckForNotes, handleCheckForChecklists])
updateSharad (EditNoteClicked note) model                         = Modal.update Modal.ShowingTriggered (editionModalState model) & Bifunctor.bimap fromModalEvent (\m -> model { currentlyDisplayed = updateToEditingExistingNote note (currentlyDisplayed model), editionModalState = m })
updateSharad (NotesRetrieved notes) model@Model{ currentlyDisplayed = NoteType _ noteEditionStatus }    = noEff model { currentlyDisplayed = NoteType notes noteEditionStatus, notesData = notes }
updateSharad (NotesRetrieved notes) model = noEff model { notesData = notes }
updateSharad (UpdateCurrentlyEditedNoteTitle newTitle) model      = noEff $ updateEditedNoteTitle model newTitle
updateSharad (UpdateCurrentlyEditedNoteBody newBody) model        = noEff $ updateEditedNoteBody model newBody
updateSharad CreateNoteClicked model                              = (editionModalState ^>> (Modal.update Modal.ShowingTriggered) >>^ Bifunctor.bimap fromModalEvent (toEditingNewNote . (updateModalState model))) model
updateSharad (NoteCreated note) model@Model { currentlyDisplayed = NoteType oldNotes _ } = noEff $ model { currentlyDisplayed = NoteType (oldNotes ++ [note]) Nothing }
updateSharad (NoteDeleted noteId) model@Model { currentlyDisplayed = NoteType oldNotes _ } = noEff $ model { currentlyDisplayed = NoteType (deleteIdentifiableFromId oldNotes noteId) Nothing }
updateSharad  CreateChecklistClicked model@Model { currentlyDisplayed = ChecklistType checklists _ } = model { currentlyDisplayed = ChecklistType checklists Nothing} <# (SharadEvent <$> callPostChecklist emptyChecklistContent { name = "New Checklist"})
updateSharad (ChecklistEditItemLabelChanged newLabel) model   = updateItemLabel model newLabel 
updateSharad (UpdateCurrentlyEditedChecklistTitle newTitle) model = handleTitleModificationEnd newTitle model
updateSharad (ChecklistCreated checklist) model@Model { currentlyDisplayed = ChecklistType oldChecklists _ } = noEff $ model { currentlyDisplayed = ChecklistType (oldChecklists ++ [checklist]) Nothing }
updateSharad (RetrivedChecklists checklists) model@Model { currentlyDisplayed = ChecklistType _ _ } = noEff model { currentlyDisplayed = ChecklistType checklists Nothing, checklistsData = checklists }
updateSharad (RetrivedChecklists checklists) model = noEff model { checklistsData = checklists }
updateSharad (ChecklistDeleted checklistId) model@Model { currentlyDisplayed = ChecklistType oldChecklists _ } = noEff $ model { currentlyDisplayed = ChecklistType (deleteIdentifiableFromId oldChecklists  checklistId) Nothing }
updateSharad (ChecklistChanged checklist) model@Model { currentlyDisplayed = ChecklistType oldChecklists maybeEditState } = noEff $ model { currentlyDisplayed = ChecklistType (changeChecklist checklist oldChecklists) (maybeEditState >>= updateChecklistInEditState checklist) }
updateSharad (CheckingItem originalChecklist itemIdx) model@Model { currentlyDisplayed = ChecklistType oldChecklists _ } =
  let newChecklists :: [Identifiable ChecklistContent] = modifyChecklistItemCheck originalChecklist itemIdx True oldChecklists
      Just newChecklist = find ((==  (id . storageId) originalChecklist) . id . storageId) newChecklists
  in
    model { currentlyDisplayed = ChecklistType newChecklists (Just $ (originalChecklist, EditingItems $ CheckItemTransitioning itemIdx UncheckDisappearing))} <# callPutChecklist newChecklist
updateSharad (UncheckingItem originalChecklist itemIdx) model@Model { currentlyDisplayed = ChecklistType oldChecklists _ } =
  let newChecklists = modifyChecklistItemCheck originalChecklist itemIdx False oldChecklists
      Just newChecklist = find ((== (id . storageId) originalChecklist) . id . storageId) newChecklists
  in
    model { currentlyDisplayed = ChecklistType newChecklists (Just (originalChecklist, EditingItems $ CheckItemTransitioning itemIdx CheckDisappearing))} <# callPutChecklist newChecklist
updateSharad (EditChecklistTitle originalChecklist) model@Model{ currentlyDisplayed = ChecklistType oldChecklists _ } = noEff model { currentlyDisplayed = ChecklistType oldChecklists (Just (originalChecklist, EditingTitle)) }
updateSharad (EditItemLabel idx) model@Model{ currentlyDisplayed = ChecklistType oldChecklists (Just (checklistContent, _)) } = noEff model { currentlyDisplayed = ChecklistType oldChecklists (Just (checklistContent, EditingItems $ EditingLabel idx)) }
updateSharad (ErrorHappened newErrorStr) model                    = noEff model { errorStr = Just newErrorStr }
updateSharad (DeleteNoteClicked noteId) model                     = model <# catch (do
  response <- xhrByteString $ deleteNoteRequest noteId
  if status response /= 200
    then return $ SharadEvent (ErrorHappened "Server answer != 200 OK")
    else return $ SharadEvent (NoteDeleted noteId))
  (\(e :: SomeException) -> return $ SharadEvent (ErrorHappened "NetworkError"))
updateSharad (DeleteChecklistClicked checklistId) model = model <# do
  response <- xhrByteString $ deleteChecklistRequest checklistId
  if status response /= 200
    then return $ SharadEvent (ErrorHappened "Server answer != 200 OK")
    else return $ SharadEvent (ChecklistDeleted checklistId)
updateSharad ErrorDismissed model                     = noEff model { errorStr = Nothing }
updateSharad (AddItemToChecklist checklist) model@Model {currentlyDisplayed = ChecklistType oldChecklists Nothing } = newModel <# (return $ SharadEvent $ ScrollAndFocus "checklist-item-input")
  where
    newModel = model { currentlyDisplayed = ChecklistType (newChecklists) (Just (newChecklist, EditingItems $ EditingLabel indexToEdit))}
    newChecklists = addItemToChecklist checklist oldChecklists
    indexToEdit = (length $ items newContent) - 1
    newChecklist = checklist { content = newContent }
    newContent = (content checklist) { items = (items (content checklist)) ++ [emptyChecklistItem] }
updateSharad (ScrollAndFocus elementId) model = model <# ((SharadEvent NoEffect) <$ (focus (ms elementId) >> scrollIntoView (ms elementId)))


-- ================================== Utils for UPDATE ==============================

fromModalEvent :: Modal.Event -> AppEvent
fromModalEvent = AppModalEvent

fromSharadEvent :: SharadEventInstance -> AppEvent
fromSharadEvent = SharadEvent

handleNoteChanged :: Model -> Identifiable NoteContent -> Model
handleNoteChanged model@Model { currentlyDisplayed = NoteType notes Nothing } newNote =
  model { currentlyDisplayed = NoteType (changeNote newNote notes) Nothing }

updateChecklistInEditState :: Identifiable ChecklistContent -> (Identifiable ChecklistContent, ChecklistEditionState) -> Maybe (Identifiable ChecklistContent, ChecklistEditionState)
updateChecklistInEditState checklist (editedChecklist, editionState) = if (id . storageId) checklist == (id . storageId) editedChecklist then Just (checklist, editionState) else Nothing

modifyChecklistItemCheck :: Identifiable ChecklistContent -> Int -> Bool -> [Identifiable ChecklistContent] -> [Identifiable ChecklistContent]
modifyChecklistItemCheck originalChecklist itemIdx isChecked checklists =
  changeChecklist originalChecklist { content = newContent } checklists
  where
    newContent = originalContent { items = newItems }
    originalContent = content originalChecklist
    newItems = mapWithIndex (\idx currentItem -> if idx == itemIdx then currentItem { checked = isChecked } else currentItem) originalItems
    originalItems = items originalContent

updateToEditingExistingNote :: Identifiable NoteContent -> DisplayMode -> DisplayMode
updateToEditingExistingNote editedNote (NoteType notes Nothing) = NoteType notes (Just $ EditingExistingNote editedNote (content editedNote))

updateEditedNoteTitle :: Model -> String -> Model
updateEditedNoteTitle model@(Model { currentlyDisplayed = NoteType notes (Just (EditingNewNote editedContent)) }) newTitle =
  model { currentlyDisplayed = NoteType notes (Just (EditingNewNote editedContent { title = Just newTitle }))}
updateEditedNoteTitle model@Model { currentlyDisplayed = NoteType notes (Just (EditingExistingNote originalNote editedContent)) } newTitle =
  model { currentlyDisplayed = NoteType (changeNote originalNote { content = (content originalNote) { title = Just newTitle }} notes) Nothing }
updateEditedNoteTitle model _                                                         = model { errorStr = Just $ "Unable to update note title while display state is " ++ show (currentlyDisplayed model) }

updateEditedNoteBody :: Model -> String -> Model
updateEditedNoteBody model@(Model { currentlyDisplayed = NoteType notes (Just (EditingNewNote editedContent)) }) newContent =
  model { currentlyDisplayed = NoteType notes (Just (EditingNewNote editedContent { noteContent = newContent }))}
updateEditedNoteBody model@Model { currentlyDisplayed = NoteType notes (Just (EditingExistingNote originalNote editedContent)) } newContent =
  model { currentlyDisplayed = NoteType (changeNote originalNote { content = (content originalNote) { noteContent = newContent }} notes) Nothing }
updateEditedNoteBody model _                                                         = model { errorStr = Just $ "Unable to update note body while display state is " ++ show (currentlyDisplayed model) }

handleTitleModificationEnd :: String -> Model -> Effect AppEvent Model
handleTitleModificationEnd newTitle model@Model { currentlyDisplayed = ChecklistType checklists (Just (originalChecklist, checklistEditionState)) } =
  model { currentlyDisplayed = ChecklistType checklists Nothing } <# callPutChecklist originalChecklist { content = newContent }
  where
    newContent = (content originalChecklist) { name = newTitle }

changeNote :: Identifiable NoteContent -> [Identifiable NoteContent] -> [Identifiable NoteContent]
changeNote newNote (item:otherItems) =
  if (id . storageId) item  == (id . storageId) newNote 
    then newNote : otherItems
    else item    : changeNote newNote otherItems

changeChecklistTitle :: String -> Identifiable ChecklistContent -> [Identifiable ChecklistContent] -> [Identifiable ChecklistContent]
changeChecklistTitle newTitle originalChecklist@(Identifiable { content = oldContent}) checklists =
  changeChecklist (originalChecklist { content = oldContent { name = newTitle }}) checklists

changeChecklist :: Identifiable ChecklistContent -> [Identifiable ChecklistContent] -> [Identifiable ChecklistContent]
changeChecklist newChecklist (item:otherItems) =
  if (id . storageId) item == (id . storageId) newChecklist 
    then newChecklist : otherItems
    else item         : changeChecklist newChecklist otherItems

addItemToChecklist :: Identifiable ChecklistContent -> [Identifiable ChecklistContent] -> [Identifiable ChecklistContent]
addItemToChecklist checklist checklists = changeChecklist (checklist { content = newContent }) checklists
  where
    newContent = (content checklist) { items = newItems }
    newItems = (items $ content checklist) ++ [emptyChecklistItem]

deleteIdentifiableFromId :: Content a => [Identifiable a] -> String -> [Identifiable a]
deleteIdentifiableFromId [] noteId = [] -- TODO propagate error not finding item to delete
deleteIdentifiableFromId (currentItem@Identifiable { storageId = StorageId { id = currentId }}:rest) itemId =
  if currentId == itemId
    then rest
    else currentItem : deleteIdentifiableFromId rest itemId

toEditingNewNote :: Model -> Model
toEditingNewNote model@(Model { currentlyDisplayed = NoteType notes editionSt }) = model { currentlyDisplayed = NoteType notes (Just $ EditingNewNote emptyNoteContent) }
toEditingNewNote model = model { errorStr = Just ("Cannot switch to editing new note while displaying state is " ++ show (currentlyDisplayed model)) }

updateItemLabel :: Model -> String -> Effect AppEvent Model
updateItemLabel model@Model { currentlyDisplayed = ChecklistType checklists (Just (originalChecklist, EditingItems (EditingLabel idx)))} newLabel = model { currentlyDisplayed = ChecklistType checklists Nothing } <# callPutChecklist newChecklist
  where
    newChecklist = originalChecklist { content = newContent }
    newContent = changeItemLabel idx newLabel (content originalChecklist)
updateItemLabel model newLabel = noEff model { errorStr = Just $ "Cannot update item label while on display state is " ++ show (currentlyDisplayed model) }

changeItemLabel :: Int -> String -> ChecklistContent -> ChecklistContent
changeItemLabel idxToModify newLabel clContent@ChecklistContent { items = clItems } =
  clContent { items = mapWithIndexMaybe (\currentIdx currentItem -> if null newLabel then Nothing else if currentIdx == idxToModify then Just currentItem { label = newLabel } else Just currentItem ) clItems }

updateModalState :: Model -> Modal.State -> Model
updateModalState model newModalState = model { editionModalState = newModalState }

handleNoteEditionFinished :: Model -> Effect SharadEventInstance Model
handleNoteEditionFinished model@Model { currentlyDisplayed = NoteType notes (Just (EditingNewNote newNoteContent)) } = model { currentlyDisplayed = NoteType notes Nothing, editionModalState = Modal.Hidden } <# callPostNote newNoteContent
handleNoteEditionFinished model@Model { currentlyDisplayed = NoteType notes (Just (EditingExistingNote originalNote newNoteContent)) } = model { currentlyDisplayed = NoteType notes Nothing, editionModalState = Modal.Hidden } <# do
  newStorageId <- callAndRetrieveBody $ putNoteRequest Identifiable { storageId = storageId originalNote, content = newNoteContent }
  case newStorageId of
    Just newStorageId -> return $ NoteChanged Identifiable { storageId = newStorageId, content = newNoteContent }
    Nothing           -> return (ErrorHappened "No Body in POST /note response")

handleCheckForNotes :: IO SharadEventInstance
handleCheckForNotes = do
  retrievedNotes <- callAndRetrieveBody getNotesRequest
  return $ maybe (ErrorHappened "No Body in GET /note response")
                 NotesRetrieved
                 retrievedNotes

handleCheckForChecklists :: IO SharadEventInstance
handleCheckForChecklists = do
  retrievedChecklists <- callAndRetrieveBody getChecklistsRequest
  return $ maybe (ErrorHappened "No Body in GET /checklist response")
                 RetrivedChecklists
                 retrievedChecklists

switchCheckTransitionMode :: Model -> Model
switchCheckTransitionMode model = case currentlyDisplayed model of
  ChecklistType checklists (Just (originalChecklist, EditingItems (CheckItemTransitioning idx transition))) -> case transition of
    CheckDisappearing   -> model { currentlyDisplayed = ChecklistType checklists $ Just (originalChecklist, EditingItems (CheckItemTransitioning idx UncheckAppearing)) }
    UncheckDisappearing -> model { currentlyDisplayed = ChecklistType checklists $ Just (originalChecklist, EditingItems (CheckItemTransitioning idx CheckAppearing)) }
    _ -> model { currentlyDisplayed = ChecklistType checklists Nothing }
  _ -> model { errorStr = Just ("Unable to switch transition while display mode is " ++ show (currentlyDisplayed model))}

switchCurrentlyDisplayed :: Model -> Effect AppEvent Model
switchCurrentlyDisplayed model = case currentlyDisplayed model of
  NoteType      notes _      -> noEff $ model { currentlyDisplayed = ChecklistType (checklistsData model) Nothing, notesData = notes }
  ChecklistType checklists _ -> noEff $ model { currentlyDisplayed = NoteType (notesData model) Nothing, checklistsData = checklists }

toSubscription :: IO a -> Sub a
toSubscription io = (\sink -> do
    res <- io
    sink res)

-- =================================== CRUD =======================================================

callAndRetrieveBody :: FromJSON a => Request -> IO (Maybe a)
callAndRetrieveBody req = do 
  maybeBS <- fmap contents $ xhrByteString req
  case maybeBS of
    Nothing -> return Nothing
    Just bs -> return $ decode bs

decode :: FromJSON fromJson => ByteString -> Maybe fromJson
decode = Aeson.decode . fromStrict

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
callPostNote :: NoteContent -> IO SharadEventInstance
callPostNote newContent = do
  maybeStoreId <- callAndRetrieveBody (postNoteRequest newContent)
  return $ maybe (ErrorHappened "No Body in POST /note response")
                 (\storeId -> NoteCreated Identifiable { storageId = storeId, content = newContent })
                 maybeStoreId

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

callPutChecklist :: Identifiable ChecklistContent -> IO AppEvent
callPutChecklist newChecklist = do
  maybeStoreId <- callAndRetrieveBody (putChecklistRequest newChecklist)
  (return . SharadEvent) $ maybe (ErrorHappened "No Body in PUT /note response")
                                 (\storeId -> ChecklistChanged Identifiable { storageId = storeId, content = content newChecklist })
                                 maybeStoreId


callPostChecklist :: ChecklistContent -> IO SharadEventInstance
callPostChecklist newContent = do
  maybeStoreId <- callAndRetrieveBody (postChecklistRequest newContent)
  return $ maybe (ErrorHappened "No Body in POST /note response")
                 (\storeId -> ChecklistCreated Identifiable { storageId = storeId, content = newContent })
                 maybeStoreId

asRequestBody :: ToJSON requestObj => requestObj -> RequestData
asRequestBody = StringData . ms . toString . toStrict . encode

-- ================================ VIEW =================================================

appView :: Model -> View AppEvent
appView model = 
  main_ [ id_ "App", class_ "container" ]
    (modalEditView model ++ 
    [ div_ [ class_ "row" ]
        [ header_ [ class_ "col pt-2" ]
            (listViewFromMaybe errorView (errorStr model) ++ [ createItemsButtonView (currentlyDisplayed model)
                                                             , h1_ [ class_ "row h2 justify-content-center" ] [ text "Sharad" ] 
                                                             , navigationMenuView (currentlyDisplayed model)
                                                             ])
        ]
    , div_ [ class_ "row" ]
        [ div_ [ class_ "col" ] [ ul_ [ class_ "list-group" ] contentView ] ]
    ])
  where
      contentView = case currentlyDisplayed model of
          NoteType notes _ -> map noteView notes
          ChecklistType checklists (Just (originalChecklist, checklistEditionState)) -> map (\checklist -> if (id . storageId) checklist == (id . storageId) originalChecklist then checklistView (Just checklistEditionState) checklist else checklistView Nothing checklist) checklists
          ChecklistType checklists Nothing -> map (checklistView Nothing) checklists

modalEditView :: Model -> [View AppEvent]
modalEditView model = case currentlyDisplayed model of
    NoteType _ (Just editionState@(EditingNewNote editedContent))             -> modalNoteEditView (title editedContent) (noteContent editedContent) editionState (editionModalState model)
    NoteType _ (Just editionState@(EditingExistingNote _ editedContent))      -> modalNoteEditView (title editedContent) (noteContent editedContent) editionState (editionModalState model)
    _ -> []

navigationMenuView :: DisplayMode -> View AppEvent
navigationMenuView currDisplayed =
     ul_ [ class_ "row nav nav-tabs nav-fill" ]
         [ li_ [ class_ "nav-item" ]
             [ a_ ([ class_ (ms $ "nav-link " ++ activePropertyForNotes), href_ "#" ] ++  onClickChangeToNotes) [ text "Notes" ]]
         , li_ [ class_ "nav-item" ]
             [ a_ ([ class_ (ms $ "nav-link " ++ activePropertyForChecklists), href_ "#" ] ++ onClickChangeToChecklists)  [text "Checklists" ]]
         ]
  where
     activePropertyForNotes = case currDisplayed of
       NoteType _ _ -> "active"
       _ -> ""
     activePropertyForChecklists = case currDisplayed of
       ChecklistType _ _ -> "active"
       _ -> ""
     onClickChangeToNotes = case currDisplayed of
       NoteType _ _ -> []
       _ -> [ onClick (SharadEvent ChangeDisplayed) ]
     onClickChangeToChecklists = case currDisplayed of
       ChecklistType _ _ -> []
       _ -> [ onClick (SharadEvent ChangeDisplayed) ]

createItemsButtonView :: DisplayMode -> View AppEvent
createItemsButtonView (NoteType _ _) = div_ [ class_ "row justify-content-end px-3" ] [ openNoteCreationModalButton ]
createItemsButtonView (ChecklistType _ _) = div_ [ class_ "row justify-content-end px-3" ] [ openChecklistCreationModalButton ]

listViewFromMaybe :: (a -> View AppEvent) -> Maybe a -> [View AppEvent]
listViewFromMaybe toView maybeA = maybe [] (\a -> [ toView a ]) maybeA

errorView :: String -> View AppEvent
errorView errorStr =
  div_ [ class_ "row mx-1 justify-content-center" ] 
    [ div_ [ class_ "alert alert-danger w-100 text-center with-close-button", role_ "alert" ]
      [ text (ms errorStr)
      , button_ [ type_ "button", class_ "close", onClick (SharadEvent ErrorDismissed)  ] [ span_ [] [ text "x" ] ]
      ]
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
    [ div_ [ class_ "col" ]
      ([ h2_ [ class_ "h4" ] [ text _noteTitle ] ]
      ++ noteContentView _noteContent
      ++ [ div_ [ class_ "text-center" ]
             [ button_ [ onClick (SharadEvent . DeleteNoteClicked $ (id . storageId) note), class_ "btn btn-sm btn-outline-danger" ] [ i_ [ class_ "bi bi-trash" ] [] ]]
      ])
    ]
  where
    _noteContent = noteContent (content note)
    _noteTitle = (ms . fromMaybe "" . title . content) note

checklistView :: Maybe ChecklistEditionState -> Identifiable ChecklistContent -> View AppEvent
checklistView clEditionSt checklist = 
  li_ [ class_ "row list-group-item" ]
    [ div_ [ class_ "col" ]
      [ h2_ [ class_ "row h4" ] [ checklistTitleView ] 
      , div_ [ class_ "row" ] [ checklistContentItemView clEditionSt checklist _checklistContent ]
      , div_ [ class_ "row justify-content-center" ]
         [ button_ [ onClick (SharadEvent . DeleteChecklistClicked $ (id . storageId) checklist), class_ "btn btn-sm btn-outline-danger" ]
           [ i_ [ class_ "bi bi-trash" ] [] ]
         ] 
      ]
   ]
  where
    _checklistContent = (items . content) checklist
    _checklistName    = (name . content) checklist

    checklistTitleView :: View AppEvent
    checklistTitleView = case clEditionSt of
      Just EditingTitle -> checklistTitleInputView _checklistName
      _ -> div_ [ onDoubleClick (SharadEvent $ EditChecklistTitle checklist)] [ text $ ms  _checklistName ]
    
    checklistContentItemView :: Maybe ChecklistEditionState -> Identifiable ChecklistContent -> [ChecklistItem] -> View AppEvent
    checklistContentItemView clEditionState originalChecklist items = ul_ [ class_ "col" ] ((mapWithIndex (\idx item -> li_ [ class_ "row" ] [ checklistItemView (maybeItemEditionState idx clEditionState) originalChecklist (item, idx) ]) items) ++ addItemButton)

    maybeItemEditionState :: Int -> Maybe ChecklistEditionState -> Maybe ChecklistItemsEditionState 
    maybeItemEditionState currentlyEditedIdx (Just (EditingItems cles@(EditingLabel idx))) = if idx == currentlyEditedIdx then Just cles else Nothing 
    maybeItemEditionState currentlyEditedIdx (Just (EditingItems (CheckItemTransitioning idx transitioningState))) = if idx == currentlyEditedIdx then (Just $ CheckItemTransitioning idx transitioningState) else Nothing 
    maybeItemEditionState currentlyEditedIdx _ = Nothing

    addItemButton :: [View AppEvent]
    addItemButton =
      [ li_ [ class_ "row" ]
        [ div_ [ class_ "col" ]
          [ button_ [ class_ "btn btn-light btn-block", type_ "button", onClick (SharadEvent $ AddItemToChecklist checklist) ]
            [ text "+" ]
          ]
        ]
      ]

checklistItemView :: Maybe ChecklistItemsEditionState -> Identifiable ChecklistContent -> (ChecklistItem, Int) -> View AppEvent
checklistItemView maybeChecklistEditionState originalChecklist (item, idx) =
  div_ [ class_ "col" ]
    [ div_ [ class_ "row align-items-center"]
      ([ input_ [ type_ "checkbox", class_ "checklist-item-checkbox", id_ checkboxId, checked_ (checked item), onChecked (\(Checked bool) -> if bool then SharadEvent $ CheckingItem originalChecklist idx else SharadEvent $ UncheckingItem originalChecklist idx)]] ++ itemLabelView)
    ]
  where
    checklistId = (id . storageId) originalChecklist
    checkboxId = ms $ "checklist-item-checkbox-" ++ checklistId ++ "-" ++ show idx
    itemLabelView = case maybeChecklistEditionState of
      Just (EditingLabel currentItemIdx) -> if currentItemIdx == idx then editLabelView Nothing item else displayLabelView checkboxId Nothing (item, idx)
      Just (CheckItemTransitioning editedItemIdx transition) -> displayLabelView checkboxId (if editedItemIdx == idx then Just transition else Nothing) (item, idx)
      _ -> displayLabelView checkboxId Nothing (item, idx)

displayLabelView :: MisoString -> Maybe CheckItemTransition -> (ChecklistItem, Int) -> [View AppEvent]
displayLabelView checkboxId maybeTransition (item, idx) =
  [ label_ [ for_ checkboxId, class_ "mb-0", onDoubleClick (SharadEvent $ EditItemLabel idx)]
      [ svgCheckboxView maybeTransition (checked item)
      , span_ [ class_ "pl-2" ] [ text $ ms (label item) ]
      ]
  ]

editLabelView :: Maybe CheckItemTransition -> ChecklistItem -> [View AppEvent]
editLabelView maybeTransition item =
  [ svgCheckboxView maybeTransition (checked item)
  , input_ [ type_ "text", id_ "checklist-item-input", class_ "col ml-2 form-control", onBlur (SharadEvent . ChecklistEditItemLabelChanged . fromMisoString), onEnterKeyHit (SharadEvent . ChecklistEditItemLabelChanged), value_ (ms $ label item)  ]
  ]

svgCheckboxView :: Maybe CheckItemTransition -> Bool -> View AppEvent
svgCheckboxView maybeTransition isChecked = maybe (notTransitioningSvg isChecked) transitioningSvg maybeTransition

transitioningSvg ::  CheckItemTransition -> View AppEvent
transitioningSvg transition = case transition of
  CheckAppearing -> svgChecked Appearing
  CheckDisappearing -> svgChecked Disappearing
  UncheckAppearing -> svgUnchecked Appearing
  UncheckDisappearing -> svgUnchecked Disappearing
    
notTransitioningSvg :: Bool -> View AppEvent
notTransitioningSvg isChecked = if isChecked then svgChecked NotTransitioning else svgUnchecked NotTransitioning
    

noteContentView :: String -> [View a]
noteContentView noteContentStr = map (\t -> p_ [] [ text $ ms t ]) (lines noteContentStr)

makeListOfEditingItems :: Maybe Int -> [ChecklistItem] -> [(ChecklistItem, Int, Bool)]
makeListOfEditingItems Nothing           items = mapWithIndex (\currentIdx item -> (item, currentIdx, False)) items
makeListOfEditingItems (Just editingIdx) items = mapWithIndex (\currentIdx currentItem -> (currentItem, currentIdx, currentIdx ==  editingIdx)) items

mapWithIndexMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
mapWithIndexMaybe f l = fst fold_
    where
        fold_ = foldr accumulatingFunction ([], length l - 1) l 
        accumulatingFunction currentItem (accumulatingList, currentIdx) = (newAccumulatingList, currentIdx - 1)
            where newAccumulatingList = let newRes = f currentIdx currentItem in
                      case newRes of
                          Just res -> res : accumulatingList
                          Nothing -> accumulatingList

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f l = mapWithIndexMaybe (\i a -> Just (f i a)) l
 
modalNoteEditView :: Maybe String -> String -> NoteEditionState -> Modal.State -> [View AppEvent]
modalNoteEditView title content noteEditionState modalEditionState =
  Modal.view "Création d'une note"
            [ form_ [ class_ "row justify-content-center" ] 
              [ div_ [ class_ "col" ] 
                [ noteTitleInputView $ ms $ fromMaybe "" title
                , noteContentInputView $ ms $ content
                ]
              ]
            ]
            [ div_ [ class_ "row justify-content-end" ]
              [ button_ [ class_ "col-4 btn btn-secondary mx-2", onClick (SharadEvent NoteEditionAborted) ] [ text "Cancel" ]
              , button_ [ class_ "col-4 btn btn-primary", onClick (SharadEvent NoteEditionFinidhed) ] [ text "Submit" ]
              ]
            ]
            modalEditionState

noteTitleInputView :: MisoString -> View AppEvent
noteTitleInputView titleValue =
  div_ [ class_ "row form-group mb-3"]
    [ div_ [ class_ "col" ]
      [ label_ [ textProp "htmlFor" "inputTitle", class_ "form-label" ] [ text "Titre" ]
      , input_ [ type_ "text", id_ "inputTitle", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedNoteTitle . fromMisoString), value_ titleValue ]
      ]
    ]

noteContentInputView :: MisoString -> View AppEvent
noteContentInputView contentValue =
  div_ [ class_ "row form-group mb-3"]
    [ div_ [ class_ "col" ]
      [ label_ [ textProp "htmlFor" "inputContent", class_ "form-label" ] [ text "Contenu" ]
      , textarea_ [ id_ "inputContent", class_ "form-control", onChange (SharadEvent . UpdateCurrentlyEditedNoteBody . fromMisoString), value_ contentValue ] []
      ]
    ]

checklistTitleInputView :: String -> View AppEvent
checklistTitleInputView name  =
  div_ [ class_ "form-group mb-3 row" ]
    [ div_ [ class_ "col" ]
      [ input_ [ type_ "text", id_ "inputTitle", class_ "row form-control", onBlur (SharadEvent . UpdateCurrentlyEditedChecklistTitle . fromMisoString), onEnterKeyHit (SharadEvent . UpdateCurrentlyEditedChecklistTitle), value_ $ ms name ]
      ]
    ]

data CheckTransitioningState = Appearing
                             | Disappearing
                             | NotTransitioning

svgChecked :: CheckTransitioningState -> View AppEvent
svgChecked transition =
  svg_ ([ _width, _height, _viewBox, _class ] ++ onAnimationEndIfNeeded)
       [ path_ [ d_ "M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-3.97-3.03a.75.75 0 0 0-1.08.022L7.477 9.417 5.384 7.323a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-.01-1.05z" ] [] ]
  where
    _width = width_ "16"
    _height = height_ "16"
    _viewBox = viewBox_ "0 0 16 16"
    _class = class_ $ ms ("svg-checkbox svg-checkbox-checked" ++ transitionClass)
    transitionClass = case transition of
      Appearing        -> " appearing"
      Disappearing     -> " disappearing"
      NotTransitioning -> ""
    onAnimationEndIfNeeded = case transition of
      Disappearing -> [ onAnimationEnd (SharadEvent TransitionCheckboxDisappearingEnd) ]
      _            -> []

svgUnchecked :: CheckTransitioningState -> View AppEvent
svgUnchecked transition =
  svg_ ([ _width, _height, _viewBox, _class ] ++ onAnimationEndIfNeeded)
       [ path_ [ d_ "M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z" ] [] ]
  where
    _width = width_ "16"
    _height = height_ "16"
    _viewBox = viewBox_ "0 0 16 16"
    _class = class_ $ ms ("svg-checkbox svg-checkbox-unchecked" ++ transitionClass)
    transitionClass = case transition of
      Appearing -> " appearing"
      Disappearing -> " disappearing"
      NotTransitioning -> ""
    onAnimationEndIfNeeded = case transition of
      Disappearing -> [ onAnimationEnd (SharadEvent TransitionCheckboxDisappearingEnd) ]
      _            -> []

onAnimationEnd :: action -> Attribute action
onAnimationEnd action = on "animationend" emptyDecoder (\() -> action)

onBlur :: (MisoString -> action) -> Attribute action
onBlur = on "blur" valueDecoder

onEnterKeyHit :: (String -> AppEvent) -> Attribute AppEvent
onEnterKeyHit toAction = on "keyup" onEnterInputDecoder (\maybeMisoStr -> maybe (SharadEvent NoEffect) (toAction . fromMisoString) maybeMisoStr)

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

emptyChecklistContent :: ChecklistContent
emptyChecklistContent = ChecklistContent { name = "", items = [] }

emptyChecklistItem :: ChecklistItem
emptyChecklistItem = ChecklistItem { label = "", checked = False }

