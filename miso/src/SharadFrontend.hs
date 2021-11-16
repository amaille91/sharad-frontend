{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SharadFrontend (runSharadFrontend) where

import Prelude hiding (id)
import qualified Prelude (id)
import GHC.Generics (Generic)
import Data.Function ((&))
import Control.Exception (SomeException, catch)
import Control.Applicative ((<|>))
import Control.Arrow
import Data.Maybe (fromMaybe, fromJust)
import Data.String (lines)
import Data.ByteString.Lazy (toStrict)
import Data.Map.Strict (singleton)
import qualified Data.Bifunctor as Bifunctor (first, second, bimap)
import Control.Monad.State.Strict (StateT, mapStateT)
import Data.Aeson (Array(..), (.:))

import Miso (consoleLog, startApp, defaultEvents, stringify, App(..), View, getElementById, addEventListener, onCreated)
import Miso.Types (LogLevel(Off), Transition, toTransition, fromTransition, mapAction)
import Miso.String (ms, fromMisoString, MisoString)
import Miso.Html (Attribute, h1_, h2_, text, p_, main_, header_, span_, div_, nav_, ul_, li_, button_, a_, i_, input_, textarea_, on, for_)
import Miso.Html.Event (onClick, onDoubleClick, onSubmit, onChange, onChecked)
import Miso.Html.Property (id_, class_, type_, value_, href_, textProp, intProp, checked_)
import Miso.Effect (Effect(..), noEff, (<#), Sub, mapSub)
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))
import Language.Javascript.JSaddle.Object (jsg1)
import Language.Javascript.JSaddle.Types (JSM)


import SharadFrontend.Utils (callAndRetrieveBody, mapWithIndexMaybe, mapWithIndex, asRequestBody, onBlur, onEnterKeyHit)
import SharadFrontend.System
import SharadFrontend.Checklist

import Model

data Model = Model { currentlyDisplayed :: DisplayMode
                   , notesData :: NoteData
                   , checklistsData :: ChecklistData
                   , errorStr :: Maybe String
                   } deriving(Eq, Show)

data DisplayMode = NoteType
                 | ChecklistType deriving (Eq, Show)

data NoteData = NotDisplayingNotes [Identifiable NoteContent]
              | DisplayingNotes NotesDisplayedState
              deriving (Eq, Show)

type NotesDisplayedState = ([Identifiable NoteContent], NoteEditingState)

type NoteEditingState = Maybe (Identifiable NoteContent, NoteEditingType)

data NoteEditingType = EditingNoteTitle | EditingNoteBody deriving (Eq, Show)

data AppEvent = CheckForContent
              | NoteEventInstance NoteEvent
              | ChecklistEvent ChecklistEventInstance
              | SystemEventInstance SystemEvent
              | ChangeDisplayed
              deriving (Eq, Show)
          
misoApp :: App Model AppEvent
misoApp = App { model = initialModel 
              , update = logWrapperUpdateApp
              , view = appView
              , subs = []
              , events = defaultEvents <> singleton "animationend" False
              , initialAction = CheckForContent
              , mountPoint = Nothing
              , logLevel = Off
              }

initialModel :: Model
initialModel = Model { currentlyDisplayed = NoteType
                     , notesData = DisplayingNotes ([], Nothing)
                     , checklistsData = NotDisplayingChecklist []
                     , errorStr = Nothing
                     }

runSharadFrontend :: IO ()
runSharadFrontend = startApp misoApp

logWrapperUpdateApp :: AppEvent -> Model -> Effect AppEvent Model
logWrapperUpdateApp event model =
  let Effect newModel subs = updateApp event model
  in Effect newModel ((\sink -> do
      consoleLog $ ms ("receiving event: " ++ show event)) : subs)

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp (SystemEventInstance event) model = Bifunctor.bimap SystemEventInstance
                                                              (\newErrorStr -> model { errorStr = newErrorStr })
                                                              (updateSystem event (errorStr model))
updateApp CheckForContent model = Effect model (map toSubscription [either SystemEventInstance NoteEventInstance <$> handleCheckForNotes, either SystemEventInstance (ChecklistEvent) <$> handleCheckForChecklists])
updateApp ChangeDisplayed model = switchCurrentlyDisplayed model
updateApp (ChecklistEvent event) model = updateChecklist event model
updateApp (NoteEventInstance event) model@(Model { notesData = notesData_ }) = Bifunctor.bimap (either SystemEventInstance NoteEventInstance)
                                                                                               (\newNotesData -> model { notesData = newNotesData })
                                                                                               (updateNote event notesData_)
toSubscription :: IO a -> Sub a
toSubscription io = (\sink -> do
    res <- io
    sink res)

updateChecklist :: ChecklistEventInstance -> Model -> Effect AppEvent Model
updateChecklist event model = case checklistsData model of
  NotDisplayingChecklist checklists -> case updateChecklistNotDisplaying event checklists of
    Effect newChecklists subs -> Effect model { checklistsData = NotDisplayingChecklist newChecklists } (mapSub toSharadEvent <$> subs)
  DisplayingChecklist checklistDisplayedState -> case updateChecklistDisplaying event checklistDisplayedState of
    Effect m subs -> Effect model { checklistsData = DisplayingChecklist m } (mapSub toSharadEvent <$> subs)
  where
    toSharadEvent = either SystemEventInstance
                           (ChecklistEvent)


data NoteEvent = NotesRetrieved [Identifiable NoteContent]
               | NoteChanged (Identifiable NoteContent)
               | NoteCreated (Identifiable NoteContent)
               | NoteDeleted String
               | CreateNoteClicked
               | EditNoteTitle (Identifiable NoteContent)
               | NoteTitleEditionEnd String
               | EditNoteBody (Identifiable NoteContent)
               | NoteBodyEditionEnd String
               | DeleteNoteClicked String
               deriving (Eq, Show)

updateNote :: NoteEvent -> NoteData -> Effect (Either SystemEvent NoteEvent) NoteData
updateNote event (NotDisplayingNotes notes) = updateNoteNotDisplaying event notes & Bifunctor.second NotDisplayingNotes
updateNote event (DisplayingNotes displayedNotesState) = updateNoteDisplaying event displayedNotesState & Bifunctor.second DisplayingNotes

updateNoteNotDisplaying :: NoteEvent -> [Identifiable NoteContent] -> Effect (Either SystemEvent NoteEvent) [Identifiable NoteContent]
updateNoteNotDisplaying (NotesRetrieved notes) _ = noEff notes
updateNoteNotDisplaying (NoteChanged newNote) notes = noEff (changeNote newNote notes)
updateNoteNotDisplaying (NoteCreated note) oldNotes = noEff (oldNotes ++ [note])
updateNoteNotDisplaying (NoteDeleted noteId) oldNotes = noEff (deleteIdentifiableFromId oldNotes noteId)
updateNoteNotDisplaying event oldNotes = oldNotes <# (return . Left . ErrorHappened) ("Unable to handle event while not displaying notes.\n\t" ++ show event)

updateNoteDisplaying :: NoteEvent ->  NotesDisplayedState -> Effect (Either SystemEvent NoteEvent) NotesDisplayedState
updateNoteDisplaying (NotesRetrieved notes) (_, editionState) = noEff (notes, editionState)
updateNoteDisplaying (NoteChanged newNote) (notes, editionState) = noEff (changeNote newNote notes, Nothing)
updateNoteDisplaying (NoteCreated note) (oldNotes, editionState) = (oldNotes ++ [note], Just $ (note, EditingNoteBody)) <# (return . Left . ScrollAndFocus) "note-content-input"
updateNoteDisplaying (NoteDeleted noteId) (oldNotes, editionState) = noEff (deleteIdentifiableFromId oldNotes noteId, Nothing)
updateNoteDisplaying CreateNoteClicked noteDisplayedState = noteDisplayedState <# callPostNote emptyNoteContent
updateNoteDisplaying (EditNoteTitle originalNote) (oldNotes, _) = (oldNotes, Just (originalNote, EditingNoteTitle)) <# (return . Left . ScrollAndFocus) "note-title-input"
updateNoteDisplaying (NoteTitleEditionEnd newTitle) noteDisplayedState@(_, Just (originalNote, _)) = noteDisplayedState <# callPutNote originalNote { content = (content originalNote) { title = Just newTitle }}
updateNoteDisplaying (EditNoteBody originalNote) (oldNotes, _) = (oldNotes, Just (originalNote, EditingNoteBody)) <# (return . Left . ScrollAndFocus) "note-content-input"
updateNoteDisplaying (NoteBodyEditionEnd newBody) noteDisplayedState@(_, Just (originalNote, _)) = noteDisplayedState <# callPutNote originalNote { content = (content originalNote) { noteContent = newBody } }
updateNoteDisplaying (DeleteNoteClicked noteId) noteDisplayedState = noteDisplayedState <# callDeleteNote noteId

-- ================================== Utils for UPDATE ==============================

changeNote :: Identifiable NoteContent -> [Identifiable NoteContent] -> [Identifiable NoteContent]
changeNote newNote (item:otherItems) =
  if (id . storageId) item  == (id . storageId) newNote 
    then newNote : otherItems
    else item    : changeNote newNote otherItems

handleCheckForNotes :: IO (Either SystemEvent NoteEvent)
handleCheckForNotes = do
  retrievedNotes <- callAndRetrieveBody getNotesRequest
  return $ maybe (Left $ ErrorHappened "No Body in GET /note response")
                 (Right . NotesRetrieved)
                 retrievedNotes

switchCurrentlyDisplayed :: Model -> Effect AppEvent Model
switchCurrentlyDisplayed
  model@Model { currentlyDisplayed = NoteType
              , notesData = DisplayingNotes (oldNotes, _) } =
  noEff $ model { currentlyDisplayed = ChecklistType
                , notesData = NotDisplayingNotes oldNotes
                , checklistsData = DisplayingChecklist (retrieveChecklists $ checklistsData model, Nothing)
                }
switchCurrentlyDisplayed
  model@Model { currentlyDisplayed = ChecklistType
              , notesData = NotDisplayingNotes notes } =
  noEff $ model { currentlyDisplayed = NoteType
                , notesData = DisplayingNotes (notes, Nothing)
                , checklistsData = NotDisplayingChecklist (retrieveChecklists $ checklistsData model)
                }

-- =================================== CRUD =======================================================

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
callPostNote :: NoteContent -> IO (Either SystemEvent NoteEvent)
callPostNote newContent = do
  maybeStoreId <- callAndRetrieveBody (postNoteRequest newContent)
  return $ maybe (Left $ ErrorHappened "No Body in POST /note response")
                 (\storeId -> Right $ NoteCreated Identifiable { storageId = storeId, content = newContent })
                 maybeStoreId

callPutNote :: Identifiable NoteContent -> IO (Either SystemEvent NoteEvent)
callPutNote note = do
  newStorageId <- callAndRetrieveBody $ putNoteRequest note
  case newStorageId of
    Just newStorageId -> (return . Right . NoteChanged) note { storageId = newStorageId }
    Nothing           -> (return . Left . ErrorHappened) "No Body in POST /note response"

callDeleteNote :: String -> IO (Either SystemEvent NoteEvent)
callDeleteNote noteId = catch (do
  response <- xhrByteString $ deleteNoteRequest noteId
  if status response /= 200
    then (return . Left . ErrorHappened) "Server answer != 200 OK"
    else (return . Right . NoteDeleted) noteId)
  (\(e :: SomeException) -> (return . Left . ErrorHappened) "NetworkError")

-- ================================ VIEW =================================================

appView :: Model -> View AppEvent
appView model = 
  main_ [ id_ "App", class_ "container" ]
    [ div_ [ class_ "row" ]
        [ header_ [ class_ "col pt-2" ]
            (listViewFromMaybe errorView (errorStr model) ++ [ createItemsButtonView (currentlyDisplayed model)
                                                             , h1_ [ class_ "row h2 justify-content-center" ] [ text "Sharad" ] 
                                                             , navigationMenuView (currentlyDisplayed model)
                                                             ])
        ]
    , div_ [ class_ "row" ]
        [ div_ [ class_ "col" ] [ ul_ [ class_ "list-group" ] contentView ] ]
    ]
  where
    contentView = case model of
        Model { currentlyDisplayed = NoteType, notesData = DisplayingNotes (notes, maybeEditionState) } -> map ((either SystemEventInstance NoteEventInstance <$>) . (noteView maybeEditionState)) notes
        Model { currentlyDisplayed = ChecklistType, checklistsData = DisplayingChecklist (checklists, maybeEditionState) } -> map ((either SystemEventInstance ChecklistEvent <$>) . (checklistContentView maybeEditionState)) checklists

    checklistContentView Nothing checklist = checklistView Nothing checklist
    checklistContentView (Just (originalChecklist, editionState)) checklist
      | (id . storageId) originalChecklist == (id . storageId) checklist = checklistView (Just editionState) checklist
      | otherwise = checklistView Nothing checklist

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
       NoteType -> "active"
       _ -> ""
     activePropertyForChecklists = case currDisplayed of
       ChecklistType -> "active"
       _ -> ""
     onClickChangeToNotes = case currDisplayed of
       NoteType -> []
       _ -> [ onClick ChangeDisplayed ]
     onClickChangeToChecklists = case currDisplayed of
       ChecklistType -> []
       _ -> [ onClick ChangeDisplayed ]

createItemsButtonView :: DisplayMode -> View AppEvent
createItemsButtonView NoteType = div_ [ class_ "row justify-content-end px-3" ] [ openNoteCreationModalButton ]
createItemsButtonView ChecklistType = div_ [ class_ "row justify-content-end px-3" ] [ openChecklistCreationModalButton ]

listViewFromMaybe :: (a -> View AppEvent) -> Maybe a -> [View AppEvent]
listViewFromMaybe toView maybeA = maybe [] (\a -> [ toView a ]) maybeA

errorView :: String -> View AppEvent
errorView errorStr =
  div_ [ class_ "row mx-1 justify-content-center" ] 
    [ div_ [ class_ "alert alert-danger w-100 text-center with-close-button", role_ "alert" ]
      [ text (ms errorStr)
      , button_ [ type_ "button", class_ "close", onClick (SystemEventInstance ErrorDismissed)  ] [ span_ [] [ text "x" ] ]
      ]
    ]
  
role_ :: MisoString -> Attribute AppEvent
role_ role = textProp "role" role

openNoteCreationModalButton :: View AppEvent
openNoteCreationModalButton = 
  button_ [ class_ "btn btn-primary col-4", onClick (NoteEventInstance CreateNoteClicked) ] [ text "Create note" ]

openChecklistCreationModalButton :: View AppEvent
openChecklistCreationModalButton  = 
  button_ [ class_ "btn btn-primary col-4", onClick (ChecklistEvent CreateChecklistClicked) ] [ text "Create checklist" ]

noteView :: NoteEditingState -> Identifiable NoteContent -> View (Either SystemEvent NoteEvent)
noteView noteEditionState note = 
  li_ [ class_ "row list-group-item" ]
    [ div_ [ class_ "col" ]
      ([ h2_ [ class_ "row h4" ] [ noteTitleView noteEditionState ] ] ++
      [ div_ [ class_ "row my-2" ] [ noteContentView noteEditionState ]
      , div_ [ class_ "text-center" ]
        [ button_ [ onClick (Right $ DeleteNoteClicked $ (id . storageId) note), class_ "btn btn-sm btn-outline-danger" ] [ i_ [ class_ "bi bi-trash" ] [] ]]
      ])
    ]
  where
    _noteContent = noteContent (content note)
    _noteTitle = (ms . fromMaybe "" . title . content) note

    noteTitleView (Just (originalNote, EditingNoteTitle)) = noteTitleInputView _noteTitle
    noteTitleView _ = div_ [ class_ "col", onDoubleClick (Right $ EditNoteTitle note) ] [ text $ ms   _noteTitle ]

    noteContentView :: NoteEditingState -> View (Either SystemEvent NoteEvent)
    noteContentView (Just (originalNote, EditingNoteBody)) = Right <$> noteContentInputView _noteContent
    noteContentView _ = Right <$> noteContentDisplayView _noteContent note

noteTitleInputView :: MisoString -> View (Either SystemEvent NoteEvent)
noteTitleInputView titleValue =
  div_ [ class_ "col" ]
    [ input_ [ type_ "text", id_ "note-title-input", class_ "form-control", onBlur (Right . NoteTitleEditionEnd . fromMisoString), onEnterKeyHit NoteTitleEditionEnd, value_ titleValue ] ]

noteContentDisplayView :: String -> Identifiable NoteContent -> View NoteEvent
noteContentDisplayView noteContentStr originalNote = 
  div_ [ class_ "col content-display-view", onDoubleClick (EditNoteBody originalNote) ] (map (\t -> p_ [] [ text $ ms t ]) (lines noteContentStr))

noteContentInputView :: String -> View NoteEvent
noteContentInputView contentValue =
  div_ [ class_ "col" ]
    [ textarea_ [ id_ "note-content-input", class_ "form-control", onBlur (NoteBodyEditionEnd . fromMisoString), value_ $ ms contentValue ] [] ]

emptyNoteContent :: NoteContent
emptyNoteContent = NoteContent { title = Just "A new title", noteContent = "" }

