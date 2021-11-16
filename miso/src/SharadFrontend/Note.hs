{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Note where

import Prelude hiding (id)

import Data.Maybe (fromMaybe, fromJust)
import Data.Function ((&))
import Control.Exception (SomeException, catch)
import Data.Bifunctor (second)

import SharadFrontend.Utils
import SharadFrontend.System
import Model

import Miso (View)
import Miso.String (ms, fromMisoString, MisoString)
import Miso.Effect (Effect(..), noEff, (<#))

import Miso.Html (h2_, text, p_, div_, li_, button_, i_, input_, textarea_)
import Miso.Html.Property (id_, class_, type_, value_)
import Miso.Html.Event (onClick, onDoubleClick)

import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))

data NoteData = NotDisplayingNotes [Identifiable NoteContent]
              | DisplayingNotes NotesDisplayedState
              deriving (Eq, Show)

type NotesDisplayedState = ([Identifiable NoteContent], NoteEditingState)

type NoteEditingState = Maybe (Identifiable NoteContent, NoteEditingType)

data NoteEditingType = EditingNoteTitle | EditingNoteBody deriving (Eq, Show)

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

changeNote :: Identifiable NoteContent -> [Identifiable NoteContent] -> [Identifiable NoteContent]
changeNote newNote (item:otherItems) =
  if (id . storageId) item  == (id . storageId) newNote 
    then newNote : otherItems
    else item    : changeNote newNote otherItems

-- ============================================= UPDATE ======================================================

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
updateNote event (NotDisplayingNotes notes) = updateNoteNotDisplaying event notes & second NotDisplayingNotes
updateNote event (DisplayingNotes displayedNotesState) = updateNoteDisplaying event displayedNotesState & second DisplayingNotes

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

