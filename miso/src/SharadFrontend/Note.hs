{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Note where

import Prelude hiding (id)

import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Bifunctor (second)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Either (EitherT, runEitherT, newEitherT, bimapEitherT)

import SharadFrontend.Utils
import SharadFrontend.System
import SharadFrontend.Crud
import Model

import Miso (View)
import Miso.String (ms, fromMisoString)
import Miso.Effect (Effect(..), noEff, (<#))

import Miso.Html (h2_, text, p_, div_, li_, button_, i_, input_, textarea_)
import Miso.Html.Property (id_, class_, type_, value_)
import Miso.Html.Event (onClick, onDoubleClick)

data NoteData = NotDisplayingNotes [Identifiable NoteContent]
              | DisplayingNotes NotesDisplayedState
              deriving (Eq, Show)

type NotesDisplayedState = ([Identifiable NoteContent], NoteEditingState)

type NoteEditingState = Maybe (Identifiable NoteContent, NoteEditingType)

data NoteEditingType = EditingNoteTitle | EditingNoteBody deriving (Eq, Show)

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

emptyNoteContent :: NoteContent
emptyNoteContent = NoteContent { title = Just "A new title", noteContent = "" }

instance CrudType NoteToken NoteContent where
  getEndpoint NoteToken = "/note"

data NoteToken = NoteToken

callPostNote :: NoteContent -> EitherT SystemEvent IO NoteEvent
callPostNote newContent = do
  bimapEitherT toSystemError
               (\newId -> NoteCreated Identifiable { storageId = newId, content = newContent })
               (callPost NoteToken newContent)

callPutNote :: Identifiable NoteContent -> EitherT SystemEvent IO NoteEvent
callPutNote note = 
  bimapEitherT toSystemError
               (\newStorageId -> NoteChanged note { storageId = newStorageId })
               (callPut NoteToken note)

callDeleteNote :: String -> EitherT SystemEvent IO NoteEvent
callDeleteNote noteId = newEitherT (do
  maybeErr <- runMaybeT (callDelete NoteToken noteId)
  return $ maybe (Right $ NoteDeleted noteId)
                 (Left . toSystemError)
                 maybeErr)

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
updateNoteDisplaying CreateNoteClicked noteDisplayedState = noteDisplayedState <# (runEitherT $ callPostNote emptyNoteContent)
updateNoteDisplaying (EditNoteTitle originalNote) (oldNotes, _) = (oldNotes, Just (originalNote, EditingNoteTitle)) <# (return . Left . ScrollAndFocus) "note-title-input"
updateNoteDisplaying (NoteTitleEditionEnd newTitle) noteDisplayedState@(_, Just (originalNote, _)) = noteDisplayedState <# (runEitherT $ callPutNote originalNote { content = (content originalNote) { title = Just newTitle }})
updateNoteDisplaying (EditNoteBody originalNote) (oldNotes, _) = (oldNotes, Just (originalNote, EditingNoteBody)) <# (return . Left . ScrollAndFocus) "note-content-input"
updateNoteDisplaying (NoteBodyEditionEnd newBody) noteDisplayedState@(_, Just (originalNote, _)) = noteDisplayedState <# (runEitherT $ callPutNote originalNote { content = (content originalNote) { noteContent = newBody } })
updateNoteDisplaying (DeleteNoteClicked noteId) noteDisplayedState = noteDisplayedState <# (runEitherT $ callDeleteNote noteId) 


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
    _noteTitle = (fromMaybe "" . title . content) note

    noteTitleView (Just (originalNote, EditingNoteTitle))
      | (id . storageId) originalNote == (id . storageId) note = noteTitleInputView _noteTitle
      | otherwise = div_ [ class_ "col", onClick (Right $ EditNoteTitle note) ] [ text $ ms _noteTitle ]
    noteTitleView _ = div_ [ class_ "col", onClick (Right $ EditNoteTitle note) ] [ text $ ms _noteTitle ]

    noteContentView :: NoteEditingState -> View (Either SystemEvent NoteEvent)
    noteContentView (Just (originalNote, EditingNoteBody))
      | (id . storageId) originalNote == (id . storageId) note = Right <$> noteContentInputView _noteContent
      | otherwise = Right <$> noteContentDisplayView _noteContent note
    noteContentView _ = Right <$> noteContentDisplayView _noteContent note

noteTitleInputView :: String -> View (Either SystemEvent NoteEvent)
noteTitleInputView titleValue =
  div_ [ class_ "col" ]
    [ input_ [ type_ "text", id_ "note-title-input", class_ "form-control", onBlur (Right . NoteTitleEditionEnd . fromMisoString), onEnterKeyHit NoteTitleEditionEnd, value_ (ms titleValue) ] ]

noteContentDisplayView :: String -> Identifiable NoteContent -> View NoteEvent
noteContentDisplayView noteContentStr originalNote = 
  div_ [ class_ "col content-display-view", onClick (EditNoteBody originalNote) ] (map (\t -> p_ [] [ text $ ms t ]) (lines noteContentStr))

noteContentInputView :: String -> View NoteEvent
noteContentInputView contentValue =
  div_ [ class_ "col" ]
    [ textarea_ [ id_ "note-content-input", class_ "form-control", onBlur (NoteBodyEditionEnd . fromMisoString), value_ $ ms contentValue ] [] ]

changeNote :: Identifiable NoteContent -> [Identifiable NoteContent] -> [Identifiable NoteContent]
changeNote newNote (item:otherItems) =
  if (id . storageId) item  == (id . storageId) newNote 
    then newNote : otherItems
    else item    : changeNote newNote otherItems

