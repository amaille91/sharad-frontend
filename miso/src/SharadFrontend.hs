{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SharadFrontend (runSharadFrontend) where

import Prelude hiding (id)
import qualified Prelude (id)
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.Arrow
import Data.String (lines)
import Data.ByteString.Lazy (toStrict)
import Data.Map.Strict (singleton)
import qualified Data.Bifunctor as Bifunctor (first, bimap)
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
import SharadFrontend.Note
import SharadFrontend.Checklist

import Model

data Model = Model { currentlyDisplayed :: DisplayMode
                   , notesData :: NoteData
                   , checklistsData :: ChecklistData
                   , errorStr :: Maybe String
                   } deriving(Eq, Show)

data DisplayMode = NoteType
                 | ChecklistType deriving (Eq, Show)

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


-- ================================== Utils for UPDATE ==============================

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

