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
import Miso.Effect (Effect(..), noEff, (<#), Sub, mapSub)
import Miso.Svg.Element (svg_, path_)
import Miso.Svg.Attribute (width_, height_, viewBox_, d_)
import JavaScript.Web.XMLHttpRequest (xhrByteString, Response(..), Request(..), Method(..), RequestData(..))
import Language.Javascript.JSaddle.Object (jsg1)
import Language.Javascript.JSaddle.Types (JSM)

import Data.Aeson (Value(..), FromJSON, ToJSON, encode, withObject, withArray, Array(..), (.:))
import qualified Data.Aeson as Aeson (decode)

import SharadFrontend.System

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

type NotesDisplayedState = ([Identifiable NoteContent], Maybe (Identifiable NoteContent, NoteEditingState))

data NoteEditingState = EditingNoteTitle | EditingNoteBody deriving (Eq, Show)

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

asRequestBody :: ToJSON requestObj => requestObj -> RequestData
asRequestBody = StringData . ms . toString . toStrict . encode

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
          Model { currentlyDisplayed = NoteType, notesData = DisplayingNotes (notes, Nothing) } -> map (\note -> either SystemEventInstance NoteEventInstance <$> noteView Nothing note) notes
          Model { currentlyDisplayed = NoteType, notesData = DisplayingNotes (notes, Just (originalNote, noteEditionState)) } -> map ((either SystemEventInstance NoteEventInstance <$>) . (\currentNote -> if (id . storageId) currentNote == (id . storageId) originalNote then noteView (Just noteEditionState) currentNote else noteView Nothing currentNote)) notes
          Model { currentlyDisplayed = ChecklistType, checklistsData = DisplayingChecklist (checklists, Just (originalChecklist, checklistEditionState)) } -> map ((either SystemEventInstance ChecklistEvent <$>) . (\checklist -> if (id . storageId) checklist == (id . storageId) originalChecklist then checklistView (Just checklistEditionState) checklist else checklistView Nothing checklist)) checklists
          Model { currentlyDisplayed = ChecklistType, checklistsData = DisplayingChecklist (checklists, Nothing) } -> map ((either SystemEventInstance ChecklistEvent <$>) . (checklistView Nothing)) checklists

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

noteView :: Maybe NoteEditingState -> Identifiable NoteContent -> View (Either SystemEvent NoteEvent)
noteView noteEditionState note = 
  li_ [ class_ "row list-group-item" ]
    [ div_ [ class_ "col" ]
      ([ h2_ [ class_ "row h4" ] [ noteTitleView ] ] ++
      [ div_ [ class_ "row my-2" ] [ noteContentView ]
      , div_ [ class_ "text-center" ]
        [ button_ [ onClick (Right $ DeleteNoteClicked $ (id . storageId) note), class_ "btn btn-sm btn-outline-danger" ] [ i_ [ class_ "bi bi-trash" ] [] ]]
      ])
    ]
  where
    _noteContent = noteContent (content note)
    _noteTitle = (ms . fromMaybe "" . title . content) note

    noteTitleView = case noteEditionState of
      Just EditingNoteTitle -> noteTitleInputView _noteTitle
      _ -> div_ [ class_ "col", onDoubleClick (Right $ EditNoteTitle note) ] [ text $ ms   _noteTitle ]

    noteContentView = case noteEditionState of
      Just EditingNoteBody -> Right <$> noteContentInputView _noteContent
      _ -> Right <$> noteContentDisplayView _noteContent note

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

checklistView :: Maybe ChecklistEditionState -> Identifiable ChecklistContent -> View (Either SystemEvent ChecklistEventInstance)
checklistView clEditionSt checklist = 
  li_ [ class_ "row list-group-item" ]
    [ div_ [ class_ "col" ]
      [ h2_ [ class_ "row h4" ] [ checklistTitleView ] 
      , div_ [ class_ "row" ] [ checklistContentItemView clEditionSt checklist _checklistContent ]
      , Right <$> addItemButton
      , div_ [ class_ "row justify-content-center" ]
         [ button_ [ onClick (Right . DeleteChecklistClicked $ (id . storageId) checklist), class_ "btn btn-sm btn-outline-danger" ]
           [ i_ [ class_ "bi bi-trash" ] [] ]
         ] 
      ]
   ]
  where
    _checklistContent = (items . content) checklist
    _checklistName    = (name . content) checklist

    checklistTitleView :: View (Either SystemEvent ChecklistEventInstance)
    checklistTitleView = case clEditionSt of
      Just EditingChecklistTitle -> checklistTitleInputView _checklistName
      _ -> div_ [ onDoubleClick (Right $ EditChecklistTitle checklist)] [ text $ ms  _checklistName ]
    
    checklistContentItemView :: Maybe ChecklistEditionState -> Identifiable ChecklistContent -> [ChecklistItem] -> View (Either SystemEvent ChecklistEventInstance)
    checklistContentItemView clEditionState originalChecklist items = ul_ [ class_ "col" ] (mapWithIndex (\idx item -> li_ [ class_ "row" ] [ checklistItemView (maybeItemEditionState idx clEditionState) originalChecklist (item, idx) ]) items)

    maybeItemEditionState :: Int -> Maybe ChecklistEditionState -> Maybe ChecklistItemsEditionState 
    maybeItemEditionState currentlyEditedIdx (Just (EditingItems cles@(EditingLabel idx))) = if idx == currentlyEditedIdx then Just cles else Nothing 
    maybeItemEditionState currentlyEditedIdx (Just (EditingItems (CheckItemTransitioning idx transitioningState))) = if idx == currentlyEditedIdx then (Just $ CheckItemTransitioning idx transitioningState) else Nothing 
    maybeItemEditionState currentlyEditedIdx _ = Nothing

    addItemButton :: View ChecklistEventInstance
    addItemButton =
      div_ [ class_ "row py-3" ]
        [ button_ [ class_ "btn btn-outline-dark btn-block", type_ "button", onClick (AddItemToChecklist checklist) ]
          [ text "+" ]
        ]
      

checklistItemView :: Maybe ChecklistItemsEditionState -> Identifiable ChecklistContent -> (ChecklistItem, Int) -> View (Either SystemEvent ChecklistEventInstance)
checklistItemView maybeChecklistEditionState originalChecklist (item, idx) =
  div_ [ class_ "col" ]
    [ div_ [ class_ "row align-items-center"] itemLabelView ]
  where
    checklistId = (id . storageId) originalChecklist
    checkboxId = ms $ "checklist-item-checkbox-" ++ checklistId ++ "-" ++ show idx
    itemLabelView = case maybeChecklistEditionState of
      Just (EditingLabel currentItemIdx) -> if currentItemIdx == idx then editLabelView Nothing item else map (Right <$>) $ displayLabelView checkboxId originalChecklist Nothing (item, idx)
      Just (CheckItemTransitioning editedItemIdx transition) -> map (Right <$>) $ displayLabelView checkboxId originalChecklist (if editedItemIdx == idx then Just transition else Nothing) (item, idx)
      _ -> map (Right <$>) (displayLabelView checkboxId originalChecklist Nothing (item, idx))

displayLabelView :: MisoString -> Identifiable ChecklistContent -> Maybe CheckItemTransition -> (ChecklistItem, Int) -> [View ChecklistEventInstance]
displayLabelView checkboxId originalChecklist maybeTransition (item, idx) =
  [ label_ [ for_ checkboxId, class_ "col-11 mb-0", onDoubleClick (EditItemLabel originalChecklist idx)]
      [ div_ [ class_ "row align-items-center" ]
        [ input_ [ type_ "checkbox", class_ "checklist-item-checkbox", id_ checkboxId, checked_ (checked item), onChecked (\(Checked bool) -> if bool then CheckingItem originalChecklist idx else UncheckingItem originalChecklist idx)]
        , svgCheckboxView maybeTransition (checked item)
        , span_ [ class_ "pl-2" ] [ text $ ms (label item) ]
        ]
      ]
  , button_ [ class_ "col-1 close", onClick (DeleteChecklistItem originalChecklist idx) ] [ text "x" ]
  ]

editLabelView :: Maybe CheckItemTransition -> ChecklistItem -> [View (Either SystemEvent ChecklistEventInstance)]
editLabelView maybeTransition item =
  [ Right <$> svgCheckboxView maybeTransition (checked item)
  , input_ [ type_ "text", id_ "checklist-item-input", class_ "col ml-2 form-control", onBlur (Right . ChecklistEditItemLabelChanged . fromMisoString), onEnterKeyHit ChecklistEditItemLabelChanged, value_ (ms $ label item)  ]
  ]

svgCheckboxView :: Maybe CheckItemTransition -> Bool -> View ChecklistEventInstance
svgCheckboxView maybeTransition isChecked = maybe (notTransitioningSvg isChecked) transitioningSvg maybeTransition

transitioningSvg ::  CheckItemTransition -> View ChecklistEventInstance
transitioningSvg transition = case transition of
  CheckAppearing -> svgChecked Appearing
  CheckDisappearing -> svgChecked Disappearing
  UncheckAppearing -> svgUnchecked Appearing
  UncheckDisappearing -> svgUnchecked Disappearing
    
notTransitioningSvg :: Bool -> View ChecklistEventInstance
notTransitioningSvg isChecked = if isChecked then svgChecked NotTransitioning else svgUnchecked NotTransitioning

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
 
checklistTitleInputView :: String -> View (Either SystemEvent ChecklistEventInstance)
checklistTitleInputView name  =
  div_ [ class_ "form-group mb-3 row" ]
    [ div_ [ class_ "col" ]
      [ input_ [ type_ "text", id_ "checklist-title-input", class_ "row form-control", onBlur (Right . UpdateCurrentlyEditedChecklistTitle . fromMisoString), onEnterKeyHit UpdateCurrentlyEditedChecklistTitle, value_ $ ms name ]
      ]
    ]

data CheckTransitioningState = Appearing
                             | Disappearing
                             | NotTransitioning

svgChecked :: CheckTransitioningState -> View ChecklistEventInstance
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
      Disappearing -> [ onAnimationEnd TransitionCheckboxDisappearingEnd ]
      _            -> []

svgUnchecked :: CheckTransitioningState -> View ChecklistEventInstance
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
      Disappearing -> [ onAnimationEnd TransitionCheckboxDisappearingEnd ]
      _            -> []

onAnimationEnd :: action -> Attribute action
onAnimationEnd action = on "animationend" emptyDecoder (\() -> action)

onBlur :: (MisoString -> action) -> Attribute action
onBlur = on "blur" valueDecoder

onEnterKeyHit :: (String -> a) -> Attribute (Either SystemEvent a)
onEnterKeyHit toAction = on "keyup" onEnterInputDecoder (\maybeMisoStr -> maybe (Left NoEffect) (Right . toAction . fromMisoString) maybeMisoStr)

onEnterInputDecoder :: Decoder (Maybe MisoString)
onEnterInputDecoder = Decoder { decodeAt = DecodeTargets [["target"], []]
                              , decoder = withArray "event" (\array -> do
                                   value <- withObject "target" (\o -> o .: "value") (array ! 0)
                                   KeyCode keyCode <- decoder keycodeDecoder (array ! 1)
                                   if keyCode == enterKeyCode then return $ Just value else return Nothing)
                               }
                               where enterKeyCode = 13 

emptyNoteContent :: NoteContent
emptyNoteContent = NoteContent { title = Just "A new title", noteContent = "" }

-- ================================= CHECKLISTS ==============================================================================

data ChecklistData = NotDisplayingChecklist [Identifiable ChecklistContent]
                   | DisplayingChecklist ChecklistDisplayedState
                   deriving(Eq, Show)

type ChecklistDisplayedState = ([Identifiable ChecklistContent], Maybe (Identifiable ChecklistContent, ChecklistEditionState))

data ChecklistEditionState = EditingChecklistTitle
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

data ChecklistEventInstance = UpdateCurrentlyEditedChecklistTitle String
                            | ChecklistEditItemLabelChanged String
                            | ChecklistCreated (Identifiable ChecklistContent)
                            | CheckingItem (Identifiable ChecklistContent) Int
                            | UncheckingItem (Identifiable ChecklistContent) Int
                            | RetrivedChecklists [Identifiable ChecklistContent]
                            | DeleteChecklistClicked String
                            | DeleteChecklistItem (Identifiable ChecklistContent) Int
                            | ChecklistDeleted String
                            | ChecklistChanged (Identifiable ChecklistContent)
                            | EditChecklistTitle (Identifiable ChecklistContent)
                            | EditItemLabel (Identifiable ChecklistContent) Int
                            | AddItemToChecklist (Identifiable ChecklistContent)
                            | TransitionCheckboxDisappearingEnd
                            | CreateChecklistClicked
                            deriving (Eq, Show)

emptyChecklistContent :: ChecklistContent
emptyChecklistContent = ChecklistContent { name = "", items = [] }

emptyChecklistItem :: ChecklistItem
emptyChecklistItem = ChecklistItem { label = "", checked = False }
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

callPutChecklist :: Identifiable ChecklistContent -> IO (Either SystemEvent ChecklistEventInstance)
callPutChecklist newChecklist = do
  maybeStoreId <- callAndRetrieveBody (putChecklistRequest newChecklist)
  return $ maybe (Left $ ErrorHappened "No Body in PUT /note response")
                 (\storeId -> Right $ ChecklistChanged Identifiable { storageId = storeId, content = content newChecklist })
                 maybeStoreId


callPostChecklist :: ChecklistContent -> IO (Either SystemEvent ChecklistEventInstance)
callPostChecklist newContent = do
  maybeStoreId <- callAndRetrieveBody (postChecklistRequest newContent)
  return $ maybe (Left $ ErrorHappened "No Body in POST /note response")
                 (\storeId -> Right $ ChecklistCreated Identifiable { storageId = storeId, content = newContent })
                 maybeStoreId

callDeleteChecklist :: String -> IO (Either SystemEvent ChecklistEventInstance)
callDeleteChecklist checklistId = do
  response <- xhrByteString $ deleteChecklistRequest checklistId
  if status response /= 200
    then return (Left $ ErrorHappened "Server answer != 200 OK")
    else return (Right $ ChecklistDeleted checklistId)

-- =========================== CHECKLIST UPDATES =======================================================================

updateChecklistNotDisplaying :: ChecklistEventInstance -> [Identifiable ChecklistContent] -> Effect (Either SystemEvent ChecklistEventInstance) [Identifiable ChecklistContent]
updateChecklistNotDisplaying (ChecklistCreated checklist) checklists = noEff (checklists ++ [checklist])
updateChecklistNotDisplaying (RetrivedChecklists checklists) _ = noEff checklists
updateChecklistNotDisplaying (ChecklistDeleted checklistId) oldChecklists = noEff (deleteIdentifiableFromId oldChecklists  checklistId)
updateChecklistNotDisplaying (ChecklistChanged checklist) oldChecklists = noEff (changeChecklist checklist oldChecklists)
updateChecklistNotDisplaying event checklists = checklists <# (return $ Left (ErrorHappened $ "could not handle event " ++ show event ++ "while not displaying checklists")) 

updateChecklistDisplaying :: ChecklistEventInstance -> ChecklistDisplayedState -> Effect (Either SystemEvent ChecklistEventInstance) ChecklistDisplayedState
updateChecklistDisplaying (RetrivedChecklists checklists) (_, maybeEditionState) = noEff (checklists, maybeEditionState)
updateChecklistDisplaying CreateChecklistClicked (checklists, _) = (checklists, Nothing) <# callPostChecklist emptyChecklistContent { name = "New Checklist"}
updateChecklistDisplaying (ChecklistCreated checklist) (oldChecklists, _) = (oldChecklists ++ [checklist], Just (checklist, EditingChecklistTitle)) <# (return $ Left $ ScrollAndFocus "checklist-title-input")
updateChecklistDisplaying (EditChecklistTitle originalChecklist) (oldChecklists, _) = noEff (oldChecklists, Just (originalChecklist, EditingChecklistTitle))
updateChecklistDisplaying (UpdateCurrentlyEditedChecklistTitle newTitle) model = handleTitleModificationEnd newTitle model
updateChecklistDisplaying (EditItemLabel checklist idx) (oldChecklists, _) = noEff (oldChecklists, Just (checklist, EditingItems $ EditingLabel idx))
updateChecklistDisplaying (ChecklistEditItemLabelChanged newLabel) model   = updateItemLabel model newLabel 
updateChecklistDisplaying (CheckingItem originalChecklist itemIdx) (oldChecklists, _) =
  let newChecklists :: [Identifiable ChecklistContent] = modifyChecklistItemCheck originalChecklist itemIdx True oldChecklists
      Just newChecklist = find ((==  (id . storageId) originalChecklist) . id . storageId) newChecklists
  in
    (newChecklists, Just (originalChecklist, EditingItems $ CheckItemTransitioning itemIdx UncheckDisappearing)) <# callPutChecklist newChecklist
updateChecklistDisplaying (UncheckingItem originalChecklist itemIdx) (oldChecklists, _) =
  let newChecklists = modifyChecklistItemCheck originalChecklist itemIdx False oldChecklists
      Just newChecklist = find ((== (id . storageId) originalChecklist) . id . storageId) newChecklists
  in
    (newChecklists, Just (originalChecklist, EditingItems $ CheckItemTransitioning itemIdx CheckDisappearing)) <# callPutChecklist newChecklist
updateChecklistDisplaying TransitionCheckboxDisappearingEnd model = noEff $ switchCheckTransitionMode model 
updateChecklistDisplaying (DeleteChecklistItem checklist idx) checklistDisplayState = checklistDisplayState <# callPutChecklist checklist { content = changeItemWithIndex idx (const Nothing) (content checklist) }
updateChecklistDisplaying (AddItemToChecklist checklist) (oldChecklists, _) = newModel <# (return $ Left $ ScrollAndFocus "checklist-item-input")
  where
    newModel = (newChecklists, Just (newChecklist, EditingItems $ EditingLabel indexToEdit))
    newChecklists = addItemToChecklist checklist oldChecklists
    indexToEdit = (length $ items newContent) - 1
    newChecklist = checklist { content = newContent }
    newContent = (content checklist) { items = (items (content checklist)) ++ [emptyChecklistItem] }
updateChecklistDisplaying (ChecklistChanged checklist) (oldChecklists, maybeEditState) = noEff (changeChecklist checklist oldChecklists, maybeEditState >>= updateChecklistInEditState checklist)
updateChecklistDisplaying (DeleteChecklistClicked checklistId) checklistDisplayState = checklistDisplayState <# callDeleteChecklist checklistId
updateChecklistDisplaying (ChecklistDeleted checklistId) (oldChecklists, editionState) = noEff (deleteIdentifiableFromId oldChecklists  checklistId, editionState)

-- ================================== UTILS FOR CHECKLIST UPDATE =========================================

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

handleTitleModificationEnd :: String -> ChecklistDisplayedState -> Effect (Either SystemEvent ChecklistEventInstance) ChecklistDisplayedState
handleTitleModificationEnd newTitle (checklists, Just (originalChecklist, checklistEditionState)) =
  (checklists, Nothing) <# callPutChecklist originalChecklist { content = newContent }
  where
    newContent = (content originalChecklist) { name = newTitle }

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

updateItemLabel :: ChecklistDisplayedState -> String -> Effect (Either SystemEvent ChecklistEventInstance) ChecklistDisplayedState 
updateItemLabel (checklists, Just (originalChecklist, EditingItems (EditingLabel idx))) newLabel = (checklists, Nothing) <# callPutChecklist newChecklist
  where
    newChecklist = originalChecklist { content = newContent }
    newContent = changeItemLabel idx newLabel (content originalChecklist)
updateItemLabel clDisplayState@(_, maybeEditionState) _ = clDisplayState <# (return $ Left $ ErrorHappened $ "Cannot update item label while on checklist edition state is " ++ show maybeEditionState) 

changeItemLabel :: Int -> String -> ChecklistContent -> ChecklistContent
changeItemLabel idxToModify newLabel clContent =
  changeItemWithIndex idxToModify (\item -> if null newLabel then Nothing else Just item { label = newLabel }) clContent

changeItemWithIndex :: Int -> (ChecklistItem -> Maybe ChecklistItem) -> ChecklistContent -> ChecklistContent
changeItemWithIndex idx transform clContent = clContent { items = newItems }
  where
    newItems = mapWithIndexMaybe (\currIdx item -> if idx == currIdx then transform item else Just item) (items clContent)

handleCheckForChecklists :: IO (Either SystemEvent ChecklistEventInstance)
handleCheckForChecklists = do
  retrievedChecklists <- callAndRetrieveBody getChecklistsRequest
  return $ maybe (Left $ ErrorHappened "No Body in GET /checklist response")
                 (Right . RetrivedChecklists)
                 retrievedChecklists

switchCheckTransitionMode :: ChecklistDisplayedState -> ChecklistDisplayedState
switchCheckTransitionMode (checklists, (Just (originalChecklist, EditingItems (CheckItemTransitioning idx transition)))) = case transition of
  CheckDisappearing   -> (checklists, Just (originalChecklist, EditingItems (CheckItemTransitioning idx UncheckAppearing)))
  UncheckDisappearing -> (checklists, Just (originalChecklist, EditingItems (CheckItemTransitioning idx CheckAppearing)))
  _                   -> (checklists, Nothing)
switchCheckTransitionMode model = model

retrieveChecklists :: ChecklistData -> [Identifiable ChecklistContent]
retrieveChecklists (NotDisplayingChecklist cls) = cls
retrieveChecklists (DisplayingChecklist (cls, _)) = cls

