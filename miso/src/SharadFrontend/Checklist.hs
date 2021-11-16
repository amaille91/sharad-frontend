{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Checklist where

import Prelude hiding (id)
import Data.List (find)

import Model
import SharadFrontend.System
import SharadFrontend.Utils (callAndRetrieveBody, mapWithIndexMaybe, mapWithIndex, asRequestBody,  onBlur, onEnterKeyHit, deleteIdentifiableFromId)

import Miso.String (ms, MisoString, fromMisoString)
import Miso.Effect (Effect(..), noEff, (<#))
import Miso.Html (Attribute, h2_, text, span_, div_, ul_, li_, button_, i_, label_, input_, on, for_, onClick, onDoubleClick, onChecked)
import Miso.Html.Property (id_, class_, type_, value_, checked_)
import Miso.Event.Types (Checked(..))
import Miso.Event.Decoder (emptyDecoder)
import Miso (View)

import Miso.Svg.Element (svg_, path_)
import Miso.Svg.Attribute (width_, height_, viewBox_, d_)

import JavaScript.Web.XMLHttpRequest (xhrByteString, Request(..), Response(..), Method(..), RequestData(..))

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

-- =================================== VIEW ===============================


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

