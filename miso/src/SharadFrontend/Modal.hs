{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Modal (Event(..), State(..), view, update) where

import Miso (View, Effect, now, noEff, consoleLog, style_, (<#))
import Miso.String (MisoString, ms)
import Miso.Html (div_, text)
import Miso.Html.Property (class_, intProp)
import Miso.Util ((=:))

data Event =
    ShowingTriggered
  | Shown
  | Dismissed
  | TimedAction Double Double (IO Event)

data State = Hidden | Transitioning | Visible deriving(Eq, Show)

update :: Event -> State -> Effect Event State
update ShowingTriggered                         state = Transitioning <# do
                                                          currentTime <- now
                                                          return $ TimedAction currentTime displayDelay (consoleLog "Going to ModalShown" >> return Shown)
update Shown                                    state = noEff Visible
update Dismissed                                state = noEff Hidden
update (TimedAction startTime delay thenAction) state = state <# do
                                                          currentTime <- now
                                                          if currentTime - startTime > delay
                                                            then thenAction
                                                            else do
                                                              consoleLog . ms $ "looping cause currentDelay is " ++ (show (currentTime - startTime)) ++ ". Expected delay is " ++ (show delay)
                                                              return $ TimedAction startTime delay thenAction

displayDelay :: Double
displayDelay = 100

view :: MisoString -> [ View a ] -> [ View a ] -> State -> View a
view modalTitle bodyContent footerContent state =
  div_ ([ class_ "arbitrary-modal arbitrary-fade ", intProp "tabindex" (-1) ] ++ visibilityProperties)
    [ div_ [ class_ "arbitrary-modal-dialog" ]
        [ div_ [ class_ "arbitrary-modal-content" ] 
            [ div_ [ class_ "arbitrary-modal-header justify-content-center" ]
                [ div_ [ class_ "arbitrary-modal-title" ] [ text modalTitle ] ]
            , div_ [ class_ "arbitrary-modal-body" ] bodyContent                
            , div_ [ class_ "modal-footer" ] footerContent
            ]
        ]
    ]
  where 
    visibilityProperties = case state of
      Hidden        -> []
      Transitioning -> [ style_ ("display" =: "block") ]
      Visible       -> [ class_ "arbitrary-show" ]

-- bodyContent = [ form_ [ class_ "row justify-content-center" ] 
--                   [ titleInputView $ ms $ fromMaybe "" (editedNoteTitle (noteEditionState model))
--                   , contentInputView $ ms $ fromMaybe "" (editedNoteBody (noteEditionState model))
--                   ]
--               ]

-- modalTitle = "Création d'une note"

-- footerContent = [ button_ [ class_ "btn btn-secondary", textProp "data-dismiss" "modal" ] [ text "Cancel" ]
--                 , button_ [ class_ "btn btn-primary", onClick (NoteEditionFinidhed finalEditionState), textProp "data-dismiss" "modal" ] [ text "Submit" ]
--                 ]