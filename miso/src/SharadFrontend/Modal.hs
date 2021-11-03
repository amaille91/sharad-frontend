{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Modal (Event(..), State(..), view, update) where

import Control.Monad.Writer.Strict (tell)
import Control.Monad.State.Strict (modify)

import Miso (View, now, noEff, consoleLog, style_, (<#), callbackToJSVal, asyncCallback)
import Miso.String (MisoString, ms)
import Miso.Types (Transition)
import Miso.Html (div_, text)
import Miso.Html.Property (class_, intProp)
import Miso.Effect (Effect(..), Sub, Sink, noEff)
import Miso.Util ((=:))

import Language.Javascript.JSaddle (jsg, (#), toJSVal)

------------ DATA -------------------------------------------------
data Event =
    ShowingTriggered
  | Shown
  | Dismissed

data State = Hidden | Transitioning | Hiding | Visible deriving (Eq, Show)

------------ CONSTANTS --------------------------------------------

displayDelay :: Double
displayDelay = 100

update :: Event -> State -> Effect Event State
update ShowingTriggered state = Effect Transitioning [ logExpectedState Hidden        state, setTimeoutModalShown displayDelay ]
update Shown            state = case state of
  Transitioning -> Effect Visible [ logExpectedState Transitioning state ]
  Hiding        -> Effect Hidden  [ logExpectedState Hiding state ]
update Dismissed        state = Effect Hiding [ logExpectedState Visible state, setTimeoutModalShown displayDelay ]


view :: MisoString -> [ View a ] -> [ View a ] -> State -> [View a]
view modalTitle bodyContent footerContent state = 
    [ div_ ([ class_ "arbitrary-modal-overlay arbitrary-fade-quick"] ++ visibilityProperties) []
    , div_ ([ class_ "arbitrary-modal arbitrary-fade ", intProp "tabindex" (-1) ] ++ visibilityProperties)
        [ div_ [ class_ "arbitrary-modal-dialog" ]
            [ div_ [ class_ "arbitrary-modal-content container" ] 
                [ div_ [ class_ "arbitrary-modal-header justify-content-center row" ]
                    [ div_ [ class_ "arbitrary-modal-title col" ] [ text modalTitle ] ]
                , div_ [ class_ "arbitrary-modal-body" ] bodyContent
                , div_ [ class_ "modal-footer row" ] [ div_ [ class_ "col" ] footerContent ]
                ]
            ]
        ]
    ]
  where
    visibilityProperties = case state of
      Hidden -> []
      Hiding -> [ style_ ("display" =: "block") ]
      Transitioning -> [ style_ ("display" =: "block") ]
      Visible -> [ class_ "arbitrary-show" ]

setTimeoutModalShown :: Double -> Sub Event
setTimeoutModalShown delay = \sink -> do
  consoleLog . ms $ "Setting timeout for " ++ show delay
  callback <- callbackToJSVal =<< asyncCallback (consoleLog "Gonna sink ModalShown" >> sink Shown)
  delayJSVal <- toJSVal delay
  jsg window # setTimeout $ [callback, delayJSVal]
  consoleLog "Exiting setTimeout"
  where
    window = "window" :: String
    setTimeout = "setTimeout" :: String

logExpectedState :: State -> State -> Sub Event
logExpectedState expectedState currentState = \sink -> do
  if expectedState /= currentState
    then consoleLog . ms $ "Expected state " ++ show expectedState ++ " but got " ++ show currentState
    else return ()
