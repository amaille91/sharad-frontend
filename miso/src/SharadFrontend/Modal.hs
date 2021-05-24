{-# LANGUAGE OverloadedStrings #-}

module SharadFrontend.Modal (Event(..), FromModalEvent, fromModalEvent, view) where

import Miso (View)
import Miso.String (MisoString)
import Miso.Html (div_, text)
import Miso.Html.Property (id_, class_, intProp)
import Miso.Html.Event (on)
import Miso.Event.Decoder (emptyDecoder)

data Event =
    ShowingTriggered
  | Shown
  | DismissTriggered
  | Dismissed

class FromModalEvent a where
  fromModalEvent :: Event -> a

view :: (FromModalEvent a) => MisoString -> [ View a ] -> [ View a ] -> View a
view modalTitle bodyContent footerContent =
  div_ [ id_ "arbitrary-modal-id", class_ "arbitrary-modal arbitrary-fade arbitrary-show", intProp "tabindex" (-1), on "transitionend" emptyDecoder (const $ fromModalEvent Shown) ]
    [ div_ [ class_ "arbitrary-modal-dialog" ]
        [ div_ [ class_ "arbitrary-modal-content" ] 
            [ div_ [ class_ "arbitrary-modal-header justify-content-center" ]
                [ div_ [ class_ "arbitrary-modal-title" ] [ text modalTitle ] ]
            , div_ [ class_ "arbitrary-modal-body" ] bodyContent                
            , div_ [ class_ "modal-footer" ] footerContent
            ]
        ]
    ]

-- bodyContent = [ form_ [ class_ "row justify-content-center" ] 
--                   [ titleInputView $ ms $ fromMaybe "" (editedNoteTitle (noteEditionState model))
--                   , contentInputView $ ms $ fromMaybe "" (editedNoteBody (noteEditionState model))
--                   ]
--               ]

-- modalTitle = "Création d'une note"

-- footerContent = [ button_ [ class_ "btn btn-secondary", textProp "data-dismiss" "modal" ] [ text "Cancel" ]
--                 , button_ [ class_ "btn btn-primary", onClick (NoteEditionFinidhed finalEditionState), textProp "data-dismiss" "modal" ] [ text "Submit" ]
--                 ]