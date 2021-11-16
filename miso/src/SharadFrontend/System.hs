module SharadFrontend.System where

import Miso.String (ms)
import Miso.Effect (Effect, noEff, (<#))
import Miso.Effect.DOM (focus, scrollIntoView)

data SystemEvent = ErrorHappened String
                 | ScrollAndFocus String
                 | NoEffect
                 | ErrorDismissed
                 deriving (Eq, Show)

updateSystem :: SystemEvent -> Maybe String -> Effect SystemEvent (Maybe String)
updateSystem NoEffect model = noEff model
updateSystem (ScrollAndFocus elementId) model = model <# (NoEffect <$ (focus (ms elementId) >> scrollIntoView (ms elementId)))
updateSystem (ErrorHappened newErrorStr) _ = noEff (Just newErrorStr)
updateSystem ErrorDismissed _ = noEff Nothing

