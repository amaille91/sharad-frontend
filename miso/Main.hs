{-# LANGUAGE OverloadedStrings #-}
module Main where

import Miso (startApp, defaultEvents, App(..), View)
import Miso.Types (LogLevel(Off))
import Miso.String (ms)
import Miso.Html (text)
import Miso.Effect (Effect, noEff)
import JavaScript.Web.XMLHttpRequest

main :: IO ()
main = startApp misoApp

misoApp :: App Model AppEvent
misoApp = App { model = initialModel 
              , update = updateApp
              , view = appView
              , subs = []
              , events = defaultEvents
              , initialAction = NoOp
              , mountPoint = Nothing
              , logLevel = Off
              }

type Model = String
data AppEvent = NoOp

initialModel :: Model
initialModel = "HelloWorld from Sharad-Miso"

updateApp :: AppEvent -> Model -> Effect AppEvent Model
updateApp _ = noEff

appView :: Model -> View AppEvent
appView model = text $ ms model
