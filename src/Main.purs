module Main where

import Prelude
import App (runAppM)
import App.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  credentials <- Ref.new Nothing
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      router = H.hoist (runAppM credentials) Router.component
    app <- runUI router unit body
    Router.startRouting app
