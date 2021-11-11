module Main where

import Prelude
import App (runAppM)
import App.Router as Router
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      router = H.hoist runAppM Router.component
    app <- runUI router unit body
    Router.startRouting app
