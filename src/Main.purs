module Main where

import Prelude
import App (runAppM)
import App.Router as Router
import Data.Deployment (detectDeployment)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
  deployment <- detectDeployment
  HA.runHalogenAff do
    awaitLoad
    mBody <- HA.selectElement (QuerySelector "#sofa-app")
    case mBody of
      Nothing -> pure unit
      Just body -> do
        let
          router = H.hoist (runAppM deployment) Router.component
        app <- runUI router unit body
        Router.startRouting app
