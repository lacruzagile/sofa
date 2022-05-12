module Main (main) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Sofa.App (Env, runAppM)
import Sofa.App.OrderForm as OrderForm
import Sofa.App.Router as Router
import Sofa.Component.Alerts as Alert
import Sofa.Data.Auth (mkAuthEventEmitter, mkAuthInstance)
import Sofa.Data.Deployment (detectDeployment, getCrmQuoteId)
import Sofa.Data.SmartSpec (CrmQuoteId)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement as Html

-- | SOFA application entry point. This will attach the application to an
-- | element with the ID `sofa-app`, for example,
-- |
-- |     <div id="sofa-app"></div>
main :: Effect Unit
main = do
  deployment <- detectDeployment
  mCrmQuoteId <- getCrmQuoteId
  authEventEmitter <- mkAuthEventEmitter
  authInstance <- mkAuthInstance
  HA.runHalogenAff do
    alertSink <- Alert.mkAlertSink
    let
      env = { deployment, alertSink, authEventEmitter, authInstance }
    awaitLoad
    mBody <- HA.selectElement (QuerySelector "#sofa-app")
    case mBody of
      Nothing -> pure unit
      Just body -> case mCrmQuoteId of
        Nothing -> runFull env body
        Just crmQuoteId -> runOnlyOrderForm env crmQuoteId body

runFull :: Env -> Html.HTMLElement -> Aff Unit
runFull env body = do
  let
    router = H.hoist (runAppM env) Router.component
  app <- runUI router unit body
  Router.startRouting app

runOnlyOrderForm :: Env -> CrmQuoteId -> Html.HTMLElement -> Aff Unit
runOnlyOrderForm env crmQuoteId body =
  let
    router = H.hoist (runAppM env) OrderForm.component
  in
    void $ runUI router (OrderForm.ExistingCrmQuoteId crmQuoteId) body
