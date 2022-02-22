module Main where

import Prelude
import App (runAppM)
import App.OrderForm as OrderForm
import App.Router as Router
import Data.Deployment (Deployment, detectDeployment, getCrmQuoteId)
import Data.Maybe (Maybe(..))
import Data.SmartSpec (CrmQuoteId)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement as Html

main :: Effect Unit
main = do
  deployment <- detectDeployment
  mCrmQuoteId <- getCrmQuoteId
  HA.runHalogenAff do
    awaitLoad
    mBody <- HA.selectElement (QuerySelector "#sofa-app")
    case mBody of
      Nothing -> pure unit
      Just body -> case mCrmQuoteId of
        Nothing -> runFull deployment body
        Just crmQuoteId -> runOnlyOrderForm deployment crmQuoteId body

runFull :: Deployment -> Html.HTMLElement -> Aff Unit
runFull deployment body = do
  let
    router = H.hoist (runAppM deployment) Router.component
  app <- runUI router unit body
  Router.startRouting app

runOnlyOrderForm :: Deployment -> CrmQuoteId -> Html.HTMLElement -> Aff Unit
runOnlyOrderForm deployment crmQuoteId body =
  let
    router = H.hoist (runAppM deployment) OrderForm.component
  in
    void $ runUI router (OrderForm.ExistingCrmQuoteId crmQuoteId) body
