module Main (main) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitLoad)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Sofa.App (runAppM)
import Sofa.App.OrderForm as OrderForm
import Sofa.App.Router as Router
import Sofa.Data.Deployment (Deployment, detectDeployment, getCrmQuoteId)
import Sofa.Data.SmartSpec (CrmQuoteId)
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
