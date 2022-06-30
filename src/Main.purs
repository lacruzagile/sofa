module Main (main) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Sofa.App (Env, runAppM)
import Sofa.App.OrderForm as OrderForm
import Sofa.App.SalesforceDebugPage as SalesforceDebugPage
import Sofa.App.Router as Router
import Sofa.App.SsoLoggingIn as SsoLoggingIn
import Sofa.Component.Alerts as Alert
import Sofa.Data.Auth (handleSsoRedirect, mkAuthInstance)
import Sofa.Data.Deployment (Deployment(..), SalesforcePageData, detectDeployment)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement as Html

-- | SOFA application entry point. This will attach the application to an
-- | element with the ID `sofa-app`, for example,
-- |
-- |     <div id="sofa-app"></div>
main :: Effect Unit
main = do
  deployment <- detectDeployment
  authInstance <- mkAuthInstance
  HA.runHalogenAff do
    alertSink <- Alert.mkAlertSink
    let
      env = { deployment, alertSink, authInstance }
    -- Wait for the document to load and find the element that we'll take over.
    HA.awaitLoad
    mBody <- HA.selectElement (QuerySelector "#sofa-app")
    case mBody of
      Nothing ->
        H.liftEffect
          $ Console.error "Could not find 'sofa-app' element to attach."
      Just body -> case deployment of
        Salesforce { crmQuoteId: Just qId } ->
          runOrderForm
            env
            body
            (OrderForm.ExistingCrmQuoteId qId)
        Salesforce { pageData: Just pageData } -> runSalesforceDebugPage env body pageData
        _ -> runFull env body

-- | Start the full standalone SOFA implementation.
runFull :: Env -> Html.HTMLElement -> Aff Unit
runFull env body = do
  -- First handle any SSO redirects.
  { dispose: disposeSsoLoggingIn } <- runUI SsoLoggingIn.component unit body
  runAppM env handleSsoRedirect
  disposeSsoLoggingIn
  -- Then run the actual app.
  let
    router = H.hoist (runAppM env) Router.component
  app <- runUI router unit body
  Router.startRouting app

runOrderForm :: Env -> Html.HTMLElement -> OrderForm.Input -> Aff Unit
runOrderForm env body orderFormInput =
  let
    router = H.hoist (runAppM env) OrderForm.component
  in
    void $ runUI router orderFormInput body

runSalesforceDebugPage :: Env -> Html.HTMLElement -> SalesforcePageData -> Aff Unit
runSalesforceDebugPage env body buyerInput =
  let
    router = H.hoist (runAppM env) SalesforceDebugPage.component
  in
    void $ runUI router buyerInput body
