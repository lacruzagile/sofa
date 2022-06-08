module Sofa.Data.Deployment
  ( Deployment(..)
  , SalesforceData(..)
  , detectDeployment
  , getCrmQuoteId
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Sofa.Data.SmartSpec as SS

-- | The kind of deployment SOFA is currently running from.
-- |
-- | - `Standard` indicates that we're running locally or in Kubernetes.
-- |
-- | - `Salesforce` indicates that we're running in Salesforce.
data Deployment
  = Standard
  | Salesforce SalesforceData

-- | The fields injected by the Salesforce deployment. These injected fields are
-- | indicated by the `{!$xyz}` tags in the `salesforce/sofaPage.page` file.
type SalesforceData
  = { accessToken :: String
    , organizationId :: String
    , userId :: String
    , userEmail :: String
    , crmQuoteId :: Maybe SS.CrmQuoteId
    }

foreign import sfData ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  Effect (Maybe SalesforceData)

-- | Figures out which type of deployment we're running.
detectDeployment :: Effect Deployment
detectDeployment = do
  maybe Standard Salesforce <$> sfData Just Nothing

-- | Attempts to retrieve the quote identifier from the SOFA runtime context.
-- | Basically, when non-nothing then we are running inside a quotation inside
-- | Salesforce.
getCrmQuoteId :: Effect (Maybe SS.CrmQuoteId)
getCrmQuoteId = do
  deployment <- detectDeployment
  case deployment of
    Standard -> pure Nothing
    Salesforce { crmQuoteId } -> pure crmQuoteId
