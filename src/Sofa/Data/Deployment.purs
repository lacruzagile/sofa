module Sofa.Data.Deployment
  ( Deployment(..)
  , SalesforceData(..)
  , detectDeployment
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
-- |
-- | - `crmQuoteId` - when SOFA runs inside a CRM quotation then this is set to
-- |  the quote ID
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
