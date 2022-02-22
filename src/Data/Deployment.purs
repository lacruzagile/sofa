module Data.Deployment
  ( Deployment(..)
  , SalesforceData(..)
  , detectDeployment
  , getCrmQuoteId
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.SmartSpec as SS
import Effect (Effect)

-- | The kind of deployment used. The standard is used, e.g., when running
-- | locally or in Kubernetes. In the future `Salesforce` will be added.
data Deployment
  = Standard
  | Salesforce SalesforceData

type SalesforceData
  = { accessToken :: String
    , organizationId :: String
    , userId :: String
    , userEmail :: String
    }

foreign import sfData ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  Effect (Maybe SalesforceData)

-- | Figures out which type of deployment we're running.
detectDeployment :: Effect Deployment
detectDeployment = do
  -- Commented out to force use of standard login even in Salesforce
  -- deployments:
  -- maybe Standard Salesforce <$> sfData Just Nothing
  pure Standard

foreign import _getCrmQuoteId ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  Effect (Maybe String)

-- | Attempts to retrieve the quote identifier from the SOFA runtime context.
-- | Basically, when non-nothing then we are running inside a quotation inside
-- | Salesforce.
getCrmQuoteId :: Effect (Maybe SS.CrmQuoteId)
getCrmQuoteId = map SS.CrmQuoteId <$> _getCrmQuoteId Just Nothing
