module Data.Deployment (Deployment(..), SalesforceData(..), detectDeployment) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
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
detectDeployment = maybe Standard Salesforce <$> sfData Just Nothing
