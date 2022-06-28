module Sofa.Data.Deployment
  ( Deployment(..)
  , SalesforceData(..)
  , SalesforcePageData(..)
  , class MonadDeployment
  , detectDeployment
  , getDeployment
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson, printJsonDecodeError, (.:))
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import Effect.Exception as Exception
import Sofa.Data.SmartSpec as SS

-- | The kind of deployment SOFA is currently running from.
-- |
-- | - `Standard` indicates that we're running locally or in Kubernetes.
-- |
-- | - `Salesforce` indicates that we're running in Salesforce.
data Deployment
  = Standard
  | Salesforce SalesforceData

class MonadDeployment m where
  getDeployment :: m Deployment

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
    , pageData :: AVar SalesforcePageData
    }

data SalesforcePageData
  = SfPageOrderForm
    { crmAccountId :: SS.CrmAccountId
    , corporateName :: String
    , taxId :: String
    , website :: SS.Uri
    , registrationNr :: String
    , address :: SS.Address
    , contacts :: Array SS.Contact
    , platformAccountId :: String
    }
  | SfPageCustomerOrderList { crmAccountId :: SS.CrmAccountId }
  | SfPageUserOrderList

derive instance genericSalesforcePageData :: Generic SalesforcePageData _

instance showSalesforcePageData :: Show SalesforcePageData where
  show = genericShow

instance decodeJsonSalesforcePageData :: DecodeJson SalesforcePageData where
  decodeJson json = do
    o <- decodeJson json
    page <- o .: "page"
    case page of
      "ORDER_FORM" -> decodeOrderForm o
      "ORDER_LIST" -> decodeCustomerOrderList o <|> decodeUserOrderList o
      _ -> Left (TypeMismatch "SalesforcePageData")
    where
    decodeOrderForm o = do
      crmAccountId <- o .: "crmAccountId"
      corporateName <- o .: "corporateName"
      taxId <- o .: "taxId"
      website <- o .: "website"
      registrationNr <- o .: "registrationNr"
      address <- o .: "address"
      contacts <- o .: "contacts"
      platformAccountId <- o .: "platformAccountId"
      pure
        $ SfPageOrderForm
            { crmAccountId
            , corporateName
            , taxId
            , website
            , registrationNr
            , address
            , contacts
            , platformAccountId
            }

    decodeCustomerOrderList o = do
      crmAccountId <- o .: "crmAccountId"
      pure $ SfPageCustomerOrderList { crmAccountId }

    decodeUserOrderList _ = pure $ SfPageUserOrderList

foreign import getSfData ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  AVar SalesforcePageData ->
  Effect (Maybe SalesforceData)

-- | Attaches the given `populateSalesforceData` function on the `window`
-- | object.
foreign import attachPopulateSalesforceData ::
  (Json -> Effect Unit) ->
  Effect Unit

populateSalesforceData :: AVar SalesforcePageData -> Json -> Effect Unit
populateSalesforceData avar jsonPageData = do
  let
    ePageData = decodeJson jsonPageData
  case ePageData of
    Left err -> throwException $ Exception.error $ printJsonDecodeError err
    Right pageData -> void $ AVar.put pageData avar (either throwException pure)

-- | Figures out which type of deployment we're running. This action should only
-- | be run once and very early in the app execution.
detectDeployment :: Effect Deployment
detectDeployment = do
  sfPageData <- AVar.empty
  mSfData <- getSfData Just Nothing sfPageData
  case mSfData of
    Nothing -> pure Standard
    Just sfData -> do
      attachPopulateSalesforceData (populateSalesforceData sfPageData)
      pure $ Salesforce sfData
