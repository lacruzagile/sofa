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
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, jsonParser, printJsonDecodeError, (.:))
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
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
    , pageData :: Maybe SalesforcePageData
    }

data SalesforcePageData
  = SfPageOrderForm
    { buyer :: SS.Buyer
    , contacts :: Array SS.Contact
    , billingAccountId :: String
    }
  | SfPageCustomerOrderList { crmAccountId :: SS.CrmAccountId }
  | SfPageUserOrderList

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
      -- Note the name change, billingAccountId is the naming used in SOFA and
      -- Smart Spec so best to keep that.
      billingAccountId <- o .: "platformAccountId"
      pure
        $ SfPageOrderForm
            { buyer:
                SS.Buyer
                  { buyerId: Nothing
                  , crmAccountId
                  , address
                  , contacts:
                      { primary: SS.emptyContact
                      , finance: SS.emptyContact
                      }
                  , existingCustomer: true
                  , corporateName
                  , taxId
                  , website
                  , registrationNr
                  }
            , contacts
            , billingAccountId
            }

    decodeCustomerOrderList o = do
      crmAccountId <- o .: "crmAccountId"
      pure $ SfPageCustomerOrderList { crmAccountId }

    decodeUserOrderList _ = pure $ SfPageUserOrderList

foreign import getSfData ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  (String -> Effect SalesforcePageData) ->
  Effect (Maybe SalesforceData)

parseSalesforceData :: String -> Effect SalesforcePageData
parseSalesforceData pageDataStr =
  either handleError pure do
    json <- jsonParser pageDataStr
    bimap printJsonDecodeError identity $ decodeJson json
  where
  handleError :: forall a. String -> Effect a
  handleError = throwException <<< Exception.error

-- | Figures out which type of deployment we're running. This action should only
-- | be run once and very early in the app execution.
detectDeployment :: Effect Deployment
detectDeployment = do
  maybe Standard Salesforce
    <$> getSfData Just Nothing parseSalesforceData
