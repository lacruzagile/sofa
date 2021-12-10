-- | Contains various AJAX requests.
module App.Requests
  ( getBillingAccount
  , getBillingAccounts
  , getBuyer
  , getBuyers
  , getBuyerContacts
  , getLegalEntities
  , getOrders
  , getProductCatalog
  , postOrder
  ) where

import Prelude
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable, getJson, getRJson, postRJson)
import Data.Maybe (fromMaybe)
import Data.SmartSpec (BillingAccount, BillingAccountId(..), Buyer, Contact, CrmAccountId(..), LegalEntity, OrderForm, Orders, ProductCatalog)
import Effect.Aff.Class (class MonadAff)
import JSURI (encodeURIComponent)

baseUrl :: String
baseUrl = "/v1alpha1"

ordersUrl :: String
ordersUrl = baseUrl <> "/orders"

buyersUrl :: String
buyersUrl = baseUrl <> "/buyers"

appendPathPiece :: String -> String -> String
appendPathPiece a b = a <> "/" <> b

infixr 5 appendPathPiece as </>

-- | Fetches buyers that match the given query string.
getBuyers :: forall m. MonadAff m => CredentialStore m => String -> m (Loadable (Array Buyer))
getBuyers query =
  map (map conv) $ getRJson
    $ buyersUrl
    <> "?pageSize=5&corporateName="
    <> encodedQuery
  where
  encodedQuery = fromMaybe "" $ encodeURIComponent query

  conv :: { buyers :: Array Buyer } -> Array Buyer
  conv { buyers } = buyers

-- | Fetches a specific buyer that match the given CRM account ID.
getBuyer :: forall m. MonadAff m => CredentialStore m => CrmAccountId -> m (Loadable Buyer)
getBuyer (CrmAccountId id) = getRJson $ buyersUrl </> idEncoded
  where
  idEncoded = fromMaybe "" $ encodeURIComponent id

getBillingAccounts ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  CrmAccountId ->
  m (Loadable (Array BillingAccount))
getBillingAccounts (CrmAccountId id) =
  map (map conv)
    $ getRJson
    $ buyersUrl
    </> idEncoded
    </> "billing-accounts"
  where
  idEncoded = fromMaybe "" $ encodeURIComponent id

  conv :: { billingAccount :: Array BillingAccount } -> Array BillingAccount
  conv { billingAccount } = billingAccount

getBillingAccount ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  CrmAccountId ->
  BillingAccountId ->
  m (Loadable BillingAccount)
getBillingAccount (CrmAccountId aId) (BillingAccountId bId) =
  getRJson
    $ buyersUrl
    </> aIdEncoded
    </> "billing-accounts"
    </> bIdEncoded
  where
  aIdEncoded = fromMaybe "" $ encodeURIComponent aId

  bIdEncoded = fromMaybe "" $ encodeURIComponent bId

getBuyerContacts ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  CrmAccountId ->
  m (Loadable (Array Contact))
getBuyerContacts (CrmAccountId id) =
  map (map conv)
    $ getRJson
    $ buyersUrl
    </> idEncoded
    </> "contacts"
  where
  idEncoded = fromMaybe "" $ encodeURIComponent id

  conv :: { contacts :: Array Contact } -> Array Contact
  conv { contacts } = contacts

getLegalEntities :: forall m. MonadAff m => m (Loadable (Array LegalEntity))
getLegalEntities = map (map conv) $ getJson url
  where
  url = baseUrl </> "examples" </> "legalentities.json"

  conv :: { legalEntities :: Array LegalEntity } -> Array LegalEntity
  conv { legalEntities } = legalEntities

getOrders :: forall m. MonadAff m => CredentialStore m => m (Loadable Orders)
getOrders = getRJson ordersUrl

postOrder :: forall m. MonadAff m => CredentialStore m => OrderForm -> m (Loadable OrderForm)
postOrder orderForm = postRJson ordersUrl orderForm

-- | Fetches the product catalog. Note, we expect a _normalized_ product
-- | catalog.
getProductCatalog :: forall m. MonadAff m => m (Loadable ProductCatalog)
getProductCatalog = getJson url
  where
  -- This URL is "virtual" in the sense that we fetch this URL but the reverse
  -- proxy will redirect the request to the product catalog suitable for the
  -- current deployment.
  url = baseUrl </> "examples" </> "product-catalog.json"
