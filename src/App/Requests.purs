-- | Contains various AJAX requests.
module App.Requests (getBuyers, getLegalEntities, getOrders, getProductCatalog, postOrder) where

import Prelude
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable, getJson, getRJson, postRJson)
import Data.SmartSpec (Buyer, LegalEntities, OrderForm, Orders, ProductCatalog)
import Effect.Aff.Class (class MonadAff)

baseUrl :: String
baseUrl = "v1alpha1/examples"

orderingBaseUrl :: String
orderingBaseUrl = "/v1alpha1"

ordersUrl :: String
ordersUrl = orderingBaseUrl <> "/orders"

buyersUrl :: String
buyersUrl = orderingBaseUrl <> "/buyers"

-- | Fetches buyers that match the given query string.
getBuyers :: forall m. MonadAff m => CredentialStore m => String -> m (Loadable (Array Buyer))
getBuyers _query = map (map conv) $ getRJson buyersUrl
  where
  conv :: { buyers :: Array Buyer } -> Array Buyer
  conv { buyers } = buyers

getLegalEntities :: forall m. MonadAff m => m (Loadable LegalEntities)
getLegalEntities = getJson (baseUrl <> "/legalentities.json")

getOrders :: forall m. MonadAff m => CredentialStore m => m (Loadable Orders)
getOrders = getRJson ordersUrl

postOrder :: forall m. MonadAff m => CredentialStore m => OrderForm -> m (Loadable OrderForm)
postOrder orderForm = postRJson ordersUrl orderForm

getProductCatalog :: forall m. MonadAff m => m (Loadable ProductCatalog)
getProductCatalog = getJson (baseUrl <> "/product-catalog.normalized.json")
