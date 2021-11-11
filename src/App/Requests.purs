-- | Contains various AJAX requests.
module App.Requests (getLegalEntities, getOrders, getProductCatalog, postOrder) where

import Prelude
import App.Auth (class CredentialStore)
import Data.Loadable (Loadable, getJson, getRJson, postRJson)
import Data.SmartSpec (LegalEntities, OrderForm, Orders, ProductCatalog)
import Effect.Aff.Class (class MonadAff)

baseUrl :: String
baseUrl = "v1alpha1/examples"

orderingBaseUrl :: String
orderingBaseUrl = "/v1alpha1"

ordersUrl :: String
ordersUrl = orderingBaseUrl <> "/orders"

getLegalEntities :: forall m. MonadAff m => m (Loadable LegalEntities)
getLegalEntities = getJson (baseUrl <> "/legalentities.json")

getOrders :: forall m. MonadAff m => CredentialStore m => m (Loadable Orders)
getOrders = getRJson ordersUrl

postOrder :: forall m. MonadAff m => CredentialStore m => OrderForm -> m (Loadable OrderForm)
postOrder orderForm = postRJson ordersUrl orderForm

getProductCatalog :: forall m. MonadAff m => m (Loadable ProductCatalog)
getProductCatalog = getJson (baseUrl <> "/product-catalog.normalized.json")
