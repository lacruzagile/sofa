-- | Contains various AJAX requests.
module App.Requests (getLegalEntities, getOrders, getProductCatalog, postOrder) where

import Prelude
import Data.Loadable (Loadable, getJson, getRJson, postRJson)
import Data.SmartSpec (LegalEntities, OrderForm, Orders, ProductCatalog)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

baseUrl :: String
baseUrl = "v1alpha1/examples"

orderingBaseUrl :: String
orderingBaseUrl = "/v1alpha1"

ordersUrl :: String
ordersUrl = orderingBaseUrl <> "/orders"

token :: String
token = "ey…n0"

getLegalEntities :: forall m. MonadAff m => MonadEffect m => m (Loadable LegalEntities)
getLegalEntities = getJson (baseUrl <> "/legalentities.json")

getOrders :: forall m. MonadAff m => MonadEffect m => m (Loadable Orders)
getOrders = getRJson token ordersUrl

postOrder :: forall m. MonadAff m => MonadEffect m => OrderForm -> m (Loadable OrderForm)
postOrder orderForm = postRJson token ordersUrl orderForm

getProductCatalog :: forall m. MonadAff m => MonadEffect m => m (Loadable ProductCatalog)
getProductCatalog = getJson (baseUrl <> "/product-catalog.normalized.json")
