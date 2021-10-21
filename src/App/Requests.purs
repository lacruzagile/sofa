-- | Contains various AJAX requests.
module App.Requests (getLegalEntities, getOrders, getProductCatalog) where

import Prelude
import Data.Loadable (Loadable, getJson)
import Data.SmartSpec (LegalEntities, Orders, ProductCatalog)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

baseUrl :: String
baseUrl = "v1alpha1/examples"

getLegalEntities :: forall m. MonadAff m => MonadEffect m => m (Loadable LegalEntities)
getLegalEntities = getJson (baseUrl <> "/legalentities.json")

getOrders :: forall m. MonadAff m => MonadEffect m => m (Loadable Orders)
getOrders = getJson (baseUrl <> "/orders.json")

getProductCatalog :: forall m. MonadAff m => MonadEffect m => m (Loadable ProductCatalog)
getProductCatalog = getJson (baseUrl <> "/product-catalog.normalized.json")
