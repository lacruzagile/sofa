-- | Contains various AJAX requests.
module App.Requests (getLegalEntities, getOrders, getProductCatalog) where

import Prelude
import Data.Loadable (Loadable, getJson)
import Data.SmartSpec (LegalEntities, Orders, ProductCatalog)
import Effect.Aff (Aff)

baseUrl :: String
baseUrl = "v1alpha1/examples"

getLegalEntities :: Aff (Loadable LegalEntities)
getLegalEntities = getJson $ baseUrl <> "/legalentities.json"

getOrders :: Aff (Loadable Orders)
getOrders = getJson $ baseUrl <> "/orders.json"

getProductCatalog :: Aff (Loadable ProductCatalog)
getProductCatalog = getJson $ baseUrl <> "/product-catalog.normalized.json"
