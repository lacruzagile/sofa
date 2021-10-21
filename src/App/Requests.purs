-- | Contains various AJAX requests.
module App.Requests (getOrders, getProductCatalog) where

import Prelude
import Data.Loadable (Loadable, getJson)
import Data.SmartSpec (Orders, ProductCatalog)
import Effect.Aff (Aff)

baseUrl :: String
baseUrl = "v1alpha1/examples"

getOrders :: Aff (Loadable Orders)
getOrders = getJson $ baseUrl <> "/orders.json"

getProductCatalog :: Aff (Loadable ProductCatalog)
getProductCatalog = getJson $ baseUrl <> "/product-catalog.normalized.json"
