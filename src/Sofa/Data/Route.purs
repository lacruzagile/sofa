module Sofa.Data.Route (Route(..), routes, href) where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen.HTML.Properties as HP
import Routing.Match (Match, end, lit, root, str)
import Sofa.Data.SmartSpec (OrderId(..))

data Route
  = Home
  | OrderForm
  | Orders
  | Order OrderId
  | ProductCatalog

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

routes :: Match Route
routes =
  root
    *> ( home
          <|> orderForm
          <|> orders
          <|> order
          <|> productCatalog
      )
  where
  home = Home <$ end

  orderForm = OrderForm <$ lit "order-form" <* end

  orders = Orders <$ lit "orders" <* end

  order = (Order <<< OrderId) <$> (lit "orders" *> str <* end)

  productCatalog = ProductCatalog <$ lit "product-catalog" <* end

rawHref :: Route -> String
rawHref = case _ of
  Home -> "/"
  OrderForm -> "/order-form"
  Orders -> "/orders"
  Order id -> "/orders/" <> show id
  ProductCatalog -> "/product-catalog"

href :: forall r i. Route -> HP.IProp ( href :: String | r ) i
href route = HP.href $ "#" <> rawHref route