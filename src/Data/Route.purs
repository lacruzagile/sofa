module Data.Route (Route(..), routes, href) where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (catMaybes)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Halogen.HTML.Properties as HP
import JSURI (encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Routing.Match (Match, end, lit, param, root)

data Route
  = Home
  | OrderForm
  | ProductCatalog
    { catalogUri :: Maybe String
    }

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routes :: Match Route
routes = root *> (home <|> orderForm <|> productCatalog)
  where
  home = Home <$ end

  orderForm = OrderForm <$ lit "order-form" <* end

  productCatalog = ProductCatalog <$> (prodCat1 <|> prodCat2)

  prodCat1 = { catalogUri: Nothing } <$ lit "product-catalog" <* end

  prodCat2 = (\u -> { catalogUri: Just u }) <$> (lit "product-catalog" *> param "url" <* end)

rawHref :: Route -> String
rawHref = case _ of
  Home -> "/"
  OrderForm -> "/order-form"
  ProductCatalog s -> "/product-catalog" <> prodCatParams s
  where
  prodCatParams s = params $ catMaybes [ Tuple "url" <$> s.catalogUri ]

  params :: Array (Tuple String String) -> String
  params [] = ""

  params ps = "?" <> joinWith "&" (map (\(Tuple k v) -> k <> "=" <> uriEncode v) ps)

  uriEncode s = unsafePartial $ fromJust $ encodeURIComponent s

href :: forall r i. Route -> HP.IProp ( href :: String | r ) i
href route = HP.href $ "#" <> rawHref route
