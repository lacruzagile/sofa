module Data.Route (Route(..), routes, href) where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Match (Match, root, end, lit)
import Halogen.HTML.Properties as HP

data Route
  = Home
  | SolVis

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routes :: Match Route
routes = root *> (home <|> solvis)
  where
  home = pure Home <* end

  solvis = SolVis <$ lit "solvis" <* end

rawHref :: Route -> String
rawHref = case _ of
  Home -> "/"
  SolVis -> "/solvis"

href :: forall r i. Route -> HP.IProp ( href :: String | r ) i
href route = HP.href $ "#" <> rawHref route
