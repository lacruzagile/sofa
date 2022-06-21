module Sofa.App.OrderForm.SelectProduct (Slot, Output(..), proxy, component) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Sofa.Component.Card as Card
import Sofa.Css as Css
import Sofa.Data.SmartSpec (SkuCode)
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "selectProduct"
proxy = Proxy

type Input
  = { name :: String
    , selected :: Maybe SkuCode
    , products :: Array SS.Product
    }

type Output
  = SS.Product

data Action
  = Select SkuCode

type State
  = Input

component ::
  forall query m.
  MonadAff m =>
  H.Component query Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Select sku -> do
    { products } <- H.modify _ { selected = Just sku }
    case A.find (\(SS.Product { sku: sku' }) -> sku == sku') products of
      Nothing -> pure unit
      Just product -> H.raise product

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ Css.classes
        [ "grid"
        , "grid-cols-1"
        , "lg:grid-cols-2"
        , "xl:grid-cols-3"
        , "gap-5"
        ]
    ]
    $ renderProductButton
    <$> state.products
  where
  renderProductButton (SS.Product { sku, title, description }) =
    Card.renderRadio
      { title: HH.text $ fromMaybe (show sku) title
      , body: [ HH.text $ fromMaybe "" description ]
      , name: "prodsel-" <> state.name
      , selected: maybe false (sku == _) state.selected
      , enabled: true
      , onChange: \_ -> Select sku
      }
