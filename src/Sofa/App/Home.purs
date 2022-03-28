module Sofa.App.Home (Slot, proxy, component) where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "home"
proxy = Proxy

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }

initialState :: forall input. input -> Unit
initialState _ = unit

render :: forall action cs m. Unit -> H.ComponentHTML action cs m
render _ = do
  HH.div_
    [ HH.h1_
        [ HH.text "SOFA" ]
    , HH.p_
        [ HH.text "This is a basic tool to experiment with Smart Spec." ]
    , HH.p_
        [ HH.text "Currently a product catalog visualizer and order form is available."
        , HH.text " Note, both are work in progress."
        ]
    ]