module Sofa.App.Home (Slot, proxy, component) where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Sofa.Css as Css
import Sofa.Data.Route as Route
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
    , HH.div [ Css.classes [ "bg-white", "p-3" ] ]
        [ HH.p [ Css.class_ "my-2" ]
            [ HH.text "This is a basic tool to experiment with Smart Spec." ]
        , HH.p [ Css.class_ "my-2" ]
            [ HH.text "Currently a product catalog visualizer and order form is available."
            , HH.text " Note, both are work in progress."
            ]
        ]
    ,  HH.div
        [ Css.classes
            [ "my-5"
            , "flex"
            , "flex-wrap"
            , "items-center"
            , "gap-4"
            ]
        ]
        [ HH.a
          [ Route.href Route.Orders
          , Css.class_ "nectary-btn-primary"
          ]
          [ HH.text "Go to order list" ]
        ]
    
    ]
