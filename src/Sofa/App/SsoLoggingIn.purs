module Sofa.App.SsoLoggingIn (Slot, proxy, component) where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Sofa.Component.Spinner as Spinner
import Sofa.Css as Css
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "ssoLoggingIn"
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
render _ =
  HH.div
    [ Css.classes
        [ "w-full"
        , "h-screen"
        , "grid"
        , "gap-4"
        , "place-content-center"
        , "place-items-center"
        ]
    ]
    [ Spinner.render (Css.cs [ "w-12", "h-12" ])
    , HH.text "Logging in"
    ]
