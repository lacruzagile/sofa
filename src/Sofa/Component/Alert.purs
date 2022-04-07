-- | A component providing Nectary style alerts. This provides only the
-- | individual alert rendering.
module Sofa.Component.Alert
  ( Alert
  , AlertType(..)
  , closeBtn
  , defaultAlert
  , render
  ) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Web.HTML as H

data AlertType
  = Informative
  | Success
  | Warning
  | Error

instance showAlertType :: Show AlertType where
  show = case _ of
    Informative -> "Informative"
    Success -> "Success"
    Warning -> "Warning"
    Error -> "Error"

type Alert w i
  = { type_ :: AlertType
    , content :: HH.HTML w i
    , classes :: Array H.ClassName
    -- ^ Extra classes to add to wrapper div.
    }

defaultAlert :: forall w i. Alert w i
defaultAlert =
  { type_: Informative
  , content: HH.text ""
  , classes: []
  }

typeBgClass :: AlertType -> HH.ClassName
typeBgClass = case _ of
  Informative -> Css.c "bg-informative-200"
  Success -> Css.c "bg-success-200"
  Warning -> Css.c "bg-warning-200"
  Error -> Css.c "bg-error-200"

renderIcon :: AlertType -> HH.PlainHTML
renderIcon =
  let
    attrs color =
      [ Icon.classes
          [ Css.c "flex-none"
          , Css.c "w-5"
          , Css.c "h-5"
          , Css.c "my-0.5"
          , Css.c color
          ]
      , Icon.ariaHidden true
      ]
  in
    case _ of
      Informative -> Icon.info (attrs "fill-informative-500")
      Success -> Icon.checkCircle (attrs "fill-success-500")
      Warning -> Icon.warning (attrs "fill-warning-500")
      Error -> Icon.error (attrs "fill-raspberry-500")

closeBtn :: forall w i. (Unit -> i) -> HH.HTML w i
closeBtn closeAction =
  HH.button
    [ HP.classes
        [ Css.c "float-right"
        , Css.c "p-4"
        , Css.c "-m-3"
        , Css.c "ml-0"
        ]
    , HE.onClick $ \_ -> closeAction unit
    ]
    [ Icon.close
        [ Icon.classes [ Css.c "w-3.5", Css.c "h-3.5" ]
        , Icon.ariaLabel "Close"
        ]
    ]

render :: forall w i. Alert w i -> HH.HTML w i
render alert =
  HH.div
    [ HP.classes
        ( [ Css.c "w-full"
          , Css.c "flex"
          , Css.c "gap-x-3"
          , Css.c "p-3"
          , Css.c "rounded"
          , Css.c "shadow-md"
          , typeBgClass alert.type_
          ]
            <> alert.classes
        )
    ]
    [ HH.fromPlainHTML (renderIcon alert.type_)
    , HH.div
        [ HP.classes [ Css.c "grow" ] ]
        [ alert.content ]
    ]
