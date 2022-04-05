-- | A pseudo-component providing Nectary style modals.
module Sofa.Component.Modal
  ( render
  , closeBtn
  ) where

import Prelude
import Data.Array as A
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.Component.Icon as Icon
import Sofa.Css as Css

closeBtn :: forall slot action. (Unit -> action) -> HH.HTML slot action
closeBtn closeAction =
  HH.button
    [ HP.classes
        [ Css.c "cursor-pointer"
        , Css.c "px-3"
        , Css.c "py-3"
        ]
    -- Apparently needed to make the SVG appear completely centered in the
    -- button.
    , HP.style "font-size:0"
    , HE.onClick $ \_ -> closeAction unit
    ]
    [ Icon.close3 [ Icon.ariaLabel "Close" ] ]

render ::
  forall slot action.
  Array (HH.HTML slot action) ->
  HH.HTML slot action ->
  HH.HTML slot action
render toolbarContent body =
  faded
    [ wrapper
        $ [ if A.null toolbarContent then empty else toolbar toolbarContent
          , body
          ]
    ]
  where
  empty = HH.span_ []

  faded =
    HH.div
      [ HP.classes
          [ Css.c "fixed"
          , Css.c "inset-0"
          , Css.c "w-full"
          , Css.c "h-full"
          , Css.c "overflow-y-auto"
          , Css.c "z-10"
          , Css.c "bg-black/60"
          , Css.c "flex"
          ]
      ]

  wrapper =
    HH.div
      [ HP.classes
          [ Css.c "mx-auto"
          , Css.c "my-auto"
          , Css.c "p-8"
          , Css.c "bg-white"
          , Css.c "shadow-md"
          , Css.c "rounded-md"
          ]
      , HPAria.role "dialog"
      , HPAria.modal "true"
      ]

  toolbar =
    HH.div
      [ HP.classes
          [ Css.c "relative"
          , Css.c "float-right"
          , Css.c "flex"
          , Css.c "-m-8"
          ]
      ]
