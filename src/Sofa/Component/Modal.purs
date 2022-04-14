-- | A pseudo-component providing Nectary style modals.
module Sofa.Component.Modal
  ( defaultInput
  , render
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.Component.Icon as Icon
import Sofa.Css as Css

type Input w i
  = { title :: HH.PlainHTML
    , closeAction :: Maybe (Unit -> i)
    -- ^ The close action to trigger when the close icon is clicked. If
    -- nothing then no close button is added.
    , content :: HH.HTML w i
    }

defaultInput :: forall w i. Input w i
defaultInput =
  { title: HH.text ""
  , closeAction: Nothing
  , content: HH.text ""
  }

render :: forall w i. Input w i -> HH.HTML w i
render input =
  faded
    [ wrapper
        $ [ HH.div [ HP.classes [ Css.c "mb-4", Css.c "flex" ] ]
              [ HH.h3
                  [ HP.class_ (Css.c "grow") ]
                  [ HH.fromPlainHTML input.title ]
              , case input.closeAction of
                  Nothing -> HH.text ""
                  Just act -> renderCloseButton act
              ]
          , input.content
          ]
    ]
  where
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

  renderCloseButton act =
    HH.button
      [ HP.classes
          [ Css.c "cursor-pointer"
          , Css.c "flex"
          , Css.c "items-center"
          , Css.c "justify-center"
          ]
      , HE.onClick $ \_ -> act unit
      ]
      [ Icon.close6 [ Icon.ariaLabel "Close" ] ]
