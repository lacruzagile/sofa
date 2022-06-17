-- | A pseudo-component providing Nectary style modals.
module Sofa.Component.Modal
  ( defaultInput
  , render
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Web.UIEvent.MouseEvent (MouseEvent)

type Input w i
  = { title :: HH.PlainHTML
    , closeAction :: Maybe (MouseEvent -> i)
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
        $ [ HH.div [ Css.classes [ "mb-4", "flex" ] ]
              [ HH.h3
                  [ Css.class_ "grow" ]
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
      [ Css.classes
          [ "fixed"
          , "inset-0"
          , "w-full"
          , "h-full"
          , "overflow-y-auto"
          , "z-10"
          , "bg-black/60"
          , "flex"
          ]
      ]

  wrapper =
    HH.div
      [ Css.classes
          [ "mx-auto"
          , "my-auto"
          , "p-8"
          , "bg-white"
          , "shadow-md"
          , "rounded-md"
          ]
      , HPAria.role "dialog"
      , HPAria.modal "true"
      ]

  renderCloseButton act =
    HH.button
      [ Css.classes
          [ "cursor-pointer"
          , "flex"
          , "items-center"
          , "justify-center"
          ]
      , HE.onClick act
      ]
      [ Icon.close6 [ Icon.ariaLabel "Close" ] ]
