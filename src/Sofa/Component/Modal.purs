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
    -- ^ The action to trigger when the close icon is clicked. If nothing then
    -- no close button is added.
    , backgroundClickAction :: Maybe (MouseEvent -> i)
    -- ^ The action to trigger when the modal background is clicked. If nothing
    -- then the default click handler is used.
    , content :: HH.HTML w i
    }

defaultInput :: forall w i. Input w i
defaultInput =
  { title: HH.text ""
  , closeAction: Nothing
  , backgroundClickAction: Nothing
  , content: HH.text ""
  }

render :: forall w i. Input w i -> HH.HTML w i
render input =
  background
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
  bgOnClick = case input.backgroundClickAction of
    Nothing -> []
    Just onClick -> [ HE.onClick onClick ]

  background =
    HH.div
      $ [ Css.classes
            [ "fixed"
            , "inset-0"
            , "w-full"
            , "h-full"
            , "overflow-y-auto"
            , "z-10"
            , "bg-black/60"
            , "flex"
            , "cursor-default"
            ]
        ]
      <> bgOnClick

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
