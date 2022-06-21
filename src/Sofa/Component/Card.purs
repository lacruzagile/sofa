-- | A "component" providing Nectary style cards. This is not an actual Halogen
-- | component since it only has a render function.
module Sofa.Component.Card
  ( InputBase
  , InputButton
  , InputRadio
  , renderButton
  , renderRadio
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Css as Css
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent)

type InputBase slot action
  = ( title :: HH.HTML slot action
    , body :: Array (HH.HTML slot action)
    )

type InputPlain slot action
  = { | InputBase slot action }

type InputButton slot action
  = { onClick :: MouseEvent -> action
    | InputBase slot action
    }

type InputRadio slot action
  = { name :: String
    , selected :: Boolean
    , enabled :: Boolean
    , onChange :: Event -> action
    | InputBase slot action
    }

render ::
  forall slot action r.
  { body :: Array (HH.HTML slot action)
  , mInput :: Maybe (HH.HTML slot action)
  , title :: HH.HTML slot action
  , wrapper ::
      Array (HP.IProp ( class :: String | r ) action) ->
      Array (HH.HTML slot action) ->
      HH.HTML slot action
  , wrapperClasses :: Array String
  } ->
  HH.HTML slot action
render { wrapper, wrapperClasses, mInput, title, body } =
  wrapper
    [ Css.classes
        $ [ "nectary-btn-secondary"
          , "flex-wrap"
          , "cursor-pointer"
          , "text-left"
          , "text-stormy-500"
          , "h-auto"
          , "py-4"
          , "px-5"
          , "content-start"
          ]
        <> wrapperClasses
    ]
    $ [ HH.div [ Css.classes [ "text-xl", "grow" ] ] [ title ]
      , case mInput of
          Nothing -> HH.text ""
          Just input -> input
      , case body of
          [] -> HH.text ""
          b ->
            HH.div
              [ Css.classes [ "mt-5", "w-full", "font-normal", "self-start" ] ]
              b
      ]

renderButton ::
  forall slot action.
  InputButton slot action ->
  HH.HTML slot action
renderButton input =
  render
    { wrapper: \props -> HH.button (props <> [ HE.onClick input.onClick ])
    , wrapperClasses: []
    , mInput: Nothing
    , title: input.title
    , body: input.body
    }

renderRadio ::
  forall slot action.
  InputRadio slot action ->
  HH.HTML slot action
renderRadio input =
  render
    { wrapper: HH.label
    , wrapperClasses:
        [ if input.selected then
            "ring-tropical-500"
          else
            "ring-snow-700"
        ]
    , mInput:
        Just
          $ HH.input
              [ HP.type_ HP.InputRadio
              , HP.name input.name
              , Css.class_ "nectary-input-radio"
              , HP.checked input.selected
              , HP.enabled input.enabled
              , HE.onChange input.onChange
              ]
    , title: input.title
    , body: input.body
    }
