-- | A "component" providing Nectary style tooltips. This is not an actual
-- | Halogen component since it only has a render function.
module Sofa.Component.Tooltip
  ( Input
  , Orientation(..)
  , contentWithIcon
  , defaultInput
  , render
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Sofa.Component.Icon as Icon
import Sofa.Css as Css

-- | The orientation of the tooltip relative the content. E.g., 'Top' indicates
-- | that the tooltip is shown above the content.
data Orientation
  = Top
  | Left
  | Right

type Input
  = { orientation :: Orientation
    , text :: String
    , width :: Maybe String
    , inverted :: Boolean
    }

defaultInput :: Input
defaultInput =
  { orientation: Top
  , text: ""
  , width: Nothing
  , inverted: false
  }

-- | Some HTML content followed by a tooltip icon.
contentWithIcon ∷ forall w i. HH.HTML w i → HH.HTML w i
contentWithIcon content =
  HH.span
    [ HP.classes [ Css.c "flex", Css.c "items-center" ] ]
    [ content
    , Icon.tooltip
    ]

render ::
  forall slot action.
  Input ->
  HH.HTML slot action ->
  HH.HTML slot action
render input content =
  HH.div
    [ HP.classes
        [ Css.c "sofa-tooltip-group"
        , Css.c "inline-block"
        , Css.c "relative"
        , Css.c "w-fit"
        ]
    ]
    [ content
    , HH.div
        [ HP.classes
            [ Css.c "opacity-0"
            , Css.c "sofa-tooltip-group-hover:opacity-100"
            , Css.c "absolute"
            , Css.c "inset-0"
            , Css.c "pointer-events-none"
            , Css.c "transition-opacity"
            , Css.c "motion-reduce:transition-none"
            ]
        ]
        [ HH.div
            [ HP.classes
                $ case input.orientation of
                    Top -> textTopClasses
                    Left -> textLeftClasses
                    Right -> textRightClasses
            , HP.style $ maybe "" (\w -> "max-width:" <> w) input.width
            ]
            [ HH.text input.text
            , HH.div
                [ HP.classes
                    $ case input.orientation of
                        Top -> arrowTopClasses
                        Left -> arrowLeftClasses
                        Right -> arrowRightClasses
                ]
                []
            ]
        ]
    ]
  where
  arrowBaseClasses =
    [ Css.c "sofa-tooltip-arrow"
    ]

  arrowTopClasses =
    arrowBaseClasses
      <> [ Css.c "left-1/2"
        , Css.c "top-full"
        , Css.c "-translate-x-[5px]"
        , Css.c $ if input.inverted then "border-t-stormy-500" else "border-t-snow-600"
        ]

  arrowLeftClasses =
    arrowBaseClasses
      <> [ Css.c "-right-[10px]"
        , Css.c "top-1/2"
        , Css.c "-translate-y-[5px]"
        , Css.c $ if input.inverted then "border-l-stormy-500" else "border-l-snow-600"
        ]

  arrowRightClasses =
    arrowBaseClasses
      <> [ Css.c "-left-[10px]"
        , Css.c "top-1/2"
        , Css.c "-translate-y-[5px]"
        , Css.c $ if input.inverted then "border-r-stormy-500" else "border-r-snow-600"
        ]

  textBaseClasses =
    [ Css.c "sofa-tooltip-text"
    , Css.c "font-normal"
    , Css.c $ if input.inverted then "bg-stormy-500" else "bg-snow-600"
    , Css.c $ if input.inverted then "text-snow-400" else "text-stormy-500"
    ]

  textTopClasses =
    textBaseClasses
      <> [ Css.c "left-1/2"
        , Css.c "-top-2"
        , Css.c "-translate-x-1/2"
        , Css.c "-translate-y-full"
        ]

  textLeftClasses =
    textBaseClasses
      <> [ Css.c "right-full"
        , Css.c "top-1/2"
        , Css.c "-translate-x-4"
        , Css.c "-translate-y-1/2"
        ]

  textRightClasses =
    textBaseClasses
      <> [ Css.c "left-full"
        , Css.c "top-1/2"
        , Css.c "translate-x-4"
        , Css.c "-translate-y-1/2"
        ]
