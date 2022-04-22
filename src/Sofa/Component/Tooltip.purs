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
    [ Css.classes [ "flex", "items-center" ] ]
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
    [ Css.classes
        [ "sofa-tooltip-group"
        , "inline-block"
        , "relative"
        , "w-fit"
        ]
    ]
    [ content
    , HH.div
        [ Css.classes
            [ "opacity-0"
            , "sofa-tooltip-group-hover:opacity-100"
            , "absolute"
            , "inset-0"
            , "pointer-events-none"
            , "transition-opacity"
            , "motion-reduce:transition-none"
            ]
        ]
        [ HH.div
            [ Css.classes
                $ case input.orientation of
                    Top -> textTopClasses
                    Left -> textLeftClasses
                    Right -> textRightClasses
            , HP.style $ maybe "" (\w -> "max-width:" <> w) input.width
            ]
            [ HH.text input.text
            , HH.div
                [ Css.classes
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
    [ "sofa-tooltip-arrow"
    ]

  arrowTopClasses =
    arrowBaseClasses
      <> [ "left-1/2"
        , "top-full"
        , "-translate-x-[5px]"
        , if input.inverted then "border-t-stormy-500" else "border-t-snow-600"
        ]

  arrowLeftClasses =
    arrowBaseClasses
      <> [ "-right-[10px]"
        , "top-1/2"
        , "-translate-y-[5px]"
        , if input.inverted then "border-l-stormy-500" else "border-l-snow-600"
        ]

  arrowRightClasses =
    arrowBaseClasses
      <> [ "-left-[10px]"
        , "top-1/2"
        , "-translate-y-[5px]"
        , if input.inverted then "border-r-stormy-500" else "border-r-snow-600"
        ]

  textBaseClasses =
    [ "sofa-tooltip-text"
    , "font-normal"
    , if input.inverted then "bg-stormy-500" else "bg-snow-600"
    , if input.inverted then "text-snow-400" else "text-stormy-500"
    ]

  textTopClasses =
    textBaseClasses
      <> [ "left-1/2"
        , "-top-2"
        , "-translate-x-1/2"
        , "-translate-y-full"
        ]

  textLeftClasses =
    textBaseClasses
      <> [ "right-full"
        , "top-1/2"
        , "-translate-x-4"
        , "-translate-y-1/2"
        ]

  textRightClasses =
    textBaseClasses
      <> [ "left-full"
        , "top-1/2"
        , "translate-x-4"
        , "-translate-y-1/2"
        ]
