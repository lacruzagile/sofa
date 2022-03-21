module Sofa.Component.Icon
  ( close
  , sinchLogo
  , textWithTooltip
  , tooltip
  ) where

import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Safe.Coerce (coerce)
import Sofa.Css as Css

-- | A plain text followed by a tooltip icon.
textWithTooltip :: forall w i. String -> HH.HTML w i
textWithTooltip text =
  HH.span
    [ HP.classes [ Css.c "flex", Css.c "items-center" ] ]
    [ HH.text text, tooltip ]

-- | A tooltip indication icon. Rendered as a question mark inside a circle.
tooltip :: forall w i. HH.HTML w i
tooltip =
  svg
    [ viewBox "0 0 16 16"
    , classes
        [ Css.c "inline-block"
        , Css.c "fill-current"
        , Css.c "ml-1"
        , Css.c "w-4"
        , Css.c "h-4"
        ]
    ]
    [ path [ d "M7.25 12.5h1.5V11h-1.5v1.5ZM8 .5C3.86.5.5 3.86.5 8c0 4.14 3.36 7.5 7.5 7.5 4.14 0 7.5-3.36 7.5-7.5C15.5 3.86 12.14.5 8 .5ZM8 14c-3.308 0-6-2.693-6-6 0-3.308 2.692-6 6-6 3.307 0 6 2.692 6 6 0 3.307-2.693 6-6 6ZM8 3.5a3 3 0 0 0-3 3h1.5C6.5 5.675 7.175 5 8 5s1.5.675 1.5 1.5c0 1.5-2.25 1.313-2.25 3.75h1.5C8.75 8.562 11 8.375 11 6.5a3 3 0 0 0-3-3Z" ]
    ]

close :: forall w i. HH.HTML w i
close =
  svg
    [ viewBox "0 0 14 14"
    , classes
        [ Css.c "inline-block"
        , Css.c "fill-current"
        , Css.c "w-3"
        , Css.c "h-3"
        ]
    ]
    [ path
        [ d "M13.3.71a.996.996 0 0 0-1.41 0L7 5.59 2.11.7A.996.996 0 1 0 .7 2.11L5.59 7 .7 11.89a.996.996 0 1 0 1.41 1.41L7 8.41l4.89 4.89a.996.996 0 1 0 1.41-1.41L8.41 7l4.89-4.89c.38-.38.38-1.02 0-1.4Z"
        ]
    ]

sinchLogo :: forall w i. HH.HTML w i
sinchLogo =
  svg
    [ viewBox "0 0 200 130.31"
    , classes [ Css.c "h-6" ]
    ]
    [ path [ d "M199.13,66.25a36.2,36.2,0,0,1-23.46,34.22c-12.17,4.75-26.43,3-40.18-4.77l-10-5.76a35.44,35.44,0,0,1-7.68,6.61l-32,18.72-.06,0V99.12l.06,0L133.9,70.93a35.17,35.17,0,0,1-1.54,6.91l10,5.72c12.84,7.3,22.42,6.15,28.19,3.9a22.78,22.78,0,0,0,13.47-14,23.23,23.23,0,0,0,1.1-7.14,22.24,22.24,0,0,0-14.82-20.94c-5.81-2.19-15.39-3.23-28.14,4.23L64.08,95.32l-.09.06c-8.66,5.07-17.56,7.68-26,7.73a39.15,39.15,0,0,1-14.09-2.47A36.48,36.48,0,0,1,1.75,55.24,36.88,36.88,0,0,1,23.46,32.48c12.17-4.75,26.43-3.06,40.18,4.77l10,5.76a35.26,35.26,0,0,1,7.69-6.61L82,36l3.73-2.18-15.9-9A7,7,0,0,1,76.7,12.67L99.55,25.74l22.39-13A7,7,0,0,1,129,24.77L65.29,61.55a35.26,35.26,0,0,1,1.48-6.44l-10-5.72c-12.83-7.3-22.4-6.15-28.18-3.89a22.38,22.38,0,0,0,.25,42.06c5.8,2.19,15.39,3.23,28.14-4.23l78.21-45.77c8.66-5.07,17.56-7.67,26-7.72a39.14,39.14,0,0,1,14.08,2.47,36.22,36.22,0,0,1,23.87,33.94" ]
    ]

type CoreAttributes r
  = ( id :: String, "class" :: String, style :: String | r )

type SVGsvg
  = CoreAttributes ( viewBox :: String )

type SVGpath
  = CoreAttributes ( d :: String )

svgElement ::
  forall r w i.
  HH.ElemName ->
  Array (HH.IProp r i) ->
  Array (HH.HTML w i) ->
  HH.HTML w i
svgElement = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg")

svg :: forall w i. HH.Node SVGsvg w i
svg = svgElement (HH.ElemName "svg")

path :: forall w i. HH.Leaf SVGpath w i
path props = svgElement (HH.ElemName "path") props []

viewBox :: forall r i. String -> HH.IProp ( viewBox :: String | r ) i
viewBox = HH.attr (HH.AttrName "viewBox")

d :: forall r i. String -> HH.IProp ( d :: String | r ) i
d = HH.attr (HH.AttrName "d")

classes :: forall r i. Array HH.ClassName -> HH.IProp ( "class" :: String | r ) i
classes cs = HH.attr (HH.AttrName "class") (joinWith " " (coerce cs))
