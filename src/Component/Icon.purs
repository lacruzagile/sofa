module Component.Icon
  ( tooltip
  ) where

import Css as Css
import Data.String (joinWith)
import Halogen.HTML as HH
import Safe.Coerce (coerce)

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
