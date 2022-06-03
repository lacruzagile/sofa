-- | A "component" providing a Nectary style spinner. This is not an actual
-- | Halogen component since it only has a render function.
module Sofa.Component.Spinner (Input, defaults, render) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Sofa.Css as Css

type Input
  = { size :: Int -- ^ Size as a Tailwind dimension.
    , classes :: Array HH.ClassName
    }

defaults :: Input
defaults =
  { size: 5
  , classes: []
  }

render :: forall w i. Input -> HH.HTML w i
render { size, classes } = HH.div [ HP.classes $ spinnerClasses <> classes ] []
  where
  sizeStr = show size

  spinnerClasses =
    Css.cs
      [ "inline-block"
      , "border-4"
      , "border-snow-700"
      , "border-b-stormy-500"
      , "rounded-full"
      , "animate-spin"
      , "w-" <> sizeStr
      , "h-" <> sizeStr
      ]
