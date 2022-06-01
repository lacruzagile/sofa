-- | A "component" providing a Nectary style spinner. This is not an actual
-- | Halogen component since it only has a render function.
module Sofa.Component.Spinner (render) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Sofa.Css as Css

render :: forall w i. Array HH.ClassName -> HH.HTML w i
render classes = HH.div [ HP.classes $ spinnerClasses <> classes ] []
  where
  spinnerClasses =
    Css.cs
      [ "inline-block"
      , "border-4"
      , "border-snow-700"
      , "border-b-stormy-500"
      , "rounded-full"
      , "animate-spin"
      , "w-5"
      , "h-5"
      ]
