module Sofa.Css
  ( c
  , class_
  , classes
  , cs
  ) where

import Halogen (ClassName(..))
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)

-- | Creates a class name.
c :: String -> ClassName
c = ClassName

-- | Given an array of class name strings create the corresponding array of
-- | class names.
-- |
-- | Note, the implementation relies on `ClassName` being a newtype over
-- | `String`. So an array of class names has the same runtime representation as
-- | an array of class names and we can safely use unsafe coerce.
cs :: Array String -> Array ClassName
cs = unsafeCoerce

-- | Given a string generate the corresponding element `class` property.
class_ :: forall r i. String -> HP.IProp ( class :: String | r ) i
class_ name = HP.class_ (ClassName name)

-- | Given an array of strings generate the corresponding element `class`
-- | property.
classes :: forall r i. Array String -> HP.IProp ( class :: String | r ) i
classes names = HP.classes (cs names)
