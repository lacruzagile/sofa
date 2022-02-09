module Css (c) where

import Halogen (ClassName(..))

-- | Creates a class name.
c :: String -> ClassName
c = ClassName
