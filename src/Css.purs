module Css where

import Halogen (ClassName(..))

-- | Creates a class name.
c :: String -> ClassName
c = ClassName

-- SOFA layer classes.
btnRed100 :: ClassName
btnRed100 = ClassName "sofa-btn-red-100"

btnTropical :: ClassName
btnTropical = ClassName "sofa-btn-tropical"

smallTitle :: ClassName
smallTitle = ClassName "sofa-small-title"

spinner :: ClassName
spinner = ClassName "sofa-spinner"
