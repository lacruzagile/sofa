module Sofa.Css
  ( c
  , class_
  , classes
  , cs
  , statusColorClass
  ) where

import Halogen (ClassName(..))
import Halogen.HTML.Properties as HP
import Sofa.Data.SmartSpec as SS
import Unsafe.Coerce (unsafeCoerce)

-- | Creates a class name.
c :: String -> ClassName
c = ClassName

-- | Given an array of class name strings create the corresponding array of
-- | class names.
-- |
-- | Note, the implementation relies on `ClassName` being a newtype over
-- | `String`. Specifically, the array of class name strings has the same
-- | runtime representation as an array of class names and we can safely use
-- | unsafe coerce.
cs :: Array String -> Array ClassName
cs = unsafeCoerce

-- | Given a string generate the corresponding element `class` property.
class_ :: forall r i. String -> HP.IProp ( class :: String | r ) i
class_ name = HP.class_ (ClassName name)

-- | Given an array of strings generate the corresponding element `class`
-- | property.
classes :: forall r i. Array String -> HP.IProp ( class :: String | r ) i
classes names = HP.classes (cs names)

statusColorClass :: SS.OrderStatus -> ClassName
statusColorClass status =
  ClassName case status of
    SS.OsInDraft -> "bg-snow-600"
    SS.OsInReview -> "bg-informative-200"
    SS.OsInApproval -> "bg-informative-200"
    SS.OsInSignature -> "bg-informative-200"
    SS.OsInConfiguration -> "bg-informative-200"
    SS.OsInFulfillment -> "bg-informative-200"
    SS.OsFulfilled -> "bg-success-200"
    SS.OsCancelled -> "bg-warning-200"
    SS.OsFailed -> "bg-error-200"
