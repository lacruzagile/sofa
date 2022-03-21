module Sofa.Data.DateTimeUtils
  ( toIsoString
  , fromIsoString
  ) where

import Prelude
import Data.DateTime (DateTime)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))

foreign import _toIsoString :: JSDate -> String

foreign import _fromIsoString :: (JSDate -> Maybe JSDate) -> Maybe JSDate -> String -> Maybe JSDate

toIsoString :: DateTime -> String
toIsoString = _toIsoString <<< JSDate.fromDateTime

fromIsoString :: String -> Maybe DateTime
fromIsoString str = JSDate.toDateTime =<< _fromIsoString Just Nothing str
