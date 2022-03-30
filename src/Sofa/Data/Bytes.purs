module Sofa.Data.Bytes where

import Prelude
import Data.Foldable (findMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toStringWith)
import Data.Number.Format as Format

type Bytes
  = Number

-- | Shows a pretty string for the given bytes. Allows a non-exact
-- | representation rounded to one decimal.
showPretty :: Bytes -> String
showPretty bytes =
  fromMaybe ""
    $ findMap (\f -> f bytes)
    $ [ go "GiB" (1024.0 * 1024.0 * 1024.0)
      , go "MiB" (1024.0 * 1024.0)
      , go "KiB" 1024.0
      , go "B" 1.0
      ]
  where
  format = toStringWith (Format.fixed 1)

  go u m n
    | m <= n = Just (format (n / m) <> " " <> u)
    | otherwise = Nothing
