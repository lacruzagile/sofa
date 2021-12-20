module Data.Currency
  ( Currency
  , mkCurrency
  , unsafeMkCurrency
  , formatter
  , numberFormatter
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson)
import Data.Either (note')
import Data.Maybe (Maybe(..))
import Data.String.Regex as Re
import Data.String.Regex.Unsafe (unsafeRegex)

foreign import formatter :: Currency -> Number -> String

foreign import numberFormatter :: Number -> String

newtype Currency
  = Currency String

derive instance eqCurrency :: Eq Currency

derive instance ordCurrency :: Ord Currency

instance showCurrency :: Show Currency where
  show (Currency code) = code

instance decodeJsonCurrency :: DecodeJson Currency where
  decodeJson json = do
    s <- decodeJson json
    note' error $ mkCurrency s
    where
    error _ = TypeMismatch "Currency (ISO 4217 alphabetic code)"

derive newtype instance encodeJsonCurrency :: EncodeJson Currency

-- | Constructs a currency value from the given string, which is expected to be
-- | in ISO 4217 format.
mkCurrency :: String -> Maybe Currency
mkCurrency s
  | Re.test (unsafeRegex "^[A-Z]{3}$" mempty) s = Just (Currency s)
  | otherwise = Nothing

-- | Constructs a currency value from the given string, which is expected to be
-- | in ISO 4217 format.
unsafeMkCurrency :: String -> Currency
unsafeMkCurrency = Currency
