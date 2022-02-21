-- | A collection of utility function related to Smart Spec data schemas.
module Data.Schema
  ( getTitle
  , isValidValue
  ) where

import Prelude
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.FoldableWithIndex (allWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.SmartSpec (ConfigSchemaEntry(..), ConfigValue(..))
import Data.String as S
import Data.String.Regex as Re

getTitle :: ConfigSchemaEntry -> Maybe String
getTitle = case _ of
  CseBoolean { title } -> title
  CseInteger { title } -> title
  CseString { title } -> title
  CseRegex { title } -> title
  CseConst { title } -> title
  CseArray { title } -> title
  CseObject { title } -> title
  CseOneOf { title } -> title

-- | Determines whether the given value is valid for the given schema.
isValidValue :: ConfigSchemaEntry -> ConfigValue -> Boolean
isValidValue (CseBoolean _) (CvBoolean _) = true

isValidValue (CseInteger si) (CvInteger i) =
  maybe true (\c -> c <= i) si.minimum -- At least `minimum`.
    && maybe true (\c -> i <= c) si.maximum -- At most `maximum`.
    && (A.null si.enum || A.elem i si.enum) -- Member of `enum`.

isValidValue (CseString si) (CvString i) =
  let
    len = S.length i
  in
    maybe true (\c -> c <= len) si.minLength -- At least `minLength`.
      && maybe true (\c -> len <= c) si.maxLength -- At most `maxLength`.
      && (A.null si.enum || A.elem i si.enum) -- Member of `enum`.

isValidValue (CseRegex si) (CvString i) = case Re.regex si.pattern mempty of
  Left _ -> false -- Some regex syntax error.
  Right re -> Re.test re i

isValidValue (CseConst si) i = i == si.const

isValidValue (CseArray si) (CvArray i) = all (isValidValue si.items) i

isValidValue (CseObject si) (CvObject i) =
  let
    -- There is a corresponding value for the schema property, and that value is
    -- valid.
    go k si' = maybe false (isValidValue si') (Map.lookup k i)
  in
    allWithIndex go si.properties

isValidValue (CseOneOf si) i = A.any (\si' -> isValidValue si' i) si.oneOf

isValidValue _ _ = false
