-- | A collection of utility function related to Smart Spec data schemas.
module Sofa.Data.Schema
  ( getTitle
  , checkValue
  , isValidValue
  ) where

import Prelude
import Data.Array as A
import Data.Either (Either(..), isRight)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.String.Regex as Re
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Sofa.Data.SmartSpec (ConfigSchemaEntry(..), ConfigValue(..))

-- | Tries to extract the title field from the given configuration schema entry.
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
checkValue :: ConfigSchemaEntry -> ConfigValue -> Either String Unit
checkValue (CseBoolean _) (CvBoolean _) = Right unit

checkValue (CseInteger si) (CvInteger i)
  | maybe false (\c -> c > i) si.minimum = Left "integer too small"
  | maybe false (\c -> i > c) si.maximum = Left "integer too large"
  | not (A.null si.enum || A.elem i si.enum) = Left "integer not of allowed value"
  | otherwise = Right unit

checkValue (CseString si) (CvString i) =
  let
    len = S.length i

    matchRegex pat str = case Re.regex pat mempty of
      Left _ -> false -- Some regex syntax error.
      Right re -> Re.test re str
  in
    case 1 of
      _
        | maybe false (\c -> c > len) si.minLength -> Left "string too small"
      _
        | maybe false (\c -> len > c) si.maxLength -> Left "string too long"
      _
        | maybe false (\pat -> not $ matchRegex pat i) si.pattern -> Left "string doesn't match expected pattern"
      _
        | not (A.null si.enum || A.elem i si.enum) -> Left "string not of allowed value"
      _ -> Right unit

checkValue (CseRegex si) (CvString i) = case Re.regex si.pattern mempty of
  Left err -> Left $ "invalid regex syntax in solution file: " <> err
  Right re
    | Re.test re i -> Right unit
    | otherwise -> Left "string doesn't match expected pattern"

checkValue (CseConst si) i
  | i /= si.const = Left "constant value does not match"
  | otherwise = Right unit

checkValue (CseArray si) (CvArray i) =
  let
    checked = A.mapWithIndex (\idx e -> Tuple idx (checkValue si.items e)) i

    mkError = case _ of
      Tuple idx (Left err) -> Just $ err <> " at position " <> show (idx + 1)
      _ -> Nothing

    foundError = A.findMap mkError checked
  in
    case foundError of
      Nothing -> Right unit
      Just err -> Left err

checkValue (CseObject si) (CvObject i) =
  let
    doCheck (Tuple k si') = case Map.lookup k i of
      Just i' -> case checkValue si' i' of
        Left err -> Just $ err <> " at key " <> k
        _ -> Nothing
      _ -> Just $ "missing value at key " <> k
  in
    maybe (Right unit) Left
      $ A.findMap doCheck
      $ FO.toUnfoldable si.properties

checkValue (CseOneOf si) i
  | A.any (\si' -> isRight $ checkValue si' i) si.oneOf = Right unit
  | otherwise = Left "no matching alternative value"

checkValue _ _ = Left "incompatible types"

isValidValue :: ConfigSchemaEntry -> ConfigValue -> Boolean
isValidValue cse cv = isRight $ checkValue cse cv
