module Data.Iso3166
  ( countries
  , countryForCode
  , countrySubdivisions
  , subdivisionForCode
  , subdivisions
  ) where

import Prelude
import Data.Array (find)
import Data.Array as A
import Data.List.Lazy (List, filter)
import Data.List.Lazy as List
import Data.Maybe (Maybe)

type Iso31661Entry
  = { name :: String
    , state :: String
    , alpha2 :: String
    , alpha3 :: String
    , numeric :: String
    }

type Iso31662Entry
  = { code :: String
    , name :: String
    , parent :: String
    }

foreign import iso31661 :: Array Iso31661Entry

foreign import iso31662 :: Array Iso31662Entry

countries :: Array Iso31661Entry
countries = A.sortWith _.name iso31661

subdivisions :: Array Iso31662Entry
subdivisions = A.sortWith _.name iso31662

countryForCode :: String -> Maybe Iso31661Entry
countryForCode countryAlpha2 = find codeMatch countries
  where
  codeMatch country = country.alpha2 == countryAlpha2

-- | Find all subdivisions for country with the given code.
countrySubdivisions :: String -> List Iso31662Entry
countrySubdivisions countryAlpha2 =
  filter (\subdiv -> subdiv.parent == countryAlpha2)
    $ List.fromFoldable subdivisions

subdivisionForCode :: String -> String -> Maybe Iso31662Entry
subdivisionForCode countryCode subdivCode = find codeMatch subdivisions
  where
  code = countryCode <> "-" <> subdivCode

  codeMatch subdiv = subdiv.code == code
