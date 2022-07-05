module Sofa.Data.Iso3166
  ( countries
  , countryForCode
  ) where

import Prelude
import Data.Array (find)
import Data.Array as A
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

countries :: Array Iso31661Entry
countries = A.sortWith _.name iso31661

countryForCode :: String -> Maybe Iso31661Entry
countryForCode countryAlpha2 = find codeMatch countries
  where
  codeMatch country = country.alpha2 == countryAlpha2
