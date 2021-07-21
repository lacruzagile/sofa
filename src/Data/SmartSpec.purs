module Data.SmartSpec
  ( Currency
  , Price
  , Solution
  ) where

type Solution
  = { description :: String
    , products :: Array String
    , prices :: Array Price
    }

type Currency
  = { code :: String }

type Price
  = { name :: String
    , currency :: Currency
    , rateCardPathPrefix :: String
    , rateCards :: Array String
    }
