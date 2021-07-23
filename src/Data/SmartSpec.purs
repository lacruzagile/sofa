module Data.SmartSpec
  ( Currency
  , Price
  , Solution
  , BillingUnit
  ) where

import Data.Maybe (Maybe)

type Solution
  = { description :: String
    , products :: Array String
    , prices :: Array Price
    , billingUnits :: Array BillingUnit
    }

type Currency
  = { code :: String }

type Price
  = { name :: String
    , currency :: Currency
    , rateCardPathPrefix :: String
    , rateCards :: Array String
    }

type BillingUnit
  = { id :: String
    , chargeType :: String
    , description :: Maybe String
    }
