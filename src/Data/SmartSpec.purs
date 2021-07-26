module Data.SmartSpec
  ( BillingUnit(..)
  , ChargeType(..)
  , Currency(..)
  , DimType(..)
  , Meta(..)
  , Price(..)
  , Product(..)
  , ProductFeature(..)
  , Solution(..)
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe)

type Meta
  = { solutions :: Array String
    }

type Solution
  = { description :: String
    , dimTypes :: Array DimType
    , products :: Array String
    , prices :: Array Price
    , billingUnits :: Array BillingUnit
    }

type Currency
  = { code :: String }

type Price
  = { name :: String
    , currency :: Currency
    , rateCardPathPrefix :: Maybe String
    , rateCards :: Maybe (Array String)
    }

type BillingUnit
  = { id :: Maybe String
    , name :: Maybe String
    , chargeType :: ChargeType
    , description :: Maybe String
    }

data ChargeType
  = Onetime
  | Monthly
  | Usage

instance showChargeType :: Show ChargeType where
  show = case _ of
    Onetime -> "Onetime"
    Monthly -> "Monthly"
    Usage -> "Usage"

instance encodeJsonChargeType :: EncodeJson ChargeType where
  encodeJson = encodeJson <<< show

instance decodeJsonChargeType :: DecodeJson ChargeType where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Onetime" -> Right Onetime
      "Monthly" -> Right Monthly
      "Usage" -> Right Usage
      _ -> Left (TypeMismatch "ChargeType")

type Product
  = { sku :: String
    , description :: String
    , options :: Maybe (Array String)
    , features :: Maybe (Array ProductFeature)
    }

type ProductFeature
  = { name :: String }

type DimType
  = { id :: Maybe String
    , name :: Maybe String
    , description :: Maybe String
    , schema :: Maybe Json
    }

type UsageSchema
  = { id :: Maybe String
    , name :: Maybe String
    , description :: Maybe String
    , dimTypeRefs :: Maybe (Array String)
    }
