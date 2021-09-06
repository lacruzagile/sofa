module Data.SmartSpec
  ( Address(..)
  , Asset(..)
  , BillingAccountRef(..)
  , BillingOption(..)
  , Charge(..)
  , ChargeType(..)
  , Commercial(..)
  , ConfigSchemaEntry(..)
  , ConfigValue(..)
  , Contact(..)
  , ContractTerm(..)
  , Currency(..)
  , Customer(..)
  , Date(..)
  , DateTime(..)
  , DefaultPriceByUnit(..)
  , DimValue(..)
  , Discount(..)
  , LegalEntity(..)
  , OrderForm(..)
  , OrderLine(..)
  , OrderSection(..)
  , OrderSectionSummary(..)
  , OrderStatus(..)
  , OrderSummary(..)
  , Platform(..)
  , Price(..)
  , PriceBook(..)
  , PriceBookRef(..)
  , PriceByDim(..)
  , PriceByUnit(..)
  , PriceOverride(..)
  , PriceSegmentation(..)
  , PriceSegmentationByUnit(..)
  , PricesPerDimByUnit(..)
  , Product(..)
  , ProductCatalog(..)
  , ProductCategory(..)
  , ProductFeature(..)
  , ProductInstance(..)
  , ProductOption(..)
  , ProductOptionType(..)
  , ProductRef(..)
  , ProductVariable(..)
  , Purchaser(..)
  , Quantifier(..)
  , RateCard(..)
  , ReturnCustomerCommercial(..)
  , ReturnCustomerData(..)
  , Rule(..)
  , RuleConditionExpr(..)
  , RuleStage(..)
  , SalesforceAccountRef(..)
  , Segment(..)
  , SegmentPrice(..)
  , SegmentationModel(..)
  , SegmentationPeriod(..)
  , Seller(..)
  , Severity(..)
  , SimplePrice(..)
  , Sku(..)
  , Solution(..)
  , SpecUnit(..)
  , SpecUnitMap
  , UnitRef(..)
  , Uri(..)
  , Validity(..)
  , productUnits
  , skuCode
  , solutionProducts
  , specUnitLabel
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, (.!=), (.:), (.:?), (:=), (~>))
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO

type Uri
  = String

newtype Solution
  = Solution
  { id :: String
  , uri :: Maybe Uri
  , name :: Maybe String
  , description :: Maybe String
  , products :: Array Product
  , rules :: Array Rule
  , priceBooks :: Array PriceBook
  }

derive instance newtypeSolution :: Newtype Solution _

instance decodeJsonSolution :: DecodeJson Solution where
  decodeJson json = do
    o <- decodeJson json
    id <- o .: "id"
    uri <- o .:? "uri"
    name <- o .:? "name"
    description <- o .:? "description"
    products <- o .: "products"
    rules <- o .:? "rules" .!= []
    priceBooks <- o .: "priceBooks"
    pure $ Solution { id, uri, name, description, rules, products, priceBooks }

instance encodeJsonSolution :: EncodeJson Solution where
  encodeJson (Solution x) = encodeJson x

solutionProducts :: Solution -> Map String Product
solutionProducts =
  Map.fromFoldable
    <<< map (\p@(Product { sku }) -> Tuple sku p)
    <<< _.products
    <<< unwrap

newtype ProductCatalog
  = ProductCatalog
  { name :: Maybe String
  , description :: Maybe String
  , rules :: Array Rule
  , solutions :: Map String Solution
  }

instance decodeJsonProductCatalog :: DecodeJson ProductCatalog where
  decodeJson json = do
    o <- decodeJson json
    name <- o .:? "name"
    description <- o .:? "description"
    rules <- o .:? "rules" .!= []
    solutionsObj :: FO.Object Solution <- o .: "solutions"
    let
      solutions = Map.fromFoldable (FO.toUnfoldable solutionsObj :: Array _)
    pure
      $ ProductCatalog
          { name
          , description
          , rules
          , solutions
          }

data RuleStage
  = SalesOrder
  | ServiceOrder
  | OrderFulfillment

instance showRuleStage :: Show RuleStage where
  show = case _ of
    SalesOrder -> "SalesOrder"
    ServiceOrder -> "ServiceOrder"
    OrderFulfillment -> "OrderFulfillment"

instance decodeJsonRuleStage :: DecodeJson RuleStage where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "SalesOrder" -> Right SalesOrder
      "ServiceOrder" -> Right ServiceOrder
      "OrderFulfillment" -> Right OrderFulfillment
      _ -> Left (TypeMismatch "RuleStage")

instance encodeJsonRuleStage :: EncodeJson RuleStage where
  encodeJson = encodeJson <<< show

data Severity
  = SeverityInfo
  | SeverityWarning
  | SeverityError

instance showSeverity :: Show Severity where
  show = case _ of
    SeverityInfo -> "Info"
    SeverityWarning -> "Warning"
    SeverityError -> "Error"

instance decodeJsonSeverity :: DecodeJson Severity where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Info" -> Right SeverityInfo
      "Warning" -> Right SeverityWarning
      "Error" -> Right SeverityError
      _ -> Left (TypeMismatch "Severity")

instance encodeJsonSeverity :: EncodeJson Severity where
  encodeJson = encodeJson <<< show

data Quantifier
  = QuantifierAll
  | QuantifierAny

instance showQuantifier :: Show Quantifier where
  show = case _ of
    QuantifierAll -> "All"
    QuantifierAny -> "Any"

instance decodeJsonQuantifier :: DecodeJson Quantifier where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "All" -> Right QuantifierAll
      "Any" -> Right QuantifierAny
      _ -> Left (TypeMismatch "Quantifier")

instance encodeJsonQuantifier :: EncodeJson Quantifier where
  encodeJson = encodeJson <<< show

newtype RuleConditionExpr
  = RuleConditionExpr { type_ :: String, expr :: String }

instance decodeJsonRuleConditionExpr :: DecodeJson RuleConditionExpr where
  decodeJson json = do
    o <- decodeJson json
    type_ <- o .: "type"
    expr <- o .: "expr"
    pure $ RuleConditionExpr { type_, expr }

instance encodeJsonRuleConditionExpr :: EncodeJson RuleConditionExpr where
  encodeJson (RuleConditionExpr x) = encodeJson x

newtype Rule
  = Rule
  { severity :: Severity
  , stages :: Array RuleStage
  , quantifier :: Quantifier
  , message :: String
  , conditions :: Array RuleConditionExpr
  }

instance decodeJsonRule :: DecodeJson Rule where
  decodeJson json = Rule <$> decodeJson json

instance encodeJsonRule :: EncodeJson Rule where
  encodeJson (Rule x) = encodeJson x

newtype Currency
  = Currency { code :: String, country :: Maybe String }

derive instance eqCurrency :: Eq Currency

derive instance newtypeCurrency :: Newtype Currency _

instance decodeJsonCurrency :: DecodeJson Currency where
  decodeJson json = plainCode <|> currency
    where
    plainCode = (\code -> Currency { code, country: Nothing }) <$> decodeJson json

    currency = do
      o <- decodeJson json
      code <- o .: "code"
      country <- o .:? "country" .!= Nothing
      pure $ Currency { code, country }

instance encodeJsonCurrency :: EncodeJson Currency where
  encodeJson (Currency x) = case x.country of
    Nothing -> encodeJson x.code
    Just _ -> encodeJson x

newtype PriceBook
  = PriceBook
  { id :: String
  , plan :: String
  , version :: Date
  , description :: Maybe String
  , currency :: Currency
  , rateCards :: Maybe (Array RateCard)
  }

instance decodeJsonPriceBook :: DecodeJson PriceBook where
  decodeJson json = PriceBook <$> decodeJson json

instance encodeJsonPriceBook :: EncodeJson PriceBook where
  encodeJson (PriceBook x) = encodeJson x

-- TODO: Assert non-negative.
data Charge
  = ChargeSimple
    { unit :: UnitRef
    , price :: SimplePrice
    , segmentation :: Maybe PriceSegmentation
    , termOfPriceChangeInDays :: Int
    , monthlyMinimum :: Number
    }
  | ChargeMixed
    { units :: Array UnitRef
    , priceSegmentations :: Array PriceSegmentationByUnit
    , defaultPrices :: Array DefaultPriceByUnit
    , pricesPerDim :: Array PricesPerDimByUnit
    , monthlyMinimum :: Number
    , termOfPriceChangeInDays :: Int
    }
  | ChargeArray (Array Charge)

instance decodeJsonCharge :: DecodeJson Charge where
  decodeJson json = rccElement json <|> rccArray json
    where
    rccElement j = rccSimple j <|> rccMixed j

    rccArray j = ChargeArray <$> decodeArray rccElement j

    rccSimple j = do
      o <- decodeJson j
      unit <- o .: "unit"
      price <- o .: "price"
      segmentation <- o .:? "segmentation"
      termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
      monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
      pure
        $ ChargeSimple
            { unit
            , price
            , segmentation
            , termOfPriceChangeInDays
            , monthlyMinimum
            }

    rccMixed j = do
      o <- decodeJson j
      units <- o .: "units"
      priceSegmentations <- o .:? "priceSegmentations" .!= []
      defaultPrices <- o .:? "defaultPrices" .!= []
      pricesPerDim <- o .: "pricesPerDim"
      monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
      termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
      pure
        $ ChargeMixed
            { units
            , priceSegmentations
            , defaultPrices
            , pricesPerDim
            , monthlyMinimum
            , termOfPriceChangeInDays
            }

instance encodeJsonCharge :: EncodeJson Charge where
  encodeJson = case _ of
    ChargeSimple x -> encodeJson x
    ChargeMixed x -> encodeJson x
    ChargeArray x -> encodeJson x

data SimplePrice
  = SimplePriceSegmented Price
  | SimplePriceByDim (Array PriceByDim)

instance decodeJsonSimplePrice :: DecodeJson SimplePrice where
  decodeJson json = segmented <|> byDim
    where
    segmented = SimplePriceSegmented <$> decodeJson json

    byDim = SimplePriceByDim <$> decodeJson json

instance encodeJsonSimplePrice :: EncodeJson SimplePrice where
  encodeJson (SimplePriceSegmented x) = encodeJson x
  encodeJson (SimplePriceByDim x) = encodeJson x

newtype DimValue
  = DimValue ConfigValue

instance decodeJsonDimValue :: DecodeJson DimValue where
  decodeJson json = DimValue <$> decodeJson json

instance encodeJsonDimValue :: EncodeJson DimValue where
  encodeJson (DimValue x) = encodeJson x

newtype PriceByDim
  = PriceByDim
  { dim :: DimValue
  , price :: Price
  , monthlyMinimum :: Number
  }

instance decodeJsonPriceByDim :: DecodeJson PriceByDim where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    price <- o .: "price"
    monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
    pure $ PriceByDim { dim, price, monthlyMinimum }

instance encodeJsonPriceByDim :: EncodeJson PriceByDim where
  encodeJson (PriceByDim x) = encodeJson x

data Discount
  = DiscountPercentage Number
  | DiscountAbsolute Number

instance decodeJsonDiscount :: DecodeJson Discount where
  decodeJson json = percentage <|> absolute
    where
    percentage = do
      o <- decodeJson json
      perc <- o .: "percentage"
      pure $ DiscountPercentage perc

    absolute = do
      o <- decodeJson json
      amount <- o .: "amount"
      pure $ DiscountAbsolute amount

instance encodeJsonDiscount :: EncodeJson Discount where
  encodeJson (DiscountPercentage x) =
    encodeJson
      { percentage: x
      }
  encodeJson (DiscountAbsolute x) =
    encodeJson
      { amount: x
      }

newtype SegmentPrice
  = SegmentPrice
  { minimum :: Int
  , exclusiveMaximum :: Maybe Int
  , listPrice :: Number
  , salesPrice :: Number
  , discount :: Maybe Discount
  }

instance decodeJsonSegmentPrice :: DecodeJson SegmentPrice where
  decodeJson json = do
    o <- decodeJson json
    minimum <- o .: "minimum"
    exclusiveMaximum <- o .:? "exclusiveMaximum"
    let
      basicPrice = do
        p <- o .: "price"
        pure { listPrice: p, salesPrice: p, discount: Nothing }

      discountedPrice = do
        po <- o .: "price"
        listPrice <- po .: "listPrice"
        salesPrice <- po .: "salesPrice"
        discount <- po .: "discount"
        pure { listPrice, salesPrice, discount }
    p <- basicPrice <|> discountedPrice
    pure
      $ SegmentPrice
          { minimum
          , exclusiveMaximum
          , listPrice: p.listPrice
          , salesPrice: p.salesPrice
          , discount: p.discount
          }

instance encodeJsonSegmentPrice :: EncodeJson SegmentPrice where
  encodeJson (SegmentPrice x) = encodeJson x

newtype PriceSegmentation
  = PriceSegmentation
  { segmentUnit :: UnitRef
  , period :: SegmentationPeriod
  , model :: SegmentationModel
  , segments :: Array Segment
  }

instance decodeJsonPriceSegmentation :: DecodeJson PriceSegmentation where
  decodeJson json = PriceSegmentation <$> decodeJson json

instance encodeJsonPriceSegmentation :: EncodeJson PriceSegmentation where
  encodeJson (PriceSegmentation x) = encodeJson x

newtype PriceSegmentationByUnit
  = PriceSegmentationByUnit
  { unit :: UnitRef
  , segmentation :: PriceSegmentation
  }

instance decodeJsonPriceSegmentationByUnit :: DecodeJson PriceSegmentationByUnit where
  decodeJson json = PriceSegmentationByUnit <$> decodeJson json

instance encodeJsonPriceSegmentationByUnit :: EncodeJson PriceSegmentationByUnit where
  encodeJson (PriceSegmentationByUnit x) = encodeJson x

newtype RateCard
  = RateCard
  { sku :: Sku
  , name :: Maybe String
  , description :: Maybe String
  , charge :: Charge
  }

instance decodeJsonRateCard :: DecodeJson RateCard where
  decodeJson json = RateCard <$> decodeJson json

instance encodeJsonRateCard :: EncodeJson RateCard where
  encodeJson (RateCard x) = encodeJson x

newtype Price
  = Price (Array SegmentPrice)

instance decodeJsonPrice :: DecodeJson Price where
  decodeJson json = fixed <|> segmented
    where
    mkSingleton p =
      Price
        [ SegmentPrice
            { minimum: 0
            , exclusiveMaximum: Nothing
            , listPrice: p
            , salesPrice: p
            , discount: Nothing
            }
        ]

    fixed = mkSingleton <$> decodeJson json

    segmented = Price <$> decodeJson json

instance encodeJsonPrice :: EncodeJson Price where
  encodeJson (Price x) = encodeJson x

data SegmentationPeriod
  = SegmentationPeriodMonthly

instance showSegmentationPeriod :: Show SegmentationPeriod where
  show = case _ of
    SegmentationPeriodMonthly -> "Monthly"

instance decodeJsonSegmentationPeriod :: DecodeJson SegmentationPeriod where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Monthly" -> Right SegmentationPeriodMonthly
      _ -> Left (TypeMismatch "SegmentationPeriod")

instance encodeJsonSegmentationPeriod :: EncodeJson SegmentationPeriod where
  encodeJson = encodeJson <<< show

data SegmentationModel
  = SegmentationModelTiered
  | SegmentationModelVolume
  | SegmentationModelStairStep
  | SegmentationModelOverage

instance showSegmentationModel :: Show SegmentationModel where
  show = case _ of
    SegmentationModelTiered -> "Tiered"
    SegmentationModelVolume -> "Volume"
    SegmentationModelStairStep -> "StairStep"
    SegmentationModelOverage -> "Overage"

instance decodeJsonSegmentationModel :: DecodeJson SegmentationModel where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Tiered" -> Right SegmentationModelTiered
      "Volume" -> Right SegmentationModelVolume
      "StairStep" -> Right SegmentationModelStairStep
      "Overage" -> Right SegmentationModelOverage
      _ -> Left (TypeMismatch "SegmentationModel")

instance encodeJsonSegmentationModel :: EncodeJson SegmentationModel where
  encodeJson = encodeJson <<< show

newtype DefaultPriceByUnit
  = DefaultPriceByUnit
  { unit :: UnitRef
  , price :: Number
  }

instance decodeJsonDefaultPriceByUnit :: DecodeJson DefaultPriceByUnit where
  decodeJson json = DefaultPriceByUnit <$> decodeJson json

instance encodeJsonDefaultPriceByUnit :: EncodeJson DefaultPriceByUnit where
  encodeJson (DefaultPriceByUnit x) = encodeJson x

newtype PricesPerDimByUnit
  = PricesPerDimByUnit
  { dim :: DimValue
  , prices :: Array PriceByUnit
  , monthlyMinimum :: Number
  }

instance decodeJsonPricesPerDimByUnit :: DecodeJson PricesPerDimByUnit where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    prices <- o .: "prices"
    monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
    pure $ PricesPerDimByUnit { dim, prices, monthlyMinimum }

instance encodeJsonPricesPerDimByUnit :: EncodeJson PricesPerDimByUnit where
  encodeJson (PricesPerDimByUnit x) = encodeJson x

newtype PriceByUnit
  = PriceByUnit
  { unit :: UnitRef
  , price :: Price
  }

derive instance newtypePriceByUnit :: Newtype PriceByUnit _

instance decodeJsonPriceByUnit :: DecodeJson PriceByUnit where
  decodeJson = map PriceByUnit <<< decodeJson

instance encodeJsonPriceByUnit :: EncodeJson PriceByUnit where
  encodeJson (PriceByUnit x) = encodeJson x

newtype UnitRef
  = UnitRef { unitID :: String, product :: Maybe ProductRef }

derive instance newtypeUnitRef :: Newtype UnitRef _

instance decodeJsonUnitRef :: DecodeJson UnitRef where
  decodeJson json = UnitRef <$> plainID <|> full
    where
    plainID = (\id -> { unitID: id, product: Nothing }) <$> decodeJson json

    full = do
      o <- decodeJson json
      unitID <- o .: "unitID"
      product <- o .: "product"
      pure $ UnitRef { unitID, product }

instance encodeJsonUnitRef :: EncodeJson UnitRef where
  encodeJson (UnitRef x) = case x.product of
    Nothing -> encodeJson x.unitID
    Just _ -> encodeJson x

newtype ProductRef
  = ProductRef { sku :: Sku, solutionURI :: Maybe Uri }

instance decodeJsonProductRef :: DecodeJson ProductRef where
  decodeJson = map ProductRef <<< decodeJson

instance encodeJsonProductRef :: EncodeJson ProductRef where
  encodeJson (ProductRef x) = encodeJson x

data ChargeType
  = ChargeTypeOnetime
  | ChargeTypeMonthly
  | ChargeTypeUsage

instance showChargeType :: Show ChargeType where
  show = case _ of
    ChargeTypeOnetime -> "Onetime"
    ChargeTypeMonthly -> "Monthly"
    ChargeTypeUsage -> "Usage"

instance decodeJsonChargeType :: DecodeJson ChargeType where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Onetime" -> Right ChargeTypeOnetime
      "Monthly" -> Right ChargeTypeMonthly
      "Usage" -> Right ChargeTypeUsage
      _ -> Left (TypeMismatch "ChargeType")

instance encodeJsonChargeType :: EncodeJson ChargeType where
  encodeJson = encodeJson <<< show

data ConfigSchemaEntry
  = CseInteger
    { minimum :: Maybe Number
    , maximum :: Maybe Number
    , default :: Maybe Number
    }
  | CseString
    { minLength :: Maybe Int
    , maxLength :: Maybe Int
    , enum :: Array String
    , default :: Maybe String
    }
  | CseRegex
    { pattern :: String
    , default :: Maybe String
    }
  | CseConst { const :: ConfigValue }
  | CseArray
    { items :: ConfigSchemaEntry }
  | CseObject
    { properties :: Map String ConfigSchemaEntry }
  | CseOneOf { oneOf :: Array ConfigSchemaEntry }

instance decodeJsonConfigSchemaEntry :: DecodeJson ConfigSchemaEntry where
  decodeJson json = typed <|> constValue <|> oneOf
    where
    typed = do
      o <- decodeJson json
      type_ <- o .: "type"
      case type_ of
        "integer" -> do
          minimum <- o .:? "minimum"
          maximum <- o .:? "maximum"
          default <- o .:? "default"
          Right $ CseInteger { minimum, maximum, default }
        "string" -> do
          minLength <- o .:? "minLength"
          maxLength <- o .:? "maxLength"
          enum <- o .:? "enum" .!= []
          default <- o .:? "default"
          Right $ CseString { minLength, maxLength, enum, default }
        "regex" -> do
          pattern <- o .: "pattern"
          default <- o .:? "default"
          Right $ CseRegex { pattern, default }
        "array" -> do
          items <- o .: "items"
          Right $ CseArray { items }
        "object" -> do
          propertiesObj :: FO.Object ConfigSchemaEntry <- o .: "properties"
          let
            properties = Map.fromFoldable (FO.toUnfoldable propertiesObj :: Array _)
          Right $ CseObject { properties }
        _ -> Left (TypeMismatch "ConfigSchemaEntry")

    constValue = CseConst <$> decodeJson json

    oneOf = CseOneOf <$> decodeJson json

instance encodeJsonConfigSchemaEntry :: EncodeJson ConfigSchemaEntry where
  encodeJson = case _ of
    CseInteger x -> encodeJson x
    CseString x -> encodeJson x
    CseRegex x -> encodeJson x
    CseConst x -> encodeJson x
    CseArray x -> encodeJson x
    CseObject x -> encodeJson x
    CseOneOf x -> encodeJson x

data ConfigValue
  = CvInteger Int
  | CvString String
  | CvArray (Array ConfigValue)
  | CvObject (Map String ConfigValue)
  | CvNull

instance showConfigValue :: Show ConfigValue where
  show = case _ of
    CvInteger v -> show v
    CvString v -> v
    CvArray v -> show v
    CvObject v -> show v
    CvNull -> "null"

instance decodeJsonConfigValue :: DecodeJson ConfigValue where
  decodeJson json =
    (CvInteger <$> decodeJson json)
      <|> (CvString <$> decodeJson json)
      <|> (CvArray <$> decodeJson json)
      <|> parseObject
      <|> parseNull
    where
    parseObject = do
      valuesObj :: FO.Object ConfigValue <- decodeJson json
      let
        values = Map.fromFoldable (FO.toUnfoldable valuesObj :: Array _)
      pure $ CvObject values

    parseNull = do
      _ :: Maybe Int <- decodeJson json
      pure CvNull

instance encodeJsonConfigValue :: EncodeJson ConfigValue where
  encodeJson (CvInteger v) = encodeJson v
  encodeJson (CvString v) = encodeJson v
  encodeJson (CvArray v) = encodeJson v
  encodeJson (CvObject v) = encodeJson v
  encodeJson (CvNull) = encodeJson (Nothing :: Maybe Int)

-- TODO: Add `schema` and `variable`.
newtype ProductVariable
  = ProductVariable
  { name :: String, path :: String
  }

instance decodeJsonProductVariable :: DecodeJson ProductVariable where
  decodeJson = map ProductVariable <<< decodeJson

instance encodeJsonProductVariable :: EncodeJson ProductVariable where
  encodeJson (ProductVariable x) = encodeJson x

newtype SpecUnit
  = SpecUnit
  { id :: String
  , name :: Maybe String
  , description :: Maybe String
  , chargeType :: ChargeType
  , priceDimSchema :: Maybe ConfigSchemaEntry
  , reportDimSchemas :: Maybe (Array ConfigSchemaEntry)
  }

instance decodeJsonSpecUnit :: DecodeJson SpecUnit where
  decodeJson = map SpecUnit <<< decodeJson

instance encodeJsonSpecUnit :: EncodeJson SpecUnit where
  encodeJson (SpecUnit x) = encodeJson x

type SpecUnitMap
  = Map String SpecUnit

-- | A suitable label for a unit. Uses the unit name, if available, otherwise
-- | its identifier.
specUnitLabel :: SpecUnit -> String
specUnitLabel (SpecUnit { id, name }) = fromMaybe id name

newtype Product
  = Product
  { sku :: String
  , name :: Maybe String
  , description :: Maybe String
  , attr :: Maybe (Map String ConfigValue)
  , configSchema :: Maybe (Map String ConfigSchemaEntry)
  , options :: Maybe (Array ProductOption)
  , features :: Maybe (Array ProductFeature)
  , variables :: Maybe (Array ProductVariable)
  , units :: Array SpecUnit
  , rules :: Maybe (Array Rule)
  }

derive instance newtypeProduct :: Newtype Product _

instance decodeJsonProduct :: DecodeJson Product where
  decodeJson json = do
    o <- decodeJson json
    sku <- o .: "sku"
    name <- o .:? "name"
    description <- o .:? "description"
    attrObj :: Maybe (FO.Object ConfigValue) <- o .:? "attr"
    configSchemaObj :: Maybe (FO.Object ConfigSchemaEntry) <- o .:? "configSchema"
    let
      attr = (\obj -> Map.fromFoldable (FO.toUnfoldable obj :: Array _)) <$> attrObj

      configSchema = (\obj -> Map.fromFoldable (FO.toUnfoldable obj :: Array _)) <$> configSchemaObj
    options <- o .:? "options"
    features <- o .:? "features"
    variables <- o .:? "variables"
    units <- o .: "units"
    rules <- o .:? "rules"
    pure
      $ Product
          { sku
          , name
          , description
          , attr
          , configSchema
          , options
          , features
          , variables
          , units
          , rules
          }

instance encodeJsonProduct :: EncodeJson Product where
  encodeJson (Product x) = encodeJson x

-- | Produces a map from unit ID to the unit itself.
productUnits :: Product -> SpecUnitMap
productUnits = Map.fromFoldable <<< map (\u@(SpecUnit { id }) -> Tuple id u) <<< _.units <<< unwrap

data Platform
  = PlatformACL
  | PlatformChatlayer
  | PlatformDataMart
  | PlatformDialogue
  | PlatformIQ
  | PlatformMessageMedia
  | PlatformNova
  | PlatformSinchEngage
  | PlatformSinchMobile
  | PlatformUnwire
  | PlatformXura

instance showPlatform :: Show Platform where
  show = case _ of
    PlatformACL -> "ACL"
    PlatformChatlayer -> "Chatlayer"
    PlatformDataMart -> "DataMart"
    PlatformDialogue -> "Dialogue"
    PlatformIQ -> "IQ"
    PlatformMessageMedia -> "MessageMedia"
    PlatformNova -> "Nova"
    PlatformSinchEngage -> "SinchEngage"
    PlatformSinchMobile -> "SinchMobile"
    PlatformUnwire -> "Unwire"
    PlatformXura -> "Xura"

instance decodeJsonPlatform :: DecodeJson Platform where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "ACL" -> Right PlatformACL
      "Chatlayer" -> Right PlatformChatlayer
      "DataMart" -> Right PlatformDataMart
      "Dialogue" -> Right PlatformDialogue
      "IQ" -> Right PlatformIQ
      "MessageMedia" -> Right PlatformMessageMedia
      "Nova" -> Right PlatformNova
      "SinchEngage" -> Right PlatformSinchEngage
      "SinchMobile" -> Right PlatformSinchMobile
      "Unwire" -> Right PlatformUnwire
      "Xura" -> Right PlatformXura
      _ -> Left (TypeMismatch "Platform")

instance encodeJsonPlatform :: EncodeJson Platform where
  encodeJson = encodeJson <<< show

data ProductCategory
  = CategoryCalling
  | CategoryCampaignManager
  | CategoryChatlayer
  | CategoryContactCenter
  | CategoryConversationAPI
  | CategoryEmail
  | CategoryFacebookMessenger
  | CategoryFiddlebackAgencyTool
  | CategoryFlows
  | CategoryIPX
  | CategoryInbox
  | CategoryMMS
  | CategoryMessaging
  | CategoryNetworkInfrastructure
  | CategoryNumberLookup
  | CategoryNumbersVoice
  | CategoryOperatorServices
  | CategoryOtherSaaSTool
  | CategoryP2PMessaging
  | CategoryPersonalizedContent
  | CategoryPushNotification
  | CategoryRCS
  | CategoryRTC
  | CategoryRichMessaging
  | CategorySMS
  | CategorySinchForMarketing
  | CategoryVerification
  | CategoryViberBM
  | CategoryWhatsapp

instance showProductCategory :: Show ProductCategory where
  show = case _ of
    CategoryCalling -> "Calling"
    CategoryCampaignManager -> "CampaignManager"
    CategoryChatlayer -> "Chatlayer"
    CategoryContactCenter -> "ContactCenter"
    CategoryConversationAPI -> "ConversationAPI"
    CategoryEmail -> "Email"
    CategoryFacebookMessenger -> "FacebookMessenger"
    CategoryFiddlebackAgencyTool -> "FiddlebackAgencyTool"
    CategoryFlows -> "Flows"
    CategoryIPX -> "IPX"
    CategoryInbox -> "Inbox"
    CategoryMMS -> "MMS"
    CategoryMessaging -> "Messaging"
    CategoryNetworkInfrastructure -> "NetworkInfrastructure"
    CategoryNumberLookup -> "NumberLookup"
    CategoryNumbersVoice -> "NumbersVoice"
    CategoryOperatorServices -> "OperatorServices"
    CategoryOtherSaaSTool -> "OtherSaaSTool"
    CategoryP2PMessaging -> "P2PMessaging"
    CategoryPersonalizedContent -> "PersonalizedContent"
    CategoryPushNotification -> "PushNotification"
    CategoryRCS -> "RCS"
    CategoryRTC -> "RTC"
    CategoryRichMessaging -> "RichMessaging"
    CategorySMS -> "SMS"
    CategorySinchForMarketing -> "SinchForMarketing"
    CategoryVerification -> "Verification"
    CategoryViberBM -> "ViberBM"
    CategoryWhatsapp -> "Whatsapp"

instance decodeJsonProductCategory :: DecodeJson ProductCategory where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Calling" -> Right CategoryCalling
      "CampaignManager" -> Right CategoryCampaignManager
      "Chatlayer" -> Right CategoryChatlayer
      "ContactCenter" -> Right CategoryContactCenter
      "ConversationAPI" -> Right CategoryConversationAPI
      "Email" -> Right CategoryEmail
      "FacebookMessenger" -> Right CategoryFacebookMessenger
      "FiddlebackAgencyTool" -> Right CategoryFiddlebackAgencyTool
      "Flows" -> Right CategoryFlows
      "IPX" -> Right CategoryIPX
      "Inbox" -> Right CategoryInbox
      "MMS" -> Right CategoryMMS
      "Messaging" -> Right CategoryMessaging
      "NetworkInfrastructure" -> Right CategoryNetworkInfrastructure
      "NumberLookup" -> Right CategoryNumberLookup
      "NumbersVoice" -> Right CategoryNumbersVoice
      "OperatorServices" -> Right CategoryOperatorServices
      "OtherSaaSTool" -> Right CategoryOtherSaaSTool
      "P2PMessaging" -> Right CategoryP2PMessaging
      "PersonalizedContent" -> Right CategoryPersonalizedContent
      "PushNotification" -> Right CategoryPushNotification
      "RCS" -> Right CategoryRCS
      "RTC" -> Right CategoryRTC
      "RichMessaging" -> Right CategoryRichMessaging
      "SMS" -> Right CategorySMS
      "SinchForMarketing" -> Right CategorySinchForMarketing
      "Verification" -> Right CategoryVerification
      "ViberBM" -> Right CategoryViberBM
      "Whatsapp" -> Right CategoryWhatsapp
      _ -> Left (TypeMismatch "ProductCategory")

instance encodeJsonProductCategory :: EncodeJson ProductCategory where
  encodeJson = encodeJson <<< show

data Sku
  = SkuCode String
  | Sku
    { code :: String
    , name :: String
    , description :: Maybe String
    , productCategory :: ProductCategory
    , platform :: Platform
    }

instance decodeJsonSku :: DecodeJson Sku where
  decodeJson json = skuCode <|> productOption
    where
    skuCode = SkuCode <$> decodeJson json

    productOption = do
      o <- decodeJson json
      code <- o .: "code"
      name <- o .: "name"
      description <- o .:? "description"
      productCategory <- o .: "productCategory"
      platform <- o .: "platform"
      pure
        $ Sku
            { code
            , name
            , description
            , productCategory
            , platform
            }

instance encodeJsonSku :: EncodeJson Sku where
  encodeJson (SkuCode x) = encodeJson x
  encodeJson (Sku x) = encodeJson x

skuCode :: Sku -> String
skuCode (SkuCode code) = code

skuCode (Sku { code }) = code

data ProductOption
  = ProdOptSkuCode String
  | ProductOption
    { sku :: Sku
    , name :: Maybe String
    , required :: Boolean
    , quoteLineVisible :: Boolean
    , quantity :: Int
    , minQuantity :: Int
    , maxQuantity :: Int
    , requiredOptions :: Maybe (Array Sku)
    , excludeOptions :: Maybe (Array Sku)
    , selectedByDefault :: Boolean
    , type_ :: ProductOptionType
    }

instance decodeJsonProductOption :: DecodeJson ProductOption where
  decodeJson json = skuCode <|> productOption
    where
    skuCode = ProdOptSkuCode <$> decodeJson json

    productOption = do
      o <- decodeJson json
      name <- o .:? "name"
      sku <- o .: "sku"
      required <- o .:? "required" .!= false
      quoteLineVisible <- o .:? "quoteLineVisible" .!= false
      quantity <- o .:? "quantity" .!= 0
      minQuantity <- o .:? "minQuantity" .!= 0
      maxQuantity <- o .:? "maxQuantity" .!= 1
      requiredOptions <- o .:? "requiredOptions"
      excludeOptions <- o .:? "excludeOptions"
      selectedByDefault <- o .:? "selectedByDefault" .!= false
      type_ <- o .:? "type" .!= Component
      pure
        $ ProductOption
            { sku
            , name
            , required
            , quoteLineVisible
            , quantity
            , minQuantity
            , maxQuantity
            , requiredOptions
            , excludeOptions
            , selectedByDefault
            , type_
            }

instance encodeJsonProductOption :: EncodeJson ProductOption where
  encodeJson (ProdOptSkuCode x) = encodeJson x
  encodeJson (ProductOption x) = encodeJson x

data ProductOptionType
  = Component
  | Accessory
  | Related

instance showProductOptionType :: Show ProductOptionType where
  show = case _ of
    Component -> "Component"
    Accessory -> "Accessory"
    Related -> "Related"

instance decodeJsonProductOptionType :: DecodeJson ProductOptionType where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Component" -> Right Component
      "Accessory" -> Right Accessory
      "Related" -> Right Related
      _ -> Left (TypeMismatch "ProductOptionType")

instance encodeJsonProductOptionType :: EncodeJson ProductOptionType where
  encodeJson = encodeJson <<< show

newtype ProductFeature
  = ProductFeature
  { name :: Maybe String
  , description :: Maybe String
  , options :: Maybe (Array Json)
  }

instance decodeJsonProductFeature :: DecodeJson ProductFeature where
  decodeJson json = ProductFeature <$> decodeJson json

instance encodeJsonProductFeature :: EncodeJson ProductFeature where
  encodeJson (ProductFeature x) = encodeJson x

data BillingOption
  = Prepay
  | PostPay

derive instance eqBillingOption :: Eq BillingOption

instance showBillingOption :: Show BillingOption where
  show = case _ of
    Prepay -> "Prepay"
    PostPay -> "PostPay"

instance decodeJsonBillingOption :: DecodeJson BillingOption where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Prepay" -> Right Prepay
      "PostPay" -> Right PostPay
      _ -> Left (TypeMismatch "BillingOption")

instance encodeJsonBillingOption :: EncodeJson BillingOption where
  encodeJson = encodeJson <<< show

data ContractTerm
  = Ongoing
  | Fixed

derive instance eqContractTerm :: Eq ContractTerm

instance showContractTerm :: Show ContractTerm where
  show = case _ of
    Ongoing -> "Ongoing"
    Fixed -> "Fixed"

instance decodeJsonContractTerm :: DecodeJson ContractTerm where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Ongoing" -> Right Ongoing
      "Fixed" -> Right Fixed
      _ -> Left (TypeMismatch "ContractTerm")

instance encodeJsonContractTerm :: EncodeJson ContractTerm where
  encodeJson = encodeJson <<< show

newtype Commercial
  = Commercial
  { billingOption :: BillingOption
  , contractTerm :: ContractTerm
  , paymentCurrency :: Currency
  , priceCurrency :: Currency
  }

instance decodeJsonCommercial :: DecodeJson Commercial where
  decodeJson json = Commercial <$> decodeJson json

instance encodeJsonCommercial :: EncodeJson Commercial where
  encodeJson (Commercial x) = encodeJson x

newtype Address
  = Address {}

instance decodeJsonAddress :: DecodeJson Address where
  decodeJson json = Address <$> decodeJson json

instance encodeJsonAddress :: EncodeJson Address where
  encodeJson (Address x) = encodeJson x

newtype Contact
  = Contact
  { email :: String
  , name :: String
  , phone :: String
  }

instance decodeJsonContact :: DecodeJson Contact where
  decodeJson json = Contact <$> decodeJson json

instance encodeJsonContact :: EncodeJson Contact where
  encodeJson (Contact x) = encodeJson x

newtype Purchaser
  = Purchaser
  { address :: Address
  , contacts :: { primary :: Contact, finance :: Contact }
  , corporateName :: String
  , country :: String
  , registrationNr :: String
  , taxID :: String
  , website :: Uri
  }

instance decodeJsonPurchaser :: DecodeJson Purchaser where
  decodeJson json = Purchaser <$> decodeJson json

instance encodeJsonPurchaser :: EncodeJson Purchaser where
  encodeJson (Purchaser x) = encodeJson x

newtype LegalEntity
  = LegalEntity
  { name :: String
  , address :: Address
  , country :: String
  }

instance decodeJsonLegalEntity :: DecodeJson LegalEntity where
  decodeJson json = LegalEntity <$> decodeJson json

instance encodeJsonLegalEntity :: EncodeJson LegalEntity where
  encodeJson (LegalEntity x) = encodeJson x

newtype Seller
  = Seller
  { contacts :: { primary :: Contact, finance :: Contact, support :: Contact }
  , legalEntity :: LegalEntity
  }

instance decodeJsonSeller :: DecodeJson Seller where
  decodeJson json = Seller <$> decodeJson json

instance encodeJsonSeller :: EncodeJson Seller where
  encodeJson (Seller x) = encodeJson x

newtype BillingAccountRef
  = BillingAccountRef
  { billingAccountID :: String
  }

instance decodeJsonBillingAccountRef :: DecodeJson BillingAccountRef where
  decodeJson json = BillingAccountRef <$> decodeJson json

instance encodeJsonBillingAccountRef :: EncodeJson BillingAccountRef where
  encodeJson (BillingAccountRef x) = encodeJson x

data ReturnCustomerCommercial
  = RccCommercial Commercial
  | RccBillingAccountRef BillingAccountRef

instance decodeJsonReturnCustomerCommercial :: DecodeJson ReturnCustomerCommercial where
  decodeJson json =
    (RccCommercial <$> decodeJson json)
      <|> (RccBillingAccountRef <$> decodeJson json)

instance encodeJsonReturnCustomerCommercial :: EncodeJson ReturnCustomerCommercial where
  encodeJson = case _ of
    RccCommercial x -> encodeJson x
    RccBillingAccountRef x -> encodeJson x

type Date
  = String

newtype Validity
  = Validity
  { startDate :: Date
  , endDateExclusive :: Maybe Date
  }

instance decodeJsonValidity :: DecodeJson Validity where
  decodeJson json = Validity <$> decodeJson json

instance encodeJsonValidity :: EncodeJson Validity where
  encodeJson (Validity x) = encodeJson x

newtype ProductInstance
  = ProductInstance
  {
  }

instance decodeJsonProductInstance :: DecodeJson ProductInstance where
  decodeJson json = ProductInstance <$> decodeJson json

newtype DateTime
  = DateTime String

instance decodeDateTime :: DecodeJson DateTime where
  decodeJson json = DateTime <$> decodeJson json

instance encodeJsonDateTime :: EncodeJson DateTime where
  encodeJson (DateTime x) = encodeJson x

-- TODO: Add `configs`
newtype Asset
  = Asset
  { sku :: Sku
  -- , configs :: Array ConfigValue
  , billingAccount :: BillingAccountRef
  , createTime :: DateTime
  , updateTime :: DateTime
  , priceOverrides :: Array PriceOverride
  }

instance decodeJsonAsset :: DecodeJson Asset where
  decodeJson json = Asset <$> decodeJson json

instance encodeJsonAsset :: EncodeJson Asset where
  encodeJson (Asset x) = encodeJson x

newtype PriceOverride
  = PriceOverride
  { basePriceBookRef :: PriceBookRef
  , charge :: Charge
  , validity :: Validity
  }

instance decodeJsonPriceOverride :: DecodeJson PriceOverride where
  decodeJson json = PriceOverride <$> decodeJson json

instance encodeJsonPriceOverride :: EncodeJson PriceOverride where
  encodeJson (PriceOverride x) = encodeJson x

newtype PriceBookRef
  = PriceBookRef
  { priceBookID :: String
  , version :: String
  , currency :: Currency
  , solutionURI :: Maybe Uri
  }

instance decodeJsonPriceBookRef :: DecodeJson PriceBookRef where
  decodeJson json = PriceBookRef <$> decodeJson json

instance encodeJsonPriceBookRef :: EncodeJson PriceBookRef where
  encodeJson (PriceBookRef x) = encodeJson x

newtype SalesforceAccountRef
  = SalesforceAccountRef
  { salesforceAccountID :: String
  }

instance decodeJsonSalesforceAccountRef :: DecodeJson SalesforceAccountRef where
  decodeJson json = SalesforceAccountRef <$> decodeJson json

instance encodeJsonSalesforceAccountRef :: EncodeJson SalesforceAccountRef where
  encodeJson (SalesforceAccountRef x) = encodeJson x

newtype ReturnCustomerData
  = ReturnCustomerData
  { assets :: Array Asset
  , salesforceAccountRef :: SalesforceAccountRef
  }

instance decodeJsonReturnCustomerData :: DecodeJson ReturnCustomerData where
  decodeJson json = ReturnCustomerData <$> decodeJson json

instance encodeJsonReturnCustomerData :: EncodeJson ReturnCustomerData where
  encodeJson (ReturnCustomerData x) = encodeJson x

data Customer
  = NewCustomer
    { commercial :: Commercial
    , purchaser :: Purchaser
    , seller :: Seller
    }
  | ReturnCustomer
    { commercial :: ReturnCustomerCommercial
    , customer :: ReturnCustomerData
    }

instance decodeJsonCustomer :: DecodeJson Customer where
  decodeJson json = (NewCustomer <$> decodeJson json) <|> (ReturnCustomer <$> decodeJson json)

instance encodeJsonCustomer :: EncodeJson Customer where
  encodeJson = case _ of
    NewCustomer x -> encodeJson x
    ReturnCustomer x -> encodeJson x

data OrderStatus
  = OsSalesOrderNew
  | OsSalesOrderApprovalPending
  | OsSalesOrderApproved
  | OsSalesOrderAborted
  | OsSalesOrderSignPending
  | OsSalesOrderSigned
  | OsServiceOrderPending
  | OsServiceOrderOngoing
  | OsServiceOrderCompleted
  | OsServiceOrderAborted
  | OsOrderFulfillmentPending
  | OsOrderFulfillmentOngoging
  | OsOrderFulfillmentCompleted
  | OsOrderFulfillmentAborted

instance showOrderStatus :: Show OrderStatus where
  show = case _ of
    OsSalesOrderNew -> "SalesOrder.New"
    OsSalesOrderApprovalPending -> "SalesOrder.ApprovalPending"
    OsSalesOrderApproved -> "SalesOrder.Approved"
    OsSalesOrderAborted -> "SalesOrder.Aborted"
    OsSalesOrderSignPending -> "SalesOrder.SignPending"
    OsSalesOrderSigned -> "SalesOrder.Signed"
    OsServiceOrderPending -> "ServiceOrder.Pending"
    OsServiceOrderOngoing -> "ServiceOrder.Ongoing"
    OsServiceOrderCompleted -> "ServiceOrder.Completed"
    OsServiceOrderAborted -> "ServiceOrder.Aborted"
    OsOrderFulfillmentPending -> "OrderFulfillment.Pending"
    OsOrderFulfillmentOngoging -> "OrderFulfillment.Ongoging"
    OsOrderFulfillmentCompleted -> "OrderFulfillment.Completed"
    OsOrderFulfillmentAborted -> "OrderFulfillment.Aborted"

instance decodeJsonOrderStatus :: DecodeJson OrderStatus where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "SalesOrder.New" -> Right OsSalesOrderNew
      "SalesOrder.ApprovalPending" -> Right OsSalesOrderApprovalPending
      "SalesOrder.Approved" -> Right OsSalesOrderApproved
      "SalesOrder.Aborted" -> Right OsSalesOrderAborted
      "SalesOrder.SignPending" -> Right OsSalesOrderSignPending
      "SalesOrder.Signed" -> Right OsSalesOrderSigned
      "ServiceOrder.Pending" -> Right OsServiceOrderPending
      "ServiceOrder.Ongoing" -> Right OsServiceOrderOngoing
      "ServiceOrder.Completed" -> Right OsServiceOrderCompleted
      "ServiceOrder.Aborted" -> Right OsServiceOrderAborted
      "OrderFulfillment.Pending" -> Right OsOrderFulfillmentPending
      "OrderFulfillment.Ongoging" -> Right OsOrderFulfillmentOngoging
      "OrderFulfillment.Completed" -> Right OsOrderFulfillmentCompleted
      "OrderFulfillment.Aborted" -> Right OsOrderFulfillmentAborted
      _ -> Left (TypeMismatch "OrderStatus")

instance encodeOrderStatus :: EncodeJson OrderStatus where
  encodeJson = encodeJson <<< show

newtype OrderSummary
  = OrderSummary
  { estimatedUsageTotal :: Number
  , monthlyTotal :: Number
  , onetimeTotal :: Number
  }

instance decodeJsonOrderSummary :: DecodeJson OrderSummary where
  decodeJson json = OrderSummary <$> decodeJson json

instance encodeJsonOrderSummary :: EncodeJson OrderSummary where
  encodeJson (OrderSummary x) = encodeJson x

newtype OrderSectionSummary
  = OrderSectionSummary
  { estimatedUsageSubTotal :: Number
  , monthlySubTotal :: Number
  , onetimeSubTotal :: Number
  }

instance decodeJsonOrderSectionSummary :: DecodeJson OrderSectionSummary where
  decodeJson json = OrderSectionSummary <$> decodeJson json

instance encodeJsonOrderSectionSummary :: EncodeJson OrderSectionSummary where
  encodeJson (OrderSectionSummary x) = encodeJson x

newtype Segment
  = Segment
  { minimum :: Int
  , exclusiveMaximum :: Maybe Int
  }

instance decodeJsonSegment :: DecodeJson Segment where
  decodeJson json = Segment <$> decodeJson json

instance encodeJsonSegment :: EncodeJson Segment where
  encodeJson (Segment x) = encodeJson x

newtype OrderLine
  = OrderLine
  { sku :: Sku
  , charge :: Charge
  , quantity :: Int
  , configs :: Array (Map String ConfigValue)
  }

derive instance newtypeOrderLine :: Newtype OrderLine _

instance decodeJsonOrderLine :: DecodeJson OrderLine where
  decodeJson json = do
    o <- decodeJson json
    sku <- o .: "sku"
    charge <- o .: "charge"
    quantity <- o .: "quantity"
    configs <- o .:? "configs" .!= []
    pure $ OrderLine { sku, charge, quantity, configs }

instance encodeJsonOrderLine :: EncodeJson OrderLine where
  encodeJson (OrderLine x) =
    ("sku" := x.sku)
      ~> ("charge" := x.charge)
      ~> ("quantity" := x.quantity)
      ~> ("configs" := map (FO.fromFoldable <<< \c -> Map.toUnfoldable c :: Array _) x.configs)
      ~> jsonEmptyObject

newtype OrderSection
  = OrderSection
  { solutionURI :: Uri
  , basePriceBook :: PriceBookRef
  , orderLines :: Array OrderLine
  }

instance decodeJsonOrderSection :: DecodeJson OrderSection where
  decodeJson json = OrderSection <$> decodeJson json

instance encodeJsonOrderSection :: EncodeJson OrderSection where
  encodeJson (OrderSection x) = encodeJson x

newtype OrderForm
  = OrderForm
  { id :: String
  , status :: OrderStatus
  , customer :: Customer
  , sections :: Array OrderSection
  }

instance decodeJsonOrderForm :: DecodeJson OrderForm where
  decodeJson json = OrderForm <$> decodeJson json

instance encodeJsonOrderForm :: EncodeJson OrderForm where
  encodeJson (OrderForm x) = encodeJson x
