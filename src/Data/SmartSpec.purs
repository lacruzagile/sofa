module Data.SmartSpec
  ( Address(..)
  , Asset(..)
  , BillingAccountRef(..)
  , BillingOption(..)
  , Charge(..)
  , ChargeElement(..)
  , ChargeType(..)
  , ChargeUnit(..)
  , ChargeUnitMap
  , ChargeUnitRef(..)
  , Commercial(..)
  , ConfigSchemaEntry(..)
  , ConfigSchemaEntryMeta
  , ConfigValue(..)
  , Contact(..)
  , ContractTerm(..)
  , Currency(..)
  , Customer(..)
  , Date(..)
  , DateTime(..)
  , DefaultPricePerUnit(..)
  , DimValue(..)
  , Discount(..)
  , DiscountPerDim(..)
  , DiscountProfilePerUnit(..)
  , EstimatedVWAPPerUnit(..)
  , LegalEntity(..)
  , OrderForm(..)
  , OrderLine(..)
  , OrderSection(..)
  , OrderStatus(..)
  , Platform(..)
  , Price(..)
  , PriceBook(..)
  , PriceBookCurrency(..)
  , PriceBookRef(..)
  , PriceBookVersion(..)
  , PriceByDim(..)
  , PriceByUnit(..)
  , PriceByUnitPerDim(..)
  , PriceDimGroupRef(..)
  , PriceOverride(..)
  , PricePerSegment(..)
  , PriceSegmentation(..)
  , PriceSegmentationPerUnit(..)
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
  , SegmentationModel(..)
  , SegmentationPeriod(..)
  , Seller(..)
  , Severity(..)
  , Sku(..)
  , Solution(..)
  , Uri(..)
  , Validity(..)
  , chargeUnitLabel
  , configSchemaEntryDescription
  , configSchemaEntryTitle
  , productChargeUnits
  , skuCode
  , solutionProducts
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, (.!=), (.:), (.:?), (:=), (~>))
import Data.Array as A
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as QC

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
  , solutions :: Map String Solution
  }

instance decodeJsonProductCatalog :: DecodeJson ProductCatalog where
  decodeJson json = do
    o <- decodeJson json
    name <- o .:? "name"
    description <- o .:? "description"
    solutionsObj :: FO.Object Solution <- o .: "solutions"
    let
      solutions = Map.fromFoldable (FO.toUnfoldable solutionsObj :: Array _)
    pure
      $ ProductCatalog
          { name
          , description
          , solutions
          }

data RuleStage
  = RsSalesOrder
  | RsConfiguration

instance showRuleStage :: Show RuleStage where
  show = case _ of
    RsSalesOrder -> "SalesOrder"
    RsConfiguration -> "Configuration"

instance decodeJsonRuleStage :: DecodeJson RuleStage where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "SalesOrder" -> Right RsSalesOrder
      "Configuration" -> Right RsConfiguration
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
  , name :: String
  , description :: Maybe String
  , byVersion :: Array PriceBookVersion
  }

derive instance newtypePriceBook :: Newtype PriceBook _

instance decodeJsonPriceBook :: DecodeJson PriceBook where
  decodeJson json = PriceBook <$> decodeJson json

instance encodeJsonPriceBook :: EncodeJson PriceBook where
  encodeJson (PriceBook x) = encodeJson x

newtype PriceBookVersion
  = PriceBookVersion
  { version :: Date
  , parent :: Maybe PriceBookRef
  , byCurrency :: Array PriceBookCurrency
  }

derive instance newtypePriceBookVersion :: Newtype PriceBookVersion _

instance decodeJsonPriceBookVersion :: DecodeJson PriceBookVersion where
  decodeJson json = PriceBookVersion <$> decodeJson json

instance encodeJsonPriceBookVersion :: EncodeJson PriceBookVersion where
  encodeJson (PriceBookVersion x) = encodeJson x

newtype PriceBookCurrency
  = PriceBookCurrency
  { currency :: Currency
  , rateCards :: Maybe (Array RateCard)
  }

derive instance newtypePriceBookCurrency :: Newtype PriceBookCurrency _

instance decodeJsonPriceBookCurrency :: DecodeJson PriceBookCurrency where
  decodeJson json = PriceBookCurrency <$> decodeJson json

instance encodeJsonPriceBookCurrency :: EncodeJson PriceBookCurrency where
  encodeJson (PriceBookCurrency x) = encodeJson x

-- TODO: Assert non-negative.
newtype Charge
  = ChargeArray (Array ChargeElement)

instance decodeJsonCharge :: DecodeJson Charge where
  decodeJson json = rccElement <|> rccArray
    where
    rccElement = (ChargeArray <<< A.singleton) <$> decodeJson json

    rccArray = ChargeArray <$> decodeJson json

instance encodeJsonCharge :: EncodeJson Charge where
  encodeJson = case _ of
    ChargeArray [ x ] -> encodeJson x
    ChargeArray xs -> encodeJson xs

newtype ChargeElement
  = ChargeElement
  { units :: Array ChargeUnitRef
  , segmentationByUnit :: Array PriceSegmentationPerUnit
  , defaultPriceByUnit :: Array DefaultPricePerUnit
  , priceByUnitByDim :: Array PriceByUnitPerDim
  , estimatedVWAPByUnit :: Array EstimatedVWAPPerUnit
  , monthlyMinimum :: Number
  , termOfPriceChangeInDays :: Int
  }

instance decodeJsonChargeElement :: DecodeJson ChargeElement where
  decodeJson json = rccSimple json <|> rccMixed json
    where
    rccSimple j = do
      o <- decodeJson j
      unit <- o .: "unit"
      SimplePrice pricesByDim <- o .: "price"
      estimatedVWAP <- o .:? "estimatedVWAP"
      segmentation <- o .:? "segmentation"
      termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
      monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
      let
        toPriceByUnitByDim (PriceByDim p) =
          PriceByUnitPerDim
            { dim: p.dim
            , prices:
                [ PriceByUnit
                    { unit, price: p.price
                    }
                ]
            , monthlyMinimum: p.monthlyMinimum
            }
      pure
        $ ChargeElement
            { units: [ unit ]
            , segmentationByUnit:
                maybe
                  []
                  (\s -> [ PriceSegmentationPerUnit { segmentation: s, unit } ])
                  segmentation
            , defaultPriceByUnit: []
            , priceByUnitByDim: map toPriceByUnitByDim pricesByDim
            , estimatedVWAPByUnit: maybe [] A.singleton estimatedVWAP
            , monthlyMinimum
            , termOfPriceChangeInDays
            }

    rccMixed j = do
      o <- decodeJson j
      units <- o .: "units"
      segmentationByUnit <- o .:? "segmentationByUnit" .!= []
      defaultPriceByUnit <- o .:? "defaultPriceByUnit" .!= []
      priceByUnitByDim <- o .: "priceByUnitByDim"
      estimatedVWAPByUnit <- o .:? "estimatedVWAPByUnit" .!= []
      monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
      termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
      pure
        $ ChargeElement
            { units
            , segmentationByUnit
            , defaultPriceByUnit
            , priceByUnitByDim
            , estimatedVWAPByUnit
            , monthlyMinimum
            , termOfPriceChangeInDays
            }

instance encodeJsonChargeElement :: EncodeJson ChargeElement where
  encodeJson = case _ of
    ChargeElement
      { units: [ unit ]
    , segmentationByUnit: [ PriceSegmentationPerUnit { segmentation } ]
    , defaultPriceByUnit: []
    , priceByUnitByDim
    , monthlyMinimum
    , termOfPriceChangeInDays
    } ->
      encodeJson
        { unit
        , price: SimplePrice $ A.mapMaybe simplePrice priceByUnitByDim
        , segmentation
        , termOfPriceChangeInDays
        , monthlyMinimum
        }
    ChargeElement
      { units: [ unit ]
    , segmentationByUnit: []
    , defaultPriceByUnit: []
    , priceByUnitByDim
    , monthlyMinimum
    , termOfPriceChangeInDays
    } ->
      encodeJson
        { unit
        , price: SimplePrice $ A.mapMaybe simplePrice priceByUnitByDim
        , termOfPriceChangeInDays
        , monthlyMinimum
        }
    ChargeElement x -> encodeJson x
    where
    simplePrice = case _ of
      PriceByUnitPerDim
        { dim
      , prices: [ PriceByUnit { price } ]
      , monthlyMinimum
      } -> Just $ PriceByDim { dim, price, monthlyMinimum }
      _ -> Nothing

newtype SimplePrice
  = SimplePrice (Array PriceByDim)

instance decodeJsonSimplePrice :: DecodeJson SimplePrice where
  decodeJson json = segmented <|> byDim
    where
    segmented = do
      price <- decodeJson json
      pure
        $ SimplePrice
            [ PriceByDim
                { dim: DimValue CvNull
                , price
                , monthlyMinimum: 0.0
                }
            ]

    byDim = SimplePrice <$> decodeJson json

instance encodeJsonSimplePrice :: EncodeJson SimplePrice where
  encodeJson ( SimplePrice
      [ PriceByDim
      { dim: DimValue CvNull
    , price: price
    , monthlyMinimum: 0.0
    }
    ]
  ) = encodeJson price
  encodeJson (SimplePrice x) = encodeJson x

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

derive instance genericDiscount :: Generic Discount _

derive instance eqDiscount :: Eq Discount

instance showDiscount :: Show Discount where
  show = genericShow

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

instance arbDiscount :: Arbitrary Discount where
  arbitrary = QC.oneOf $ fromNonEmpty $ genPercentage :| [ genAbsolute ]
    where
    genPercentage = DiscountPercentage <$> QC.choose 0.0 100.0

    genAbsolute = DiscountAbsolute <$> QC.choose (-100.0) 0.0

newtype PricePerSegment
  = PricePerSegment
  { minimum :: Int
  , exclusiveMaximum :: Maybe Int
  , listPrice :: Number
  , salesPrice :: Maybe Number
  , discount :: Maybe Discount
  }

derive instance genericPricePerSegment :: Generic PricePerSegment _

derive instance eqPricePerSegment :: Eq PricePerSegment

derive instance newtypePricePerSegment :: Newtype PricePerSegment _

instance showPricePerSegment :: Show PricePerSegment where
  show = genericShow

instance decodeJsonPricePerSegment :: DecodeJson PricePerSegment where
  decodeJson json = do
    o <- decodeJson json
    minimum <- o .: "minimum"
    exclusiveMaximum <- o .:? "exclusiveMaximum"
    let
      basicPrice = do
        p <- o .: "price"
        pure { listPrice: p, salesPrice: Nothing, discount: Nothing }

      customPrice = do
        po <- o .: "price"
        listPrice <- po .: "listPrice"
        salesPrice <- po .: "salesPrice"
        discount <- po .:? "discount"
        pure { listPrice, salesPrice, discount }
    p <- basicPrice <|> customPrice
    pure
      $ PricePerSegment
          { minimum
          , exclusiveMaximum
          , listPrice: p.listPrice
          , salesPrice: p.salesPrice
          , discount: p.discount
          }

instance encodeJsonPricePerSegment :: EncodeJson PricePerSegment where
  encodeJson (PricePerSegment pps) = json
    where
    json
      | isNothing pps.discount && isNothing pps.salesPrice =
        encodeJson
          { minimum: pps.minimum
          , exclusiveMaximum: pps.exclusiveMaximum
          , price: pps.listPrice
          }

    json
      | otherwise =
        encodeJson
          { minimum: pps.minimum
          , exclusiveMaximum: pps.exclusiveMaximum
          , price:
              { listPrice: pps.listPrice
              , salesPrice: pps.salesPrice
              , discount: pps.discount
              }
          }

instance arbPricePerSegment :: Arbitrary PricePerSegment where
  arbitrary = do
    minimum <- arbitrary
    exclusiveMaximum <- arbitrary
    listPrice <- arbitrary
    salesPrice <- arbitrary
    discount <- arbitrary
    pure
      $ PricePerSegment
          { minimum
          , exclusiveMaximum
          , listPrice
          , salesPrice
          , discount
          }

newtype PriceSegmentation
  = PriceSegmentation
  { unit :: ChargeUnitRef
  , period :: SegmentationPeriod
  , model :: SegmentationModel
  , segments :: Array Segment
  }

instance decodeJsonPriceSegmentation :: DecodeJson PriceSegmentation where
  decodeJson json = PriceSegmentation <$> decodeJson json

instance encodeJsonPriceSegmentation :: EncodeJson PriceSegmentation where
  encodeJson (PriceSegmentation x) = encodeJson x

newtype PriceSegmentationPerUnit
  = PriceSegmentationPerUnit
  { unit :: ChargeUnitRef
  , segmentation :: PriceSegmentation
  }

instance decodeJsonPriceSegmentationPerUnit :: DecodeJson PriceSegmentationPerUnit where
  decodeJson json = PriceSegmentationPerUnit <$> decodeJson json

instance encodeJsonPriceSegmentationPerUnit :: EncodeJson PriceSegmentationPerUnit where
  encodeJson (PriceSegmentationPerUnit x) = encodeJson x

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
  = Price (Array PricePerSegment)

derive instance newtypePrice :: Newtype Price _

instance decodeJsonPrice :: DecodeJson Price where
  decodeJson json = fixed <|> custom <|> segmented
    where
    mkSingleton p =
      Price
        [ PricePerSegment
            { minimum: 0
            , exclusiveMaximum: Nothing
            , listPrice: p
            , salesPrice: Nothing
            , discount: Nothing
            }
        ]

    fixed = mkSingleton <$> decodeJson json

    custom = do
      o <- decodeJson json
      listPrice <- o .: "listPrice"
      salesPrice <- o .: "salesPrice"
      discount <- o .:? "discount"
      pure
        $ Price
            [ PricePerSegment
                { minimum: 0
                , exclusiveMaximum: Nothing
                , listPrice
                , salesPrice: Just salesPrice
                , discount
                }
            ]

    segmented = (Price <<< sortSegments) <$> decodeJson json

    sortSegments = A.sortBy $ comparing (_.minimum <<< unwrap)

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

newtype EstimatedVWAPPerUnit
  = EstimatedVWAPPerUnit
  { unit :: ChargeUnitRef
  , price :: Number
  }

instance decodeJsonEstimatedVWAPPerUnit :: DecodeJson EstimatedVWAPPerUnit where
  decodeJson json = EstimatedVWAPPerUnit <$> decodeJson json

instance encodeJsonEstimatedVWAPPerUnit :: EncodeJson EstimatedVWAPPerUnit where
  encodeJson (EstimatedVWAPPerUnit x) = encodeJson x

newtype DefaultPricePerUnit
  = DefaultPricePerUnit
  { unit :: ChargeUnitRef
  , currency :: Maybe Currency
  , price :: Number
  }

instance decodeJsonDefaultPricePerUnit :: DecodeJson DefaultPricePerUnit where
  decodeJson json = DefaultPricePerUnit <$> decodeJson json

instance encodeJsonDefaultPricePerUnit :: EncodeJson DefaultPricePerUnit where
  encodeJson (DefaultPricePerUnit x) = encodeJson x

newtype PriceByUnitPerDim
  = PriceByUnitPerDim
  { dim :: DimValue
  , prices :: Array PriceByUnit
  , monthlyMinimum :: Number
  }

instance decodeJsonPriceByUnitPerDim :: DecodeJson PriceByUnitPerDim where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    prices <- o .: "prices"
    monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
    pure
      $ PriceByUnitPerDim
          { dim
          , prices: A.sortBy (comparing (_.unitID <<< unwrap <<< _.unit <<< unwrap)) prices
          , monthlyMinimum
          }

instance encodeJsonPriceByUnitPerDim :: EncodeJson PriceByUnitPerDim where
  encodeJson (PriceByUnitPerDim x) = encodeJson x

newtype PriceByUnit
  = PriceByUnit
  { unit :: ChargeUnitRef
  , price :: Price
  }

derive instance newtypePriceByUnit :: Newtype PriceByUnit _

instance decodeJsonPriceByUnit :: DecodeJson PriceByUnit where
  decodeJson = map PriceByUnit <<< decodeJson

instance encodeJsonPriceByUnit :: EncodeJson PriceByUnit where
  encodeJson (PriceByUnit x) = encodeJson x

newtype ChargeUnitRef
  = ChargeUnitRef { unitID :: String, product :: Maybe ProductRef }

derive instance newtypeChargeUnitRef :: Newtype ChargeUnitRef _

instance decodeJsonChargeUnitRef :: DecodeJson ChargeUnitRef where
  decodeJson json = ChargeUnitRef <$> plainID <|> full
    where
    plainID = (\id -> { unitID: id, product: Nothing }) <$> decodeJson json

    full = do
      o <- decodeJson json
      unitID <- o .: "unitID"
      product <- o .: "product"
      pure $ ChargeUnitRef { unitID, product }

instance encodeJsonChargeUnitRef :: EncodeJson ChargeUnitRef where
  encodeJson (ChargeUnitRef x) = case x.product of
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

derive instance eqChargeType :: Eq ChargeType

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

type ConfigSchemaEntryMeta
  = ( title :: Maybe String
    , description :: Maybe String
    )

data ConfigSchemaEntry
  = CseInteger
    { minimum :: Maybe Int
    , maximum :: Maybe Int
    , default :: Maybe Int
    | ConfigSchemaEntryMeta
    }
  | CseString
    { minLength :: Maybe Int
    , maxLength :: Maybe Int
    , enum :: Array String
    , default :: Maybe String
    | ConfigSchemaEntryMeta
    }
  | CseRegex
    { pattern :: String
    , default :: Maybe String
    | ConfigSchemaEntryMeta
    }
  | CseConst
    { const :: ConfigValue
    | ConfigSchemaEntryMeta
    }
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
      title <- o .:? "title"
      description <- o .:? "description"
      case type_ of
        "integer" -> do
          minimum <- o .:? "minimum"
          maximum <- o .:? "maximum"
          default <- o .:? "default"
          Right $ CseInteger { title, description, minimum, maximum, default }
        "string" -> do
          minLength <- o .:? "minLength"
          maxLength <- o .:? "maxLength"
          enum <- o .:? "enum" .!= []
          default <- o .:? "default"
          Right $ CseString { title, description, minLength, maxLength, enum, default }
        "regex" -> do
          pattern <- o .: "pattern"
          default <- o .:? "default"
          Right $ CseRegex { title, description, pattern, default }
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

configSchemaEntryTitle :: ConfigSchemaEntry -> Maybe String
configSchemaEntryTitle = case _ of
  CseInteger x -> x.title
  CseString x -> x.title
  CseRegex x -> x.title
  CseConst x -> x.title
  CseArray _x -> Nothing
  CseObject _x -> Nothing
  CseOneOf _x -> Nothing

configSchemaEntryDescription :: ConfigSchemaEntry -> Maybe String
configSchemaEntryDescription = case _ of
  CseInteger x -> x.description
  CseString x -> x.description
  CseRegex x -> x.description
  CseConst x -> x.description
  CseArray _x -> Nothing
  CseObject _x -> Nothing
  CseOneOf _x -> Nothing

data ConfigValue
  = CvInteger Int
  | CvString String
  | CvArray (Array ConfigValue)
  | CvObject (Map String ConfigValue)
  | CvNull

derive instance eqConfigValue :: Eq ConfigValue

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

newtype ChargeUnit
  = ChargeUnit
  { id :: String
  , name :: Maybe String
  , description :: Maybe String
  , chargeType :: ChargeType
  , priceDimSchema :: Maybe ConfigSchemaEntry
  , reportDimSchemas :: Maybe (Array ConfigSchemaEntry)
  }

derive instance newtypeChargeUnit :: Newtype ChargeUnit _

instance decodeJsonChargeUnit :: DecodeJson ChargeUnit where
  decodeJson json = do
    o <- decodeJson json
    id <- o .: "id"
    name <- o .:? "name"
    description <- o .:? "description"
    chargeType <- o .:? "chargeType" .!= ChargeTypeUsage
    priceDimSchema <- o .:? "priceDimSchema"
    reportDimSchemas <- o .:? "reportDimSchemas"
    pure
      $ ChargeUnit
          { id
          , name
          , description
          , chargeType
          , priceDimSchema
          , reportDimSchemas
          }

instance encodeJsonChargeUnit :: EncodeJson ChargeUnit where
  encodeJson (ChargeUnit x) = encodeJson x

type ChargeUnitMap
  = Map String ChargeUnit

-- | A suitable label for a unit. Uses the unit name, if available, otherwise
-- | its identifier.
chargeUnitLabel :: ChargeUnit -> String
chargeUnitLabel (ChargeUnit { id, name }) = fromMaybe id name

data PriceDimSchema
  = PriceDimSchema ConfigSchemaEntry
  | PriceDimSchemaSuperGroup PriceDimSuperGroupRef

instance decodeJsonPriceDimSchema :: DecodeJson PriceDimSchema where
  decodeJson json =
    (PriceDimSchema <$> decodeJson json)
      <|> (PriceDimSchemaSuperGroup <$> decodeJson json)

instance encodeJsonPriceDimSchema :: EncodeJson PriceDimSchema where
  encodeJson = case _ of
    PriceDimSchema x -> encodeJson x
    PriceDimSchemaSuperGroup x -> encodeJson x

newtype Product
  = Product
  { sku :: String
  , name :: Maybe String
  , description :: Maybe String
  , attr :: Maybe (Map String ConfigValue)
  , orderConfigSchema :: Maybe (Map String ConfigSchemaEntry)
  , assetConfigSchema :: Maybe ConfigSchemaEntry
  , options :: Maybe (Array ProductOption)
  , features :: Maybe (Array ProductFeature)
  , variables :: Maybe (Array ProductVariable)
  , chargeUnits :: Array ChargeUnit
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
    orderConfigSchemaObj :: Maybe (FO.Object ConfigSchemaEntry) <- o .:? "orderConfigSchema"
    let
      attr = (\obj -> Map.fromFoldable (FO.toUnfoldable obj :: Array _)) <$> attrObj

      orderConfigSchema = (\obj -> Map.fromFoldable (FO.toUnfoldable obj :: Array _)) <$> orderConfigSchemaObj
    assetConfigSchema <- o .:? "assetConfigSchema"
    options <- o .:? "options"
    features <- o .:? "features"
    variables <- o .:? "variables"
    chargeUnits <- o .: "chargeUnits"
    rules <- o .:? "rules"
    pure
      $ Product
          { sku
          , name
          , description
          , attr
          , orderConfigSchema
          , assetConfigSchema
          , options
          , features
          , variables
          , chargeUnits
          , rules
          }

instance encodeJsonProduct :: EncodeJson Product where
  encodeJson (Product x) = encodeJson x

-- | Produces a map from unit ID to the charge unit itself.
productChargeUnits :: Product -> ChargeUnitMap
productChargeUnits =
  Map.fromFoldable
    <<< map (\u@(ChargeUnit { id }) -> Tuple id u)
    <<< _.chargeUnits
    <<< unwrap

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
    , platform :: Maybe Platform
    }

instance decodeJsonSku :: DecodeJson Sku where
  decodeJson json = decodeSkuCode <|> decodeSku
    where
    decodeSkuCode = SkuCode <$> decodeJson json

    decodeSku = do
      o <- decodeJson json
      code <- o .: "code"
      name <- o .: "name"
      description <- o .:? "description"
      productCategory <- o .: "productCategory"
      platform <- o .:? "platform"
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
  decodeJson json = decodeProdOptSkuCode <|> decodeProductOption
    where
    decodeProdOptSkuCode = ProdOptSkuCode <$> decodeJson json

    decodeProductOption = do
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
  { basePriceBook :: PriceBookRef
  , charge :: Charge
  , validity :: Validity
  , discountProfileByUnit :: Maybe (Array DiscountProfilePerUnit)
  }

instance decodeJsonPriceOverride :: DecodeJson PriceOverride where
  decodeJson json = PriceOverride <$> decodeJson json

instance encodeJsonPriceOverride :: EncodeJson PriceOverride where
  encodeJson (PriceOverride x) = encodeJson x

newtype DiscountProfilePerUnit
  = DiscountProfilePerUnit
  { unit :: ChargeUnitRef
  , defaultDiscount :: Maybe Discount
  , discountByDim :: Maybe (Array DiscountPerDim)
  }

instance decodeJsonDiscountProfilePerUnit :: DecodeJson DiscountProfilePerUnit where
  decodeJson json = DiscountProfilePerUnit <$> decodeJson json

instance encodeJsonDiscountProfilePerUnit :: EncodeJson DiscountProfilePerUnit where
  encodeJson (DiscountProfilePerUnit x) = encodeJson x

data DiscountPerDim
  = DiscountPerDim
    { dim :: DimValue
    , discount :: Discount
    }
  | DiscountPerDimGroup
    { dimGroup :: PriceDimGroupRef
    , discount :: Discount
    }

instance decodeJsonDiscountPerDim :: DecodeJson DiscountPerDim where
  decodeJson json =
    (DiscountPerDim <$> decodeJson json)
      <|> (DiscountPerDimGroup <$> decodeJson json)

instance encodeJsonDiscountPerDim :: EncodeJson DiscountPerDim where
  encodeJson = case _ of
    DiscountPerDim x -> encodeJson x
    DiscountPerDimGroup x -> encodeJson x

newtype PriceDimGroupRef
  = PriceDimGroupRef
  { priceDimGroupID :: String
  , priceDimSuperGroupID :: String
  }

instance decodeJsonPriceDimGroupRef :: DecodeJson PriceDimGroupRef where
  decodeJson json = PriceDimGroupRef <$> decodeJson json

instance encodeJsonPriceDimGroupRef :: EncodeJson PriceDimGroupRef where
  encodeJson (PriceDimGroupRef x) = encodeJson x

newtype PriceDimSuperGroupRef
  = PriceDimSuperGroupRef
  { priceDimSuperGroupID :: String
  }

instance decodeJsonPriceDimSuperGroupRef :: DecodeJson PriceDimSuperGroupRef where
  decodeJson json = PriceDimSuperGroupRef <$> decodeJson json

instance encodeJsonPriceDimSuperGroupRef :: EncodeJson PriceDimSuperGroupRef where
  encodeJson (PriceDimSuperGroupRef x) = encodeJson x

newtype PriceBookRef
  = PriceBookRef
  { priceBookID :: String
  , version :: String
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
  = OsInDraft
  | OsInReview
  | OsInApproval
  | OsInSignature
  | OsInConfiguration
  | OsInFulfilment
  | OsAborted

instance showOrderStatus :: Show OrderStatus where
  show = case _ of
    OsInDraft -> "InDraft"
    OsInReview -> "InReview"
    OsInApproval -> "InApproval"
    OsInSignature -> "InSignature"
    OsInConfiguration -> "InConfiguration"
    OsInFulfilment -> "InFulfilment"
    OsAborted -> "Aborted"

instance decodeJsonOrderStatus :: DecodeJson OrderStatus where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "InDraft" -> Right OsInDraft
      "InReview" -> Right OsInReview
      "InApproval" -> Right OsInApproval
      "InSignature" -> Right OsInSignature
      "InConfiguration" -> Right OsInConfiguration
      "InFulfilment" -> Right OsInFulfilment
      "Aborted" -> Right OsAborted
      _ -> Left (TypeMismatch "OrderStatus")

instance encodeOrderStatus :: EncodeJson OrderStatus where
  encodeJson = encodeJson <<< show

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
