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
  , EstimatedWAP(..)
  , EstimatedWAPPerSegment(..)
  , EstimatedWAPPerUnit(..)
  , LegalEntity(..)
  , OrderForm(..)
  , OrderLine(..)
  , OrderSection(..)
  , OrderStatus(..)
  , Orders(..)
  , Platform(..)
  , Price(..)
  , PriceBook(..)
  , PriceBookCurrency(..)
  , PriceBookRef(..)
  , PriceBookVersion(..)
  , PriceByUnitPerDim(..)
  , PriceOverride(..)
  , PricePerDim(..)
  , PricePerSegment(..)
  , PricePerUnit(..)
  , PriceSegmentation(..)
  , PriceSegmentationPerUnit(..)
  , Product(..)
  , ProductCatalog(..)
  , ProductCategory(..)
  , ProductFeature(..)
  , ProductOption(..)
  , ProductOptionType(..)
  , ProductRef(..)
  , ProductVariable(..)
  , Purchaser(..)
  , Quantifier(..)
  , QuantityPerDim(..)
  , QuantityPerUnit(..)
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
  , SkuCode(..)
  , Solution(..)
  , Uri(..)
  , Validity(..)
  , chargeUnitLabel
  , configSchemaEntryDescription
  , configSchemaEntryTitle
  , productChargeUnits
  , solutionProducts
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, (.!=), (.:), (.:?), (:=), (~>), (~>?))
import Data.Array as A
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.Lazy (List)
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

derive newtype instance encodeJsonSolution :: EncodeJson Solution

solutionProducts :: Solution -> Map SkuCode Product
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

derive newtype instance encodeJsonRuleConditionExpr :: EncodeJson RuleConditionExpr

newtype Rule
  = Rule
  { severity :: Severity
  , stages :: Array RuleStage
  , quantifier :: Quantifier
  , message :: String
  , conditions :: Array RuleConditionExpr
  }

derive newtype instance decodeJsonRule :: DecodeJson Rule

derive newtype instance encodeJsonRule :: EncodeJson Rule

newtype Currency
  = Currency String

derive instance eqCurrency :: Eq Currency

derive instance newtypeCurrency :: Newtype Currency _

instance showCurrency :: Show Currency where
  show (Currency code) = code

derive newtype instance decodeJsonCurrency :: DecodeJson Currency

derive newtype instance encodeJsonCurrency :: EncodeJson Currency

newtype PriceBook
  = PriceBook
  { id :: String
  , name :: String
  , description :: Maybe String
  , byVersion :: Array PriceBookVersion
  }

derive instance newtypePriceBook :: Newtype PriceBook _

derive newtype instance decodeJsonPriceBook :: DecodeJson PriceBook

derive newtype instance encodeJsonPriceBook :: EncodeJson PriceBook

newtype PriceBookVersion
  = PriceBookVersion
  { version :: Date
  , parent :: Maybe PriceBookRef
  , byCurrency :: Array PriceBookCurrency
  }

derive instance newtypePriceBookVersion :: Newtype PriceBookVersion _

derive newtype instance decodeJsonPriceBookVersion :: DecodeJson PriceBookVersion

derive newtype instance encodeJsonPriceBookVersion :: EncodeJson PriceBookVersion

newtype PriceBookCurrency
  = PriceBookCurrency
  { currency :: Currency
  , rateCards :: Maybe (Array RateCard)
  }

derive instance newtypePriceBookCurrency :: Newtype PriceBookCurrency _

derive newtype instance decodeJsonPriceBookCurrency :: DecodeJson PriceBookCurrency

derive newtype instance encodeJsonPriceBookCurrency :: EncodeJson PriceBookCurrency

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
  , estimatedWAPByUnit :: Array EstimatedWAPPerUnit
  , periodMinimum :: Number
  , termOfPriceChangeInDays :: Int
  }

instance decodeJsonChargeElement :: DecodeJson ChargeElement where
  decodeJson json = rccSimple json <|> rccMixed json
    where
    rccSimple j = do
      o <- decodeJson j
      unit <- o .: "unit"
      SimplePrice pricesByDim <- o .: "price"
      estimatedWAP <- o .:? "estimatedWAP"
      segmentation <- o .:? "segmentation"
      termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
      periodMinimum <- o .:? "periodMinimum" .!= 0.0
      let
        toPriceByUnitByDim (PricePerDim p) =
          PriceByUnitPerDim
            { dim: p.dim
            , prices:
                [ PricePerUnit
                    { unit
                    , currency: Nothing
                    , price: p.price
                    }
                ]
            , periodMinimum: p.periodMinimum
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
            , estimatedWAPByUnit: maybe [] A.singleton estimatedWAP
            , periodMinimum
            , termOfPriceChangeInDays
            }

    rccMixed j = do
      o <- decodeJson j
      units <- o .: "units"
      segmentationByUnit <- o .:? "segmentationByUnit" .!= []
      defaultPriceByUnit <- o .:? "defaultPriceByUnit" .!= []
      priceByUnitByDim <- o .: "priceByUnitByDim"
      estimatedWAPByUnit <- o .:? "estimatedWAPByUnit" .!= []
      periodMinimum <- o .:? "periodMinimum" .!= 0.0
      termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
      pure
        $ ChargeElement
            { units: A.sort units
            , segmentationByUnit
            , defaultPriceByUnit
            , priceByUnitByDim
            , estimatedWAPByUnit: A.sortBy (comparing (_.unit <<< unwrap)) estimatedWAPByUnit
            , periodMinimum
            , termOfPriceChangeInDays
            }

instance encodeJsonChargeElement :: EncodeJson ChargeElement where
  encodeJson = case _ of
    ChargeElement
      { units: [ unit ]
    , segmentationByUnit: [ PriceSegmentationPerUnit { segmentation } ]
    , defaultPriceByUnit: []
    , priceByUnitByDim
    , periodMinimum
    , termOfPriceChangeInDays
    } ->
      encodeJson
        { unit
        , price: SimplePrice $ A.mapMaybe simplePrice priceByUnitByDim
        , segmentation
        , termOfPriceChangeInDays
        , periodMinimum
        }
    ChargeElement
      { units: [ unit ]
    , segmentationByUnit: []
    , defaultPriceByUnit: []
    , priceByUnitByDim
    , periodMinimum
    , termOfPriceChangeInDays
    } ->
      encodeJson
        { unit
        , price: SimplePrice $ A.mapMaybe simplePrice priceByUnitByDim
        , termOfPriceChangeInDays
        , periodMinimum
        }
    ChargeElement x -> encodeJson x
    where
    simplePrice = case _ of
      PriceByUnitPerDim
        { dim
      , prices: [ PricePerUnit { price } ]
      , periodMinimum
      } -> Just $ PricePerDim { dim, price, periodMinimum }
      _ -> Nothing

newtype SimplePrice
  = SimplePrice (Array PricePerDim)

instance decodeJsonSimplePrice :: DecodeJson SimplePrice where
  decodeJson json = segmented <|> byDim
    where
    segmented = do
      price <- decodeJson json
      pure
        $ SimplePrice
            [ PricePerDim
                { dim: DimValue CvNull
                , price
                , periodMinimum: 0.0
                }
            ]

    byDim = SimplePrice <$> decodeJson json

instance encodeJsonSimplePrice :: EncodeJson SimplePrice where
  encodeJson ( SimplePrice
      [ PricePerDim
      { dim: DimValue CvNull
    , price: price
    , periodMinimum: 0.0
    }
    ]
  ) = encodeJson price
  encodeJson (SimplePrice x) = encodeJson x

newtype DimValue
  = DimValue ConfigValue

derive instance genericDimValue :: Generic DimValue _

derive instance eqDimValue :: Eq DimValue

derive instance ordDimValue :: Ord DimValue

derive newtype instance showDimValue :: Show DimValue

derive newtype instance decodeJsonDimValue :: DecodeJson DimValue

derive newtype instance encodeJsonDimValue :: EncodeJson DimValue

newtype PricePerDim
  = PricePerDim
  { dim :: DimValue
  , price :: Price
  , periodMinimum :: Number
  }

instance decodeJsonPricePerDim :: DecodeJson PricePerDim where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    price <- o .: "price"
    periodMinimum <- o .:? "periodMinimum" .!= 0.0
    pure $ PricePerDim { dim, price, periodMinimum }

derive newtype instance encodeJsonPricePerDim :: EncodeJson PricePerDim

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
  , salesPrice :: Number
  , discount :: Maybe Discount
  }

derive instance genericPricePerSegment :: Generic PricePerSegment _

derive instance eqPricePerSegment :: Eq PricePerSegment

derive instance newtypePricePerSegment :: Newtype PricePerSegment _

derive newtype instance showPricePerSegment :: Show PricePerSegment

instance decodeJsonPricePerSegment :: DecodeJson PricePerSegment where
  decodeJson json = do
    o <- decodeJson json
    minimum <- o .: "minimum"
    exclusiveMaximum <- o .:? "exclusiveMaximum"
    let
      basicPrice = do
        p <- o .: "price"
        pure { listPrice: p, salesPrice: p, discount: Nothing }

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
  encodeJson (PricePerSegment pps) =
    ("minimum" := pps.minimum)
      ~> ((\x -> "exclusiveMaximum" := x) <$> pps.exclusiveMaximum)
      ~>? ("price" := price)
      ~> jsonEmptyObject
    where
    price
      | isNothing pps.discount && pps.listPrice == pps.salesPrice = encodeJson pps.listPrice
      | otherwise =
        ("listPrice" := pps.listPrice)
          ~> ("salesPrice" := pps.salesPrice)
          ~> ((\x -> "discount" := x) <$> pps.discount)
          ~>? jsonEmptyObject

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

derive newtype instance decodeJsonPriceSegmentation :: DecodeJson PriceSegmentation

derive newtype instance encodeJsonPriceSegmentation :: EncodeJson PriceSegmentation

newtype PriceSegmentationPerUnit
  = PriceSegmentationPerUnit
  { unit :: ChargeUnitRef
  , segmentation :: PriceSegmentation
  }

derive newtype instance decodeJsonPriceSegmentationPerUnit :: DecodeJson PriceSegmentationPerUnit

derive newtype instance encodeJsonPriceSegmentationPerUnit :: EncodeJson PriceSegmentationPerUnit

newtype RateCard
  = RateCard
  { sku :: SkuCode
  , name :: Maybe String
  , description :: Maybe String
  , charge :: Charge
  }

derive newtype instance decodeJsonRateCard :: DecodeJson RateCard

derive newtype instance encodeJsonRateCard :: EncodeJson RateCard

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
            , salesPrice: p
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
                , salesPrice: salesPrice
                , discount
                }
            ]

    segmented = (Price <<< sortSegments) <$> decodeJson json

    sortSegments = A.sortBy $ comparing (_.minimum <<< unwrap)

derive newtype instance encodeJsonPrice :: EncodeJson Price

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

newtype EstimatedWAPPerSegment
  = EstimatedWAPPerSegment
  { minimum :: Int
  , exclusiveMaximum :: Maybe Int
  , price :: Number
  }

instance decodeJsonEstimatedWAPPerSegment :: DecodeJson EstimatedWAPPerSegment where
  decodeJson json = EstimatedWAPPerSegment <$> decodeJson json

instance encodeJsonEstimatedWAPPerSegment :: EncodeJson EstimatedWAPPerSegment where
  encodeJson (EstimatedWAPPerSegment x) = encodeJson x

newtype EstimatedWAP
  = EstimatedWAP (Array EstimatedWAPPerSegment)

instance decodeJsonEstimatedWAP :: DecodeJson EstimatedWAP where
  decodeJson json = primitive <|> object
    where
    mk price = EstimatedWAPPerSegment { minimum: 0, exclusiveMaximum: Nothing, price }

    primitive = (EstimatedWAP <<< A.singleton <<< mk) <$> decodeJson json

    object = EstimatedWAP <$> decodeJson json

instance encodeJsonEstimatedWAP :: EncodeJson EstimatedWAP where
  encodeJson (EstimatedWAP x) = case x of
    [ EstimatedWAPPerSegment { minimum: 0, exclusiveMaximum: Nothing, price } ] -> encodeJson price
    _ -> encodeJson x

newtype EstimatedWAPPerUnit
  = EstimatedWAPPerUnit
  { unit :: ChargeUnitRef
  , wap :: EstimatedWAP
  }

derive instance newtypeEstimatedWAPPerUnit :: Newtype EstimatedWAPPerUnit _

derive newtype instance decodeJsonEstimatedWAPPerUnit :: DecodeJson EstimatedWAPPerUnit

derive newtype instance encodeJsonEstimatedWAPPerUnit :: EncodeJson EstimatedWAPPerUnit

newtype DefaultPricePerUnit
  = DefaultPricePerUnit
  { unit :: ChargeUnitRef
  , currency :: Maybe Currency
  , price :: Number
  }

derive newtype instance decodeJsonDefaultPricePerUnit :: DecodeJson DefaultPricePerUnit

derive newtype instance encodeJsonDefaultPricePerUnit :: EncodeJson DefaultPricePerUnit

newtype PriceByUnitPerDim
  = PriceByUnitPerDim
  { dim :: DimValue
  , prices :: Array PricePerUnit
  , periodMinimum :: Number
  }

instance decodeJsonPriceByUnitPerDim :: DecodeJson PriceByUnitPerDim where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    prices <- o .: "prices"
    periodMinimum <- o .:? "periodMinimum" .!= 0.0
    pure
      $ PriceByUnitPerDim
          { dim
          , prices: A.sortBy (comparing (_.unit <<< unwrap)) prices
          , periodMinimum
          }

instance encodeJsonPriceByUnitPerDim :: EncodeJson PriceByUnitPerDim where
  encodeJson (PriceByUnitPerDim x) = encodeJson x

newtype PricePerUnit
  = PricePerUnit
  { unit :: ChargeUnitRef
  , currency :: Maybe Currency
  , price :: Price
  }

derive instance newtypePricePerUnit :: Newtype PricePerUnit _

derive newtype instance decodeJsonPricePerUnit :: DecodeJson PricePerUnit

instance encodeJsonPricePerUnit :: EncodeJson PricePerUnit where
  encodeJson (PricePerUnit ppu) =
    ("unit" := ppu.unit)
      ~> ((\x -> "currency" := x) <$> ppu.currency)
      ~>? ("price" := ppu.price)
      ~> jsonEmptyObject

newtype ChargeUnitRef
  = ChargeUnitRef { unitID :: String, product :: Maybe ProductRef }

derive instance genericChargeUnitRef :: Generic ChargeUnitRef _

derive instance eqChargeUnitRef :: Eq ChargeUnitRef

derive instance ordChargeUnitRef :: Ord ChargeUnitRef

derive instance newtypeChargeUnitRef :: Newtype ChargeUnitRef _

derive newtype instance showChargeUnitRef :: Show ChargeUnitRef

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
  = ProductRef { sku :: SkuCode, solutionURI :: Maybe Uri }

derive instance genericProductRef :: Generic ProductRef _

derive instance eqProductRef :: Eq ProductRef

derive instance ordProductRef :: Ord ProductRef

derive newtype instance showProductRef :: Show ProductRef

derive newtype instance decodeJsonProductRef :: DecodeJson ProductRef

derive newtype instance encodeJsonProductRef :: EncodeJson ProductRef

data ChargeType
  = ChargeTypeOnetime
  | ChargeTypeMonthly
  | ChargeTypeQuarterly
  | ChargeTypeUsage
  | ChargeTypeSegment
  | ChargeTypeQuarterlySegment

derive instance eqChargeType :: Eq ChargeType

instance showChargeType :: Show ChargeType where
  show = case _ of
    ChargeTypeOnetime -> "Onetime"
    ChargeTypeMonthly -> "Monthly"
    ChargeTypeQuarterly -> "Quarterly"
    ChargeTypeUsage -> "Usage"
    ChargeTypeSegment -> "MonthlySegment"
    ChargeTypeQuarterlySegment -> "QuarterlySegment"

instance decodeJsonChargeType :: DecodeJson ChargeType where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Onetime" -> Right ChargeTypeOnetime
      "Monthly" -> Right ChargeTypeMonthly
      "Quarterly" -> Right ChargeTypeQuarterly
      "Usage" -> Right ChargeTypeUsage
      "MonthlySegment" -> Right ChargeTypeSegment
      "QuarterlySegment" -> Right ChargeTypeQuarterlySegment
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
  decodeJson json = typed <|> constValue <|> oneOf <|> Left (TypeMismatch "Schema")
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
    CseObject x -> ("type" := "object")
                   ~> ("properties" := encodeJson (FO.fromFoldable (Map.toUnfoldable x.properties :: List _)))
                   ~> jsonEmptyObject
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

derive instance ordConfigValue :: Ord ConfigValue

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
  encodeJson (CvObject v) = encodeJson $ FO.fromFoldable (Map.toUnfoldable v :: List _)
  encodeJson (CvNull) = encodeJson (Nothing :: Maybe Int)

-- TODO: Add `schema` and `variable`.
newtype ProductVariable
  = ProductVariable
  { name :: String, path :: String
  }

derive newtype instance decodeJsonProductVariable :: DecodeJson ProductVariable

derive newtype instance encodeJsonProductVariable :: EncodeJson ProductVariable

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

derive newtype instance encodeJsonChargeUnit :: EncodeJson ChargeUnit

type ChargeUnitMap
  = Map String ChargeUnit

-- | A suitable label for a unit. Uses the unit name, if available, otherwise
-- | its identifier.
chargeUnitLabel :: ChargeUnit -> String
chargeUnitLabel (ChargeUnit { id, name }) = fromMaybe id name

newtype PriceDimSchema
  = PriceDimSchema ConfigSchemaEntry

derive newtype instance decodeJsonPriceDimSchema :: DecodeJson PriceDimSchema

derive newtype instance encodeJsonPriceDimSchema :: EncodeJson PriceDimSchema

newtype Product
  = Product
  { sku :: SkuCode
  , productCategory :: ProductCategory
  , platform :: Maybe Platform
  , name :: Maybe String
  , description :: Maybe String
  , attr :: Maybe (Map String ConfigValue)
  , orderConfigSchema :: Maybe ConfigSchemaEntry
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
    productCategory <- o .: "productCategory"
    platform <- o .:? "platform"
    name <- o .:? "name"
    description <- o .:? "description"
    attrObj :: Maybe (FO.Object ConfigValue) <- o .:? "attr"
    let
      attr = (\obj -> Map.fromFoldable (FO.toUnfoldable obj :: Array _)) <$> attrObj
    orderConfigSchema <- o .:? "orderConfigSchema"
    assetConfigSchema <- o .:? "assetConfigSchema"
    options <- o .:? "options"
    features <- o .:? "features"
    variables <- o .:? "variables"
    chargeUnits <- o .: "chargeUnits"
    rules <- o .:? "rules"
    pure
      $ Product
          { sku
          , productCategory
          , platform
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

derive newtype instance encodeJsonProduct :: EncodeJson Product

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

derive instance eqPlatform :: Eq Platform

derive instance ordPlatform :: Ord Platform

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

derive instance eqProductCategory :: Eq ProductCategory

derive instance ordProductCategory :: Ord ProductCategory

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

newtype SkuCode
  = SkuCode String

derive instance eqSkuCode :: Eq SkuCode

derive instance ordSkuCode :: Ord SkuCode

instance showSkuCode :: Show SkuCode where
  show (SkuCode code) = code

derive newtype instance decodeJsonSkuCode :: DecodeJson SkuCode

derive newtype instance encodeJsonSkuCode :: EncodeJson SkuCode

data ProductOption
  = ProdOptSkuCode String
  | ProductOption
    { sku :: SkuCode
    , name :: Maybe String
    , required :: Boolean
    , quoteLineVisible :: Boolean
    , quantity :: Int
    , minQuantity :: Int
    , maxQuantity :: Int
    , requiredOptions :: Maybe (Array SkuCode)
    , excludeOptions :: Maybe (Array SkuCode)
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

derive newtype instance decodeJsonProductFeature :: DecodeJson ProductFeature

derive newtype instance encodeJsonProductFeature :: EncodeJson ProductFeature

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

derive newtype instance decodeJsonCommercial :: DecodeJson Commercial

derive newtype instance encodeJsonCommercial :: EncodeJson Commercial

newtype Address
  = Address String

derive newtype instance decodeJsonAddress :: DecodeJson Address

derive newtype instance encodeJsonAddress :: EncodeJson Address

newtype Contact
  = Contact
  { email :: String
  , name :: String
  , phone :: String
  }

derive newtype instance decodeJsonContact :: DecodeJson Contact

derive newtype instance encodeJsonContact :: EncodeJson Contact

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

derive newtype instance decodeJsonPurchaser :: DecodeJson Purchaser

derive newtype instance encodeJsonPurchaser :: EncodeJson Purchaser

newtype LegalEntity
  = LegalEntity
  { name :: String
  , address :: Address
  , country :: String
  }

derive newtype instance decodeJsonLegalEntity :: DecodeJson LegalEntity

derive newtype instance encodeJsonLegalEntity :: EncodeJson LegalEntity

newtype Seller
  = Seller
  { contacts :: { primary :: Contact, finance :: Contact, support :: Contact }
  , legalEntity :: LegalEntity
  }

derive newtype instance decodeJsonSeller :: DecodeJson Seller

derive newtype instance encodeJsonSeller :: EncodeJson Seller

newtype BillingAccountRef
  = BillingAccountRef
  { billingAccountID :: String
  }

derive newtype instance decodeJsonBillingAccountRef :: DecodeJson BillingAccountRef

derive newtype instance encodeJsonBillingAccountRef :: EncodeJson BillingAccountRef

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

newtype DateTime
  = DateTime String

derive newtype instance decodeDateTime :: DecodeJson DateTime

derive newtype instance encodeJsonDateTime :: EncodeJson DateTime

-- TODO: Add `configs`
newtype Asset
  = Asset
  { sku :: SkuCode
  -- , configs :: Array ConfigValue
  , billingAccount :: BillingAccountRef
  , createTime :: DateTime
  , updateTime :: DateTime
  , priceOverrides :: Array PriceOverride
  }

derive newtype instance decodeJsonAsset :: DecodeJson Asset

derive newtype instance encodeJsonAsset :: EncodeJson Asset

newtype PriceOverride
  = PriceOverride
  { basePriceBook :: PriceBookRef
  , charge :: Charge
  , validity :: Validity
  , discountProfileByUnit :: Maybe (Array DiscountProfilePerUnit)
  }

derive newtype instance decodeJsonPriceOverride :: DecodeJson PriceOverride

derive newtype instance encodeJsonPriceOverride :: EncodeJson PriceOverride

newtype DiscountProfilePerUnit
  = DiscountProfilePerUnit
  { unit :: ChargeUnitRef
  , defaultDiscount :: Maybe Discount
  , discountByDim :: Maybe (Array DiscountPerDim)
  }

derive newtype instance decodeJsonDiscountProfilePerUnit :: DecodeJson DiscountProfilePerUnit

derive newtype instance encodeJsonDiscountProfilePerUnit :: EncodeJson DiscountProfilePerUnit

newtype DiscountPerDim
  = DiscountPerDim
  { dim :: DimValue
  , discount :: Discount
  }

derive newtype instance decodeJsonDiscountPerDim :: DecodeJson DiscountPerDim

derive newtype instance encodeJsonDiscountPerDim :: EncodeJson DiscountPerDim

newtype PriceBookRef
  = PriceBookRef
  { priceBookID :: String
  , version :: String
  , solutionURI :: Maybe Uri
  }

derive newtype instance decodeJsonPriceBookRef :: DecodeJson PriceBookRef

instance encodeJsonPriceBookRef :: EncodeJson PriceBookRef where
  encodeJson (PriceBookRef x) =
    ("priceBookID" := x.priceBookID)
      ~> ("version" := x.version)
      ~> ((\uri -> "solutionURI" := uri) <$> x.solutionURI)
      ~>? jsonEmptyObject

newtype SalesforceAccountRef
  = SalesforceAccountRef
  { salesforceAccountID :: String
  }

derive newtype instance decodeJsonSalesforceAccountRef :: DecodeJson SalesforceAccountRef

derive newtype instance encodeJsonSalesforceAccountRef :: EncodeJson SalesforceAccountRef

newtype ReturnCustomerData
  = ReturnCustomerData
  { assets :: Array Asset
  , salesforceAccountRef :: SalesforceAccountRef
  }

derive newtype instance decodeJsonReturnCustomerData :: DecodeJson ReturnCustomerData

derive newtype instance encodeJsonReturnCustomerData :: EncodeJson ReturnCustomerData

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

derive instance eqSegment :: Eq Segment

derive instance ordSegment :: Ord Segment

derive newtype instance decodeJsonSegment :: DecodeJson Segment

instance encodeJsonSegment :: EncodeJson Segment where
  encodeJson (Segment { minimum, exclusiveMaximum }) =
    ("minimum" := minimum)
      ~> ((\x -> "exclusiveMaximum" := x) <$> exclusiveMaximum)
      ~>? jsonEmptyObject

newtype QuantityPerDim
  = QuantityPerDim
  { dim :: DimValue
  , quantity :: Int
  , estimated :: Boolean
  }

instance decodeJsonQuantityPerDim :: DecodeJson QuantityPerDim where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    quantity <- o .: "quantity"
    estimated <- o .:? "estimated" .!= false
    pure $ QuantityPerDim { dim, quantity, estimated }

instance encodeJsonQuantityPerDim :: EncodeJson QuantityPerDim where
  encodeJson (QuantityPerDim qpd) =
    ("dim" := qpd.dim)
      ~> ("quantity" := qpd.quantity)
      ~> (if qpd.estimated then Just ("estimated" := qpd.estimated) else Nothing)
      ~>? jsonEmptyObject

data QuantityPerUnit
  = QuantityPerUnit
    { unit :: ChargeUnitRef
    , quantity :: Int
    , estimated :: Boolean
    }
  | QuantityByDimPerUnit
    { unit :: ChargeUnitRef
    , quantityByDim :: Array QuantityPerDim
    }

instance decodeJsonQuantityPerUnit :: DecodeJson QuantityPerUnit where
  decodeJson json = perUnit <|> perUnitByDim
    where
    perUnit = do
      o <- decodeJson json
      unit <- o .: "unit"
      quantity <- o .: "quantity"
      estimated <- o .:? "estimated" .!= false
      pure $ QuantityPerUnit { unit, quantity, estimated }

    perUnitByDim = QuantityByDimPerUnit <$> decodeJson json

instance encodeJsonQuantityPerUnit :: EncodeJson QuantityPerUnit where
  encodeJson = case _ of
    QuantityPerUnit x -> encodeJson x
    QuantityByDimPerUnit x -> encodeJson x

mkQuantityPerUnitFromPrimitive :: Int -> QuantityPerUnit
mkQuantityPerUnitFromPrimitive quantity =
  QuantityByDimPerUnit
    { unit: ChargeUnitRef { unitID: "", product: Nothing }
    , quantityByDim:
        [ QuantityPerDim
            { dim: DimValue CvNull
            , quantity: quantity
            , estimated: false
            }
        ]
    }

newtype OrderLine
  = OrderLine
  { sku :: SkuCode
  , charge :: Charge
  , quantity :: Array QuantityPerUnit
  , configs :: Array ConfigValue
  }

derive instance newtypeOrderLine :: Newtype OrderLine _

instance decodeJsonOrderLine :: DecodeJson OrderLine where
  decodeJson json = do
    o <- decodeJson json
    sku <- o .: "sku"
    charge <- o .: "charge"
    quantity <- decodeQuantity =<< o .: "quantity"
    configs <- o .:? "configs" .!= []
    pure $ OrderLine { sku, charge, quantity, configs }
    where
    decodeQuantity qJson =
      -- A primitive integer quantity.
      (A.singleton <<< mkQuantityPerUnitFromPrimitive) <$> decodeJson qJson
        <|> decodeJson qJson

instance encodeJsonOrderLine :: EncodeJson OrderLine where
  encodeJson (OrderLine x) =
    ("sku" := x.sku)
      ~> ("charge" := x.charge)
      ~> ("quantity" := x.quantity)
      ~> ("configs" := x.configs)
      ~> jsonEmptyObject

newtype OrderSection
  = OrderSection
  { solutionURI :: Uri
  , basePriceBook :: PriceBookRef
  , orderLines :: Array OrderLine
  }

derive newtype instance decodeJsonOrderSection :: DecodeJson OrderSection

derive newtype instance encodeJsonOrderSection :: EncodeJson OrderSection

newtype OrderForm
  = OrderForm
  { id :: String
  , status :: OrderStatus
  , customer :: Customer
  , sections :: Array OrderSection
  }

derive newtype instance decodeJsonOrderForm :: DecodeJson OrderForm

derive newtype instance encodeJsonOrderForm :: EncodeJson OrderForm

newtype Orders
  = Orders
  { items :: Array OrderForm
  , page :: Int
  , last :: Boolean
  }

derive newtype instance decodeJsonOrders :: DecodeJson Orders

derive newtype instance encodeJsonOrders :: EncodeJson Orders
