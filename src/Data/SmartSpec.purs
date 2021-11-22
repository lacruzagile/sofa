module Data.SmartSpec
  ( Address(..)
  , Asset(..)
  , BillingAccountRef(..)
  , BillingCurrency(..)
  , BillingOption(..)
  , Buyer(..)
  , Charge(..)
  , ChargeCurrency(..)
  , ChargeCurrencyPerUnit(..)
  , ChargeKind(..)
  , ChargeSingleUnit(..)
  , ChargeUnit(..)
  , ChargeUnitId(..)
  , Commercial(..)
  , ConfigSchemaEntry(..)
  , ConfigSchemaEntryMeta
  , ConfigValue(..)
  , Contact(..)
  , ContractTerm(..)
  , Country(..)
  , Currency(..)
  , Customer(..)
  , Date(..)
  , DateTime(..)
  , DefaultPricePerUnit(..)
  , DimValue(..)
  , Discount(..)
  , DiscountPerDim(..)
  , DiscountProfilePerUnit(..)
  , EstimatedUsage(..)
  , EstimatedUsagePerDim(..)
  , EstimatedWap(..)
  , EstimatedWapPerSegment(..)
  , EstimatedWapPerUnit(..)
  , LegalEntities(..)
  , LegalEntity(..)
  , LegalEntityTraffic(..)
  , OrderForm(..)
  , OrderLine(..)
  , OrderLineConfig(..)
  , OrderLineStatus(..)
  , OrderSection(..)
  , OrderStatus(..)
  , Orders(..)
  , PaymentCurrency(..)
  , Platform(..)
  , Price(..)
  , PriceBook(..)
  , PriceBookCurrency(..)
  , PriceBookRef(..)
  , PriceBookVersion(..)
  , PriceByUnitPerDim(..)
  , PriceOverride(..)
  , PricePerDim(..)
  , PricePerDimSeg(..)
  , PricePerDimUnit(..)
  , PricePerDimUnitOptSeg(..)
  , PricePerDimUnitSeg(..)
  , PricePerSeg(..)
  , PricePerUnit(..)
  , PricePerUnitSeg(..)
  , PriceRow(..)
  , PricingCurrency(..)
  , Product(..)
  , ProductCatalog(..)
  , ProductCategory(..)
  , ProductFeature(..)
  , ProductOption(..)
  , ProductOptionType(..)
  , Quantifier(..)
  , RateCard(..)
  , ReturnCustomerCommercial(..)
  , ReturnCustomerData(..)
  , Rule(..)
  , RuleConditionExpr(..)
  , RuleStage(..)
  , SalesforceAccountRef(..)
  , Segment(..)
  , Segmentation(..)
  , SegmentationDim(..)
  , SegmentationDimPerUnit(..)
  , SegmentationModel(..)
  , SegmentationOptDim(..)
  , SegmentationOptDimPerUnit(..)
  , SegmentationPerUnit(..)
  , SegmentsPerDim(..)
  , Seller(..)
  , Severity(..)
  , SkuCode(..)
  , Solution(..)
  , Subdivision(..)
  , Uri(..)
  , Validity(..)
  , configSchemaEntryDescription
  , configSchemaEntryTitle
  , countryRegex
  , emptyAddress
  , emptyContact
  , solutionProducts
  , subdivisionRegex
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, (.!=), (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Array as A
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.Lazy (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.String as S
import Data.String.Regex as Re
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen as QC

type Uri
  = String

newtype Solution
  = Solution
  { id :: String
  , uri :: Maybe Uri
  , title :: Maybe String
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
    title <- o .:? "title"
    description <- o .:? "description"
    products <- o .: "products"
    rules <- o .:? "rules" .!= []
    priceBooks <- o .: "priceBooks"
    pure $ Solution { id, uri, title, description, rules, products, priceBooks }

derive newtype instance encodeJsonSolution :: EncodeJson Solution

solutionProducts :: Solution -> Map SkuCode Product
solutionProducts =
  Map.fromFoldable
    <<< map (\p@(Product { sku }) -> Tuple sku p)
    <<< _.products
    <<< unwrap

newtype ProductCatalog
  = ProductCatalog
  { title :: Maybe String
  , description :: Maybe String
  , solutions :: Map String Solution
  }

instance decodeJsonProductCatalog :: DecodeJson ProductCatalog where
  decodeJson json = do
    o <- decodeJson json
    title <- o .:? "title"
    description <- o .:? "description"
    solutionsObj :: FO.Object Solution <- o .: "solutions"
    let
      solutions = Map.fromFoldable (FO.toUnfoldable solutionsObj :: Array _)
    pure
      $ ProductCatalog
          { title
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

newtype Country
  = Country String

countryRegex :: Re.Regex
countryRegex = unsafeRegex "^[A-Z]{2}$" mempty

derive instance eqCountry :: Eq Country

derive instance ordCountry :: Ord Country

derive instance newtypeCountry :: Newtype Country _

instance showCountry :: Show Country where
  show (Country code) = code

instance decodeJsonCountry :: DecodeJson Country where
  decodeJson json = do
    s <- decodeJson json
    if check s then pure (Country s) else error
    where
    check = Re.test countryRegex

    error = Left (TypeMismatch "Country (ISO 3166-1 alpha-2 code)")

derive newtype instance encodeJsonCountry :: EncodeJson Country

newtype Subdivision
  = Subdivision String

subdivisionRegex :: Re.Regex
subdivisionRegex = unsafeRegex "^[0-9A-Z]{1,3}$" mempty

derive instance eqSubdivision :: Eq Subdivision

derive instance ordSubdivision :: Ord Subdivision

derive instance newtypeSubdivision :: Newtype Subdivision _

instance showSubdivision :: Show Subdivision where
  show (Subdivision code) = code

instance decodeJsonSubdivision :: DecodeJson Subdivision where
  decodeJson json = do
    s <- decodeJson json
    if check s then pure (Subdivision s) else error
    where
    check = Re.test subdivisionRegex

    error = Left (TypeMismatch "Subdivision (ISO 3166-2 subdivision code)")

derive newtype instance encodeJsonSubdivision :: EncodeJson Subdivision

newtype Currency
  = Currency String

derive instance eqCurrency :: Eq Currency

derive instance ordCurrency :: Ord Currency

derive instance newtypeCurrency :: Newtype Currency _

instance showCurrency :: Show Currency where
  show (Currency code) = code

instance decodeJsonCurrency :: DecodeJson Currency where
  decodeJson json = do
    s <- decodeJson json
    if check s then pure (Currency s) else error
    where
    check = Re.test (unsafeRegex "^[A-Z]{3}$" mempty)

    error = Left (TypeMismatch "Currency (ISO 4217 alphabetic code)")

derive newtype instance encodeJsonCurrency :: EncodeJson Currency

-- | The billing currency. This is the currency that is used for the buyer's
-- | invoice. It is always the same as the pricing currency and is therefore an
-- | alias.
type BillingCurrency
  = PricingCurrency

-- | The payment currency. This is the currency that the buyer actually use to
-- | pay their invoice.
newtype PaymentCurrency
  = PaymentCurrency Currency

derive instance newtypePaymentCurrency :: Newtype PaymentCurrency _

derive newtype instance eqPaymentCurrency :: Eq PaymentCurrency

derive newtype instance showPaymentCurrency :: Show PaymentCurrency

derive newtype instance decodeJsonPaymentCurrency :: DecodeJson PaymentCurrency

derive newtype instance encodeJsonPaymentCurrency :: EncodeJson PaymentCurrency

-- | The pricing currency. This is the currency that the billing process
-- | (rating, invoicing) is based on. For a price book, the rate cards are
-- | grouped together by the pricing currency.
newtype PricingCurrency
  = PricingCurrency Currency

derive instance newtypePricingCurrency :: Newtype PricingCurrency _

derive newtype instance eqPricingCurrency :: Eq PricingCurrency

derive newtype instance showPricingCurrency :: Show PricingCurrency

derive newtype instance decodeJsonPricingCurrency :: DecodeJson PricingCurrency

derive newtype instance encodeJsonPricingCurrency :: EncodeJson PricingCurrency

-- | The charge currency. This is the currency that is used for charges within a
-- | rate card.
newtype ChargeCurrency
  = ChargeCurrency Currency

derive instance ordChargeCurrency :: Ord ChargeCurrency

derive instance newtypeChargeCurrency :: Newtype ChargeCurrency _

derive newtype instance eqChargeCurrency :: Eq ChargeCurrency

derive newtype instance showChargeCurrency :: Show ChargeCurrency

derive newtype instance decodeJsonChargeCurrency :: DecodeJson ChargeCurrency

derive newtype instance encodeJsonChargeCurrency :: EncodeJson ChargeCurrency

newtype ChargeCurrencyPerUnit
  = ChargeCurrencyPerUnit
  { unit :: ChargeUnitId
  , currency :: ChargeCurrency
  }

derive newtype instance decodeJsonChargeCurrencyPerUnit :: DecodeJson ChargeCurrencyPerUnit

derive newtype instance encodeJsonChargeCurrencyPerUnit :: EncodeJson ChargeCurrencyPerUnit

newtype PriceBook
  = PriceBook
  { id :: String
  , title :: String
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
  { currency :: PricingCurrency
  , rateCards :: Maybe (Array RateCard)
  }

derive instance newtypePriceBookCurrency :: Newtype PriceBookCurrency _

derive newtype instance decodeJsonPriceBookCurrency :: DecodeJson PriceBookCurrency

derive newtype instance encodeJsonPriceBookCurrency :: EncodeJson PriceBookCurrency

-- | Single unit charges.
data ChargeSingleUnit
  = ChargeSimple
    { unit :: ChargeUnitId
    , currency :: Maybe ChargeCurrency
    , description :: Maybe String
    , listPrice :: Number
    , price :: Number
    , discount :: Maybe Discount
    , periodMinimum :: Maybe Number
    }
  | ChargeDim
    { unit :: ChargeUnitId
    , currency :: Maybe ChargeCurrency
    , description :: Maybe String
    , priceByDim :: Array PricePerDim
    , defaultPrice :: Maybe Price
    , periodMinimum :: Maybe Number
    }
  | ChargeSeg
    { unit :: ChargeUnitId
    , currency :: Maybe ChargeCurrency
    , description :: Maybe String
    , segmentation :: Segmentation
    , priceBySegment :: Array PricePerSeg
    , periodMinimum :: Maybe Number
    }
  | ChargeDimSeg
    { unit :: ChargeUnitId
    , currency :: Maybe ChargeCurrency
    , description :: Maybe String
    , segmentation :: SegmentationOptDim
    , priceBySegmentByDim :: Array PricePerDimSeg
    , defaultPrice :: Maybe Price
    , periodMinimum :: Maybe Number
    }

instance decodeJsonChargeSingleUnit :: DecodeJson ChargeSingleUnit where
  decodeJson json =
    decodeChargeSimple
      <|> (ChargeDim <$> decodeJson json)
      <|> (ChargeSeg <$> decodeJson json)
      <|> (ChargeDimSeg <$> decodeJson json)
    where
    decodeChargeSimple = do
      o <- decodeJson json
      unit <- o .: "unit"
      currency <- o .:? "currency"
      description <- o .:? "description"
      price <- o .: "price"
      listPrice <- o .:? "listPrice" .!= price
      discount <- o .:? "discount"
      periodMinimum <- o .:? "periodMinimum"
      pure
        $ ChargeSimple
            { unit
            , currency
            , description
            , price
            , listPrice
            , discount
            , periodMinimum
            }

instance encodeJsonChargeSingleUnit :: EncodeJson ChargeSingleUnit where
  encodeJson = case _ of
    ChargeSimple x ->
      encodeCommon x
        $ encodePriceRowJson x
        $ jsonEmptyObject
    ChargeDim x ->
      encodeCommon x
        $ ("priceByDim" := x.priceByDim)
        ~> jsonEmptyObject
    ChargeSeg x ->
      encodeCommon x
        $ ("segmentation" := x.segmentation)
        ~> ("priceBySegment" := x.priceBySegment)
        ~> jsonEmptyObject
    ChargeDimSeg x ->
      encodeCommon x
        $ ("segmentation" := x.segmentation)
        ~> ("priceBySegmentByDim" := x.priceBySegmentByDim)
        ~> ("defaultPrice" := x.defaultPrice)
        ~> jsonEmptyObject
    where
    encodeCommon ::
      forall r a.
      EncodeJson a =>
      { unit :: ChargeUnitId
      , currency :: Maybe ChargeCurrency
      , description :: Maybe String
      , periodMinimum :: Maybe Number
      | r
      } ->
      a -> Json
    encodeCommon x rest =
      ("unit" := x.unit)
        ~> ("currency" :=? x.currency)
        ~>? ("description" :=? x.description)
        ~>? ("periodMinimum" :=? x.periodMinimum)
        ~>? rest

data Charge
  = ChargeSingleUnit ChargeSingleUnit
  | ChargeList
    { description :: Maybe String
    , charges :: Array ChargeSingleUnit
    , periodMinimum :: Maybe Number
    }
  | ChargeDimUnitOptSeg
    { units :: Array ChargeUnitId
    , currencyByUnit :: Array ChargeCurrencyPerUnit
    , description :: Maybe String
    , segmentationByUnit :: Array SegmentationOptDimPerUnit
    , priceByUnitByDim :: Array PricePerDimUnitOptSeg
    , defaultPriceByUnit :: Array DefaultPricePerUnit
    , periodMinimum :: Maybe Number
    -- estimatedWAPByUnit :: Maybe _ // FIXME
    }

instance decodeJsonCharge :: DecodeJson Charge where
  decodeJson json =
    (ChargeSingleUnit <$> decodeJson json)
      <|> (ChargeList <$> decodeJson json)
      <|> decodeChargeDimUnitOptSeg
    where
    decodeChargeDimUnitOptSeg = do
      o <- decodeJson json
      units <- o .: "units"
      currencyByUnit <- o .:? "currencyByUnit" .!= []
      description <- o .:? "description"
      segmentationByUnit <- o .:? "segmentationByUnit" .!= []
      priceByUnitByDim <- o .: "priceByUnitByDim"
      defaultPriceByUnit <- o .:? "defaultPriceByUnit" .!= []
      periodMinimum <- o .:? "periodMinimum"
      pure
        $ ChargeDimUnitOptSeg
            { units
            , currencyByUnit
            , description
            , segmentationByUnit
            , priceByUnitByDim
            , defaultPriceByUnit
            , periodMinimum
            }

instance encodeJsonCharge :: EncodeJson Charge where
  encodeJson = case _ of
    ChargeSingleUnit x -> encodeJson x
    ChargeList x ->
      ("description" :=? x.description)
        ~>? ("charges" := x.charges)
        ~> ("periodMinimum" :=? x.periodMinimum)
        ~>? jsonEmptyObject
    ChargeDimUnitOptSeg x ->
      ("units" := x.units)
        ~> ("currencyByUnit" :=? ifNonEmpty x.currencyByUnit)
        ~>? ("description" :=? x.description)
        ~>? ("segmentationByUnit" :=? ifNonEmpty x.segmentationByUnit)
        ~>? ("priceByUnitByDim" := x.priceByUnitByDim)
        ~> ("defaultPriceByUnit" :=? ifNonEmpty x.defaultPriceByUnit)
        ~>? ("periodMinimum" :=? x.periodMinimum)
        ~>? jsonEmptyObject

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
  | PriceRow
  }

instance decodeJsonPricePerDim :: DecodeJson PricePerDim where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    Price p <- decodeJson json
    pure $ PricePerDim { dim, listPrice: p.listPrice, price: p.price, discount: p.discount }

instance encodeJsonPricePerDim :: EncodeJson PricePerDim where
  encodeJson (PricePerDim x) =
    ("dim" := x.dim)
      ~> ("listPrice" :=? ifNonEq x.price x.listPrice)
      ~>? ("discount" :=? x.discount)
      ~>? ("price" := x.price)
      ~> jsonEmptyObject

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

newtype Segmentation
  = Segmentation
  { unit :: Maybe ChargeUnitId
  , model :: SegmentationModel
  , segments :: Array Segment
  }

derive newtype instance decodeJsonSegmentation :: DecodeJson Segmentation

derive newtype instance encodeJsonSegmentation :: EncodeJson Segmentation

newtype SegmentationDimPerUnit
  = SegmentationDimPerUnit
  { unit :: ChargeUnitId
  , segmentation :: SegmentationDim
  }

derive newtype instance decodeJsonSegmentationDimPerUnit :: DecodeJson SegmentationDimPerUnit

derive newtype instance encodeJsonSegmentationDimPerUnit :: EncodeJson SegmentationDimPerUnit

newtype SegmentationPerUnit
  = SegmentationPerUnit
  { unit :: ChargeUnitId
  , segmentation :: Segmentation
  }

derive newtype instance decodeJsonSegmentationPerUnit :: DecodeJson SegmentationPerUnit

derive newtype instance encodeJsonSegmentationPerUnit :: EncodeJson SegmentationPerUnit

newtype SegmentationDim
  = SegmentationDim
  { unit :: Maybe ChargeUnitId
  , model :: SegmentationModel
  , segmentsByDim :: Array SegmentsPerDim
  }

derive newtype instance decodeJsonSegmentationDim :: DecodeJson SegmentationDim

derive newtype instance encodeJsonSegmentationDim :: EncodeJson SegmentationDim

data SegmentationOptDimPerUnit
  = SegmentationOptUndimPerUnit SegmentationPerUnit
  | SegmentationOptDimPerUnit SegmentationDimPerUnit

instance decodeJsonSegmentationOptDimPerUnit :: DecodeJson SegmentationOptDimPerUnit where
  decodeJson json =
    (SegmentationOptUndimPerUnit <$> decodeJson json)
      <|> (SegmentationOptDimPerUnit <$> decodeJson json)

instance encodeJsonSegmentationOptDimPerUnit :: EncodeJson SegmentationOptDimPerUnit where
  encodeJson = case _ of
    SegmentationOptUndimPerUnit x -> encodeJson x
    SegmentationOptDimPerUnit x -> encodeJson x

data SegmentationOptDim
  = SegmentationOptUndim Segmentation
  | SegmentationOptDim SegmentationDim

instance decodeJsonSegmentationOptDim :: DecodeJson SegmentationOptDim where
  decodeJson json =
    (SegmentationOptUndim <$> decodeJson json)
      <|> (SegmentationOptDim <$> decodeJson json)

instance encodeJsonSegmentationOptDim :: EncodeJson SegmentationOptDim where
  encodeJson = case _ of
    SegmentationOptUndim x -> encodeJson x
    SegmentationOptDim x -> encodeJson x

newtype SegmentsPerDim
  = SegmentsPerDim
  { dim :: DimValue
  , segments :: Array Segment
  }

derive newtype instance decodeJsonSegmentsPerDim :: DecodeJson SegmentsPerDim

derive newtype instance encodeJsonSegmentsPerDim :: EncodeJson SegmentsPerDim

newtype RateCard
  = RateCard
  { sku :: SkuCode
  , title :: Maybe String
  , description :: Maybe String
  , charges :: Array Charge
  }

derive newtype instance decodeJsonRateCard :: DecodeJson RateCard

derive newtype instance encodeJsonRateCard :: EncodeJson RateCard

type PriceRow
  = ( listPrice :: Number
    , price :: Number
    , discount :: Maybe Discount
    )

encodePriceRowJson ::
  forall r a.
  EncodeJson a =>
  { discount :: Maybe Discount
  , listPrice :: Number
  , price :: Number
  | r
  } ->
  a -> Json
encodePriceRowJson p rest =
  ("listPrice" :=? ifNonEq p.price p.listPrice)
    ~>? ("discount" :=? p.discount)
    ~>? ("price" := p.price)
    ~> rest

newtype Price
  = Price { | PriceRow }

derive instance genericPrice :: Generic Price _

derive instance eqPrice :: Eq Price

derive instance newtypePrice :: Newtype Price _

instance showPrice :: Show Price where
  show = genericShow

instance decodeJsonPrice :: DecodeJson Price where
  decodeJson json = do
    o <- decodeJson json
    price <- o .: "price"
    listPrice <- o .:? "listPrice" .!= price
    discount <- o .:? "discount"
    pure $ Price { price, listPrice, discount }

instance encodeJsonPrice :: EncodeJson Price where
  encodeJson (Price x) =
    ("listPrice" :=? ifNonEq x.price x.listPrice)
      ~>? ("discount" :=? x.discount)
      ~>? ("price" := x.price)
      ~> jsonEmptyObject

newtype PricePerSeg
  = PricePerSeg
  { minimum :: Int
  , exclusiveMaximum :: Maybe Int
  | PriceRow
  }

derive instance genericPricePerSeg :: Generic PricePerSeg _

derive instance eqPricePerSeg :: Eq PricePerSeg

instance showPricePerSeg :: Show PricePerSeg where
  show = genericShow

instance decodeJsonPricePerSeg :: DecodeJson PricePerSeg where
  decodeJson json = do
    o <- decodeJson json
    minimum <- o .: "minimum"
    exclusiveMaximum <- o .:? "exclusiveMaximum"
    price <- o .: "price"
    listPrice <- o .:? "listPrice" .!= price
    discount <- o .:? "discount"
    pure $ PricePerSeg { minimum, exclusiveMaximum, price, listPrice, discount }

instance encodeJsonPricePerSeg :: EncodeJson PricePerSeg where
  encodeJson (PricePerSeg p) =
    ("minimum" := p.minimum)
      ~> ("exclusiveMaximum" :=? p.exclusiveMaximum)
      ~>? ("listPrice" :=? ifNonEq p.price p.listPrice)
      ~>? ("discount" :=? p.discount)
      ~>? ("price" := p.price)
      ~> jsonEmptyObject

newtype PricePerDimSeg
  = PricePerDimSeg
  { dim :: DimValue
  , priceBySegment :: Array PricePerSeg
  , periodMinimum :: Maybe Number
  }

instance decodeJsonPricePerDimSeg :: DecodeJson PricePerDimSeg where
  decodeJson json =
    map PricePerDimSeg
      $ { dim: _, priceBySegment: _, periodMinimum: _ }
      <$> decodeJson json
      <*> decodeJson json
      <*> decodeJson json

instance encodeJsonPricePerDimSeg :: EncodeJson PricePerDimSeg where
  encodeJson (PricePerDimSeg x) =
    ("dim" := x.dim)
      ~> ("priceBySegment" := x.priceBySegment)
      ~> ("periodMinimum" :=? x.periodMinimum)
      ~>? jsonEmptyObject

newtype PricePerDimUnit
  = PricePerDimUnit
  { dim :: DimValue
  , priceByUnit :: Array PricePerUnit
  , periodMinimum :: Number
  }

instance decodeJsonPricePerDimUnit :: DecodeJson PricePerDimUnit where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    priceByUnit <- o .: "priceByUnit"
    periodMinimum <- o .:? "periodMinimum" .!= 0.0
    pure $ PricePerDimUnit { dim, priceByUnit, periodMinimum }

instance encodeJsonPricePerDimUnit :: EncodeJson PricePerDimUnit where
  encodeJson (PricePerDimUnit x) =
    ("dim" := x.dim)
      ~> ("priceByUnit" := x.priceByUnit)
      ~> ("periodMinimum" :=? ifNonZero x.periodMinimum)
      ~>? jsonEmptyObject

newtype PricePerDimUnitSeg
  = PricePerDimUnitSeg
  { dim :: DimValue
  , priceBySegmentByUnit :: Array PricePerUnitSeg
  , periodMinimum :: Maybe Number
  }

instance decodeJsonPricePerDimUnitSeg :: DecodeJson PricePerDimUnitSeg where
  decodeJson json =
    map PricePerDimUnitSeg
      $ { dim: _, priceBySegmentByUnit: _, periodMinimum: _ }
      <$> decodeJson json
      <*> decodeJson json
      <*> decodeJson json

instance encodeJsonPricePerDimUnitSeg :: EncodeJson PricePerDimUnitSeg where
  encodeJson (PricePerDimUnitSeg x) =
    ("dim" := x.dim)
      ~> ("priceBySegmentByUnit" := x.priceBySegmentByUnit)
      ~> ("periodMinimum" :=? x.periodMinimum)
      ~>? jsonEmptyObject

data PricePerDimUnitOptSeg
  = PricePerDimUnitOptSeg PricePerDimUnitSeg
  | PricePerDimUnitOptNoSeg PricePerDimUnit

instance decodeJsonPricePerDimUnitOptSeg :: DecodeJson PricePerDimUnitOptSeg where
  decodeJson json =
    (PricePerDimUnitOptSeg <$> decodeJson json)
      <|> (PricePerDimUnitOptNoSeg <$> decodeJson json)

instance encodeJsonPricePerDimUnitOptSeg :: EncodeJson PricePerDimUnitOptSeg where
  encodeJson = case _ of
    PricePerDimUnitOptSeg x -> encodeJson x
    PricePerDimUnitOptNoSeg x -> encodeJson x

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

newtype EstimatedWapPerSegment
  = EstimatedWapPerSegment
  { minimum :: Int
  , exclusiveMaximum :: Maybe Int
  , price :: Number
  }

instance decodeJsonEstimatedWapPerSegment :: DecodeJson EstimatedWapPerSegment where
  decodeJson json = EstimatedWapPerSegment <$> decodeJson json

instance encodeJsonEstimatedWapPerSegment :: EncodeJson EstimatedWapPerSegment where
  encodeJson (EstimatedWapPerSegment x) = encodeJson x

newtype EstimatedWap
  = EstimatedWap (Array EstimatedWapPerSegment)

instance decodeJsonEstimatedWap :: DecodeJson EstimatedWap where
  decodeJson json = primitive <|> object
    where
    mk price = EstimatedWapPerSegment { minimum: 0, exclusiveMaximum: Nothing, price }

    primitive = (EstimatedWap <<< A.singleton <<< mk) <$> decodeJson json

    object = EstimatedWap <$> decodeJson json

instance encodeJsonEstimatedWap :: EncodeJson EstimatedWap where
  encodeJson (EstimatedWap x) = case x of
    [ EstimatedWapPerSegment { minimum: 0, exclusiveMaximum: Nothing, price } ] -> encodeJson price
    _ -> encodeJson x

newtype EstimatedWapPerUnit
  = EstimatedWapPerUnit
  { unit :: ChargeUnitId
  , wap :: EstimatedWap
  }

derive instance newtypeEstimatedWapPerUnit :: Newtype EstimatedWapPerUnit _

derive newtype instance decodeJsonEstimatedWapPerUnit :: DecodeJson EstimatedWapPerUnit

derive newtype instance encodeJsonEstimatedWapPerUnit :: EncodeJson EstimatedWapPerUnit

newtype DefaultPricePerUnit
  = DefaultPricePerUnit
  { unit :: ChargeUnitId
  , description :: Maybe String
  | PriceRow
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
  { unit :: ChargeUnitId
  , description :: Maybe String
  | PriceRow
  }

derive instance newtypePricePerUnit :: Newtype PricePerUnit _

instance decodeJsonPricePerUnit :: DecodeJson PricePerUnit where
  decodeJson json = do
    o <- decodeJson json
    unit <- o .: "unit"
    description <- o .:? "description"
    Price p <- decodeJson json
    pure
      $ PricePerUnit
          { unit
          , description
          , price: p.price
          , listPrice: p.listPrice
          , discount: p.discount
          }

instance encodeJsonPricePerUnit :: EncodeJson PricePerUnit where
  encodeJson (PricePerUnit ppu) =
    ("unit" := ppu.unit)
      ~> ("description" :=? ppu.description)
      ~>? ("price" := ppu.price)
      ~> jsonEmptyObject

newtype PricePerUnitSeg
  = PricePerUnitSeg
  { unit :: ChargeUnitId
  , description :: Maybe String
  , priceBySegment :: Array PricePerSeg
  }

derive instance newtypePricePerUnitSeg :: Newtype PricePerUnitSeg _

derive newtype instance decodeJsonPricePerUnitSeg :: DecodeJson PricePerUnitSeg

instance encodeJsonPricePerUnitSeg :: EncodeJson PricePerUnitSeg where
  encodeJson (PricePerUnitSeg ppu) =
    ("unit" := ppu.unit)
      ~> ((\x -> "description" := x) <$> ppu.description)
      ~>? ("priceBySegment" := ppu.priceBySegment)
      ~> jsonEmptyObject

newtype ChargeUnitId
  = ChargeUnitId String

derive instance genericChargeUnitId :: Generic ChargeUnitId _

derive instance eqChargeUnitId :: Eq ChargeUnitId

derive instance ordChargeUnitId :: Ord ChargeUnitId

derive instance newtypeChargeUnitId :: Newtype ChargeUnitId _

derive newtype instance showChargeUnitId :: Show ChargeUnitId

instance decodeJsonChargeUnitId :: DecodeJson ChargeUnitId where
  decodeJson json = ChargeUnitId <$> decodeJson json

instance encodeJsonChargeUnitId :: EncodeJson ChargeUnitId where
  encodeJson (ChargeUnitId x) = encodeJson x

data ChargeKind
  = CkOnetime
  | CkMonthly
  | CkQuarterly
  | CkUsage
  | CkSegment

derive instance eqChargeKind :: Eq ChargeKind

instance showChargeKind :: Show ChargeKind where
  show = case _ of
    CkOnetime -> "Onetime"
    CkMonthly -> "Monthly"
    CkQuarterly -> "Quarterly"
    CkUsage -> "Usage"
    CkSegment -> "Segment"

instance decodeJsonChargeKind :: DecodeJson ChargeKind where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "Onetime" -> Right CkOnetime
      "Monthly" -> Right CkMonthly
      "Quarterly" -> Right CkQuarterly
      "Usage" -> Right CkUsage
      "Segment" -> Right CkSegment
      _ -> Left (TypeMismatch "ChargeKind")

instance encodeJsonChargeKind :: EncodeJson ChargeKind where
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
    CseObject x ->
      ("type" := "object")
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

newtype ChargeUnit
  = ChargeUnit
  { id :: ChargeUnitId
  , title :: Maybe String
  , description :: Maybe String
  , kind :: ChargeKind
  , priceDimSchema :: Maybe ConfigSchemaEntry
  , reportDimSchemas :: Maybe (Array ConfigSchemaEntry)
  }

derive instance newtypeChargeUnit :: Newtype ChargeUnit _

instance decodeJsonChargeUnit :: DecodeJson ChargeUnit where
  decodeJson json = do
    o <- decodeJson json
    id <- o .: "id"
    title <- o .:? "title"
    description <- o .:? "description"
    kind <- o .:? "kind" .!= CkUsage
    priceDimSchema <- o .:? "priceDimSchema"
    reportDimSchemas <- o .:? "reportDimSchemas"
    pure
      $ ChargeUnit
          { id
          , title
          , description
          , kind
          , priceDimSchema
          , reportDimSchemas
          }

derive newtype instance encodeJsonChargeUnit :: EncodeJson ChargeUnit

newtype PriceDimSchema
  = PriceDimSchema ConfigSchemaEntry

derive newtype instance decodeJsonPriceDimSchema :: DecodeJson PriceDimSchema

derive newtype instance encodeJsonPriceDimSchema :: EncodeJson PriceDimSchema

newtype Product
  = Product
  { sku :: SkuCode
  , title :: Maybe String
  , description :: Maybe String
  , attr :: Maybe (Map String ConfigValue)
  , orderConfigSchema :: Maybe ConfigSchemaEntry
  , assetConfigSchema :: Maybe ConfigSchemaEntry
  , options :: Maybe (Array ProductOption)
  , features :: Maybe (Array ProductFeature)
  , chargeUnits :: Array ChargeUnit
  , rules :: Maybe (Array Rule)
  }

derive instance newtypeProduct :: Newtype Product _

instance decodeJsonProduct :: DecodeJson Product where
  decodeJson json = do
    o <- decodeJson json
    sku <- o .: "sku"
    title <- o .:? "title"
    description <- o .:? "description"
    attrObj :: Maybe (FO.Object ConfigValue) <- o .:? "attr"
    let
      attr = (\obj -> Map.fromFoldable (FO.toUnfoldable obj :: Array _)) <$> attrObj
    orderConfigSchema <- o .:? "orderConfigSchema"
    assetConfigSchema <- o .:? "assetConfigSchema"
    options <- o .:? "options"
    features <- o .:? "features"
    chargeUnits <- o .: "chargeUnits"
    rules <- o .:? "rules"
    pure
      $ Product
          { sku
          , title
          , description
          , attr
          , orderConfigSchema
          , assetConfigSchema
          , options
          , features
          , chargeUnits
          , rules
          }

derive newtype instance encodeJsonProduct :: EncodeJson Product

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
  | PlatformSkycore
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
    PlatformSkycore -> "Skycore"
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
      "Skycore" -> Right PlatformSkycore
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
    , title :: Maybe String
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
      title <- o .:? "title"
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
            , title
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
  { title :: Maybe String
  , description :: Maybe String
  , options :: Maybe (Array Json)
  }

derive newtype instance decodeJsonProductFeature :: DecodeJson ProductFeature

derive newtype instance encodeJsonProductFeature :: EncodeJson ProductFeature

data BillingOption
  = Prepay
  | Postpay

derive instance eqBillingOption :: Eq BillingOption

instance decodeJsonBillingOption :: DecodeJson BillingOption where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "BILLING_OPTION_UNSPECIFIED" -> Right Prepay
      "PREPAY" -> Right Prepay
      "POSTPAY" -> Right Postpay
      _ -> Left (TypeMismatch "BillingOption")

instance encodeJsonBillingOption :: EncodeJson BillingOption where
  encodeJson =
    encodeJson
      <<< case _ of
          Prepay -> "PREPAY"
          Postpay -> "POSTPAY"

data ContractTerm
  = Ongoing
  | Fixed

derive instance eqContractTerm :: Eq ContractTerm

instance decodeJsonContractTerm :: DecodeJson ContractTerm where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "CONTRACT_TERM_UNSPECIFIED" -> Right Ongoing
      "ONGOING" -> Right Ongoing
      "FIXED" -> Right Fixed
      _ -> Left (TypeMismatch "ContractTerm")

instance encodeJsonContractTerm :: EncodeJson ContractTerm where
  encodeJson =
    encodeJson
      <<< case _ of
          Ongoing -> "ONGOING"
          Fixed -> "FIXED"

newtype Commercial
  = Commercial
  { billingOption :: BillingOption
  , contractTerm :: ContractTerm
  , paymentCurrency :: PaymentCurrency
  , billingCurrency :: BillingCurrency
  }

derive newtype instance decodeJsonCommercial :: DecodeJson Commercial

derive newtype instance encodeJsonCommercial :: EncodeJson Commercial

newtype Address
  = Address
  { line1 :: Maybe String
  , line2 :: Maybe String
  , line3 :: Maybe String
  , city :: Maybe String
  , stateOrProvince :: Maybe Subdivision
  , county :: Maybe String
  , country :: Maybe Country
  , postOfficeBox :: Maybe String
  , postalCode :: Maybe String
  }

emptyAddress :: Address
emptyAddress =
  Address
    { line1: Nothing
    , line2: Nothing
    , line3: Nothing
    , city: Nothing
    , stateOrProvince: Nothing
    , county: Nothing
    , country: Nothing
    , postOfficeBox: Nothing
    , postalCode: Nothing
    }

instance decodeJsonAddress :: DecodeJson Address where
  decodeJson json = do
    o <- decodeJson json
    line1 <- getString o "line1" 1 250
    line2 <- getString o "line2" 1 250
    line3 <- getString o "line3" 1 250
    city <- getString o "city" 1 80
    stateOrProvince <- o .:? "stateOrProvince"
    county <- getString o "county" 1 50
    country <- o .:? "country"
    postOfficeBox <- getString o "postOfficeBox" 1 20
    postalCode <- getString o "postalCode" 1 20
    pure
      $ Address
          { line1
          , line2
          , line3
          , city
          , stateOrProvince
          , county
          , country
          , postOfficeBox
          , postalCode
          }
    where
    getString o field minLen maxLen = do
      value <- o .:? field
      case value of
        Nothing -> pure Nothing
        Just v ->
          if S.length v >= minLen && S.length v <= maxLen then
            pure value
          else
            Left (TypeMismatch $ "string of length " <> show minLen <> " through " <> show maxLen)

instance encodeJsonAddress :: EncodeJson Address where
  encodeJson (Address addr) =
    (("line1" := _) <$> addr.line1)
      ~>? (("line2" := _) <$> addr.line2)
      ~>? (("line3" := _) <$> addr.line3)
      ~>? (("city" := _) <$> addr.city)
      ~>? (("stateOrProvince" := _) <$> addr.stateOrProvince)
      ~>? (("county" := _) <$> addr.county)
      ~>? (("country" := _) <$> addr.country)
      ~>? (("postOfficeBox" := _) <$> addr.postOfficeBox)
      ~>? (("postalCode" := _) <$> addr.postalCode)
      ~>? jsonEmptyObject

newtype Contact
  = Contact
  { email :: Maybe String
  , name :: Maybe String
  , phone :: Maybe String
  }

emptyContact :: Contact
emptyContact = Contact { email: Nothing, name: Nothing, phone: Nothing }

derive newtype instance decodeJsonContact :: DecodeJson Contact

instance encodeJsonContact :: EncodeJson Contact where
  encodeJson (Contact x) =
    (("email" := _) <$> x.email)
      ~>? (("name" := _) <$> x.name)
      ~>? (("phone" := _) <$> x.phone)
      ~>? jsonEmptyObject

newtype Buyer
  = Buyer
  { buyerId :: Maybe String
  , address :: Address
  , contacts :: { primary :: Contact, finance :: Contact }
  , corporateName :: String
  , registrationNr :: String
  , taxId :: String
  , website :: Uri
  }

derive newtype instance decodeJsonBuyer :: DecodeJson Buyer

derive newtype instance encodeJsonBuyer :: EncodeJson Buyer

newtype Seller
  = Seller
  { name :: String
  , address :: Address
  , contacts :: { primary :: Contact, finance :: Contact, support :: Contact }
  }

derive newtype instance decodeJsonSeller :: DecodeJson Seller

derive newtype instance encodeJsonSeller :: EncodeJson Seller

newtype BillingAccountRef
  = BillingAccountRef
  { billingAccountId :: String
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

newtype Asset
  = Asset
  { sku :: SkuCode
  , configs :: Array ConfigValue
  , billingAccount :: BillingAccountRef
  , createTime :: DateTime
  , updateTime :: DateTime
  , priceOverrides :: Array PriceOverride
  , priceChangeNotificationInDays :: Int
  }

instance decodeJsonAsset :: DecodeJson Asset where
  decodeJson json = do
    o <- decodeJson json
    sku <- o .: "sku"
    configs <- o .: "configs"
    billingAccount <- o .: "billingAccount"
    createTime <- o .: "createTime"
    updateTime <- o .: "updateTime"
    priceOverrides <- o .: "priceOverrides"
    priceChangeNotificationInDays <- o .:? "priceChangeNotificationInDays" .!= 0
    pure
      $ Asset
          { sku
          , configs
          , billingAccount
          , createTime
          , updateTime
          , priceOverrides
          , priceChangeNotificationInDays
          }

instance encodeJsonAsset :: EncodeJson Asset where
  encodeJson (Asset x) =
    ("sku" := x.sku)
      ~> ("configs" := x.configs)
      ~> ("billingAccount" := x.billingAccount)
      ~> ("createTime" := x.createTime)
      ~> ("updateTime" := x.updateTime)
      ~> ("priceOverrides" := x.priceOverrides)
      ~> ("priceChangeNotificationInDays" :=? ifNonZero x.priceChangeNotificationInDays)
      ~>? jsonEmptyObject

newtype PriceOverride
  = PriceOverride
  { basePriceBook :: PriceBookRef
  , charges :: Array Charge
  , validity :: Validity
  , discountProfileByUnit :: Maybe (Array DiscountProfilePerUnit)
  }

derive newtype instance decodeJsonPriceOverride :: DecodeJson PriceOverride

derive newtype instance encodeJsonPriceOverride :: EncodeJson PriceOverride

newtype DiscountProfilePerUnit
  = DiscountProfilePerUnit
  { unit :: ChargeUnitId
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
  { priceBookId :: String
  , version :: String
  , solutionUri :: Maybe Uri
  }

derive newtype instance decodeJsonPriceBookRef :: DecodeJson PriceBookRef

instance encodeJsonPriceBookRef :: EncodeJson PriceBookRef where
  encodeJson (PriceBookRef x) =
    ("priceBookId" := x.priceBookId)
      ~> ("version" := x.version)
      ~> ((\uri -> "solutionUri" := uri) <$> x.solutionUri)
      ~>? jsonEmptyObject

newtype SalesforceAccountRef
  = SalesforceAccountRef
  { salesforceAccountId :: String
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
    , buyer :: Buyer
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
  | OsInFulfillment
  | OsFulfilled
  | OsCancelled

instance decodeJsonOrderStatus :: DecodeJson OrderStatus where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "IN_DRAFT" -> Right OsInDraft
      "IN_REVIEW" -> Right OsInReview
      "IN_APPROVAL" -> Right OsInApproval
      "IN_SIGNATURE" -> Right OsInSignature
      "IN_CONFIGURATION" -> Right OsInConfiguration
      "IN_FULFILLMENT" -> Right OsInFulfillment
      "FULFILLED" -> Right OsFulfilled
      "CANCELLED" -> Right OsCancelled
      _ -> Left (TypeMismatch "OrderStatus")

instance encodeOrderStatus :: EncodeJson OrderStatus where
  encodeJson =
    encodeJson
      <<< case _ of
          OsInDraft -> "IN_DRAFT"
          OsInReview -> "IN_REVIEW"
          OsInApproval -> "IN_APPROVAL"
          OsInSignature -> "IN_SIGNATURE"
          OsInConfiguration -> "IN_CONFIGURATION"
          OsInFulfillment -> "IN_FULFILLMENT"
          OsFulfilled -> "FULFILLED"
          OsCancelled -> "CANCELLED"

data OrderLineStatus
  = OlsNew
  | OlsAccepted
  | OlsSucceeded
  | OlsFailed
  | OlsCancelled

instance decodeJsonOrderLineStatus :: DecodeJson OrderLineStatus where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "NEW" -> Right OlsNew
      "ACCEPTED" -> Right OlsAccepted
      "SUCCEEDED" -> Right OlsSucceeded
      "FAILED" -> Right OlsFailed
      "CANCELLED" -> Right OlsCancelled
      _ -> Left (TypeMismatch "OrderLineStatus")

instance encodeOrderLineStatus :: EncodeJson OrderLineStatus where
  encodeJson =
    encodeJson
      <<< case _ of
          OlsNew -> "NEW"
          OlsAccepted -> "ACCEPTED"
          OlsSucceeded -> "SUCCEEDED"
          OlsFailed -> "FAILED"
          OlsCancelled -> "CANCELLED"

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

newtype EstimatedUsagePerDim
  = EstimatedUsagePerDim
  { dim :: DimValue
  , usage :: Int
  }

derive newtype instance decodeJsonEstimatedUsagePerDim :: DecodeJson EstimatedUsagePerDim

derive newtype instance encodeJsonEstimatedUsagePerDim :: EncodeJson EstimatedUsagePerDim

data EstimatedUsage
  = EstimatedUsagePerUnit
    { unit :: ChargeUnitId
    , usage :: Int
    }
  | EstimatedUsageByDimPerUnit
    { unit :: ChargeUnitId
    , usageByDim :: Array EstimatedUsagePerDim
    }

instance decodeJsonEstimatedUsage :: DecodeJson EstimatedUsage where
  decodeJson json = perUnit <|> perUnitByDim
    where
    perUnit = EstimatedUsagePerUnit <$> decodeJson json

    perUnitByDim = EstimatedUsageByDimPerUnit <$> decodeJson json

instance encodeJsonEstimatedUsage :: EncodeJson EstimatedUsage where
  encodeJson = case _ of
    EstimatedUsagePerUnit x -> encodeJson x
    EstimatedUsageByDimPerUnit x -> encodeJson x

newtype OrderLineConfig
  = OrderLineConfig
  { quantity :: Int
  , config :: Maybe ConfigValue
  }

derive newtype instance decodeJsonOrderLineConfig :: DecodeJson OrderLineConfig

instance encodeJsonOrderLineConfig :: EncodeJson OrderLineConfig where
  encodeJson (OrderLineConfig x) =
    ("quantity" := x.quantity)
      ~> ("config" :=? x.config)
      ~>? jsonEmptyObject

newtype OrderLine
  = OrderLine
  { orderLineId :: Maybe String
  , status :: OrderLineStatus
  , sku :: SkuCode
  , charges :: Array Charge
  , configs :: Array OrderLineConfig
  , estimatedUsage :: Array EstimatedUsage
  }

derive instance newtypeOrderLine :: Newtype OrderLine _

instance decodeJsonOrderLine :: DecodeJson OrderLine where
  decodeJson json = do
    o <- decodeJson json
    orderLineId <- o .:? "orderLineId"
    status <- o .:? "status" .!= OlsNew
    sku <- o .: "sku"
    charges <- o .: "charges"
    configs <- o .:? "configs" .!= []
    estimatedUsage <- o .:? "estimatedUsage" .!= []
    pure $ OrderLine { orderLineId, status, sku, charges, configs, estimatedUsage }

instance encodeJsonOrderLine :: EncodeJson OrderLine where
  encodeJson (OrderLine x) =
    ("orderLineId" :=? x.orderLineId)
      ~>? ("status" := x.status)
      ~> ("sku" := x.sku)
      ~> ("charges" := x.charges)
      ~> ("configs" :=? ifNonEmpty x.configs)
      ~>? ("estimatedUsage" :=? ifNonEmpty x.estimatedUsage)
      ~>? jsonEmptyObject

newtype OrderSection
  = OrderSection
  { basePriceBook :: PriceBookRef
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
  { orders :: Array OrderForm
  }

derive newtype instance decodeJsonOrders :: DecodeJson Orders

derive newtype instance encodeJsonOrders :: EncodeJson Orders

newtype LegalEntityTraffic
  = LegalEntityTraffic
  { originating :: String
  , trafficPlatform :: String
  , automatedBillingSupport :: String
  , trafficIntegration :: String
  }

derive newtype instance decodeJsonLegalEntityTraffic :: DecodeJson LegalEntityTraffic

derive newtype instance encodeJsonLegalEntityTraffic :: EncodeJson LegalEntityTraffic

newtype LegalEntity
  = LegalEntity
  { registeredName :: String
  , novaShortName :: String
  , status :: String
  , allowNewCustomers :: String
  , defaultBankCurrency :: Currency
  , availableCurrencies :: Set Currency
  , traffics :: Array LegalEntityTraffic
  , address :: Address
  , phone :: Maybe String
  , region :: String
  , regionalVpInDpa :: String
  , contacts :: { primary :: Contact, finance :: Contact, support :: Contact }
  }

derive instance newtypeLegalEntity :: Newtype LegalEntity _

derive newtype instance decodeJsonLegalEntity :: DecodeJson LegalEntity

derive newtype instance encodeJsonLegalEntity :: EncodeJson LegalEntity

newtype LegalEntities
  = LegalEntities { legalEntities :: Array LegalEntity }

derive newtype instance decodeJsonLegalEntities :: DecodeJson LegalEntities

derive newtype instance encodeJsonLegalEntities :: EncodeJson LegalEntities

-- | Given a value, returns nothing if equal a first value, otherwise just the value.
ifNonEq :: forall a. Eq a => a -> a -> Maybe a
ifNonEq x y
  | x /= y = Just y
  | otherwise = Nothing

-- | Given a value, returns nothing if 0, otherwise just the value.
ifNonZero :: forall a. Semiring a => Eq a => a -> Maybe a
ifNonZero x
  | x == zero = Nothing
  | otherwise = Just x

-- | Given an array, returns nothing if empty, otherwise just the array.
ifNonEmpty :: forall a. Array a -> Maybe (Array a)
ifNonEmpty = case _ of
  [] -> Nothing
  xs -> Just xs
