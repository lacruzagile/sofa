module Data.SmartSpec
  ( Address(..)
  , Asset(..)
  , BillingAccountRef(..)
  , BillingOption(..)
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
  , LegalEntity(..)
  , OrderForm(..)
  , OrderLine(..)
  , OrderSection(..)
  , OrderSectionSummary(..)
  , OrderStatus(..)
  , OrderSummary(..)
  , Platform(..)
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
  , RateCardCharge(..)
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
  , SegmentedPrice(..)
  , Seller(..)
  , Severity(..)
  , SimplePrice(..)
  , Sku(..)
  , Solution(..)
  , SpecUnit(..)
  , UnitRef(..)
  , Uri(..)
  , Validity(..)
  ) where

import Prelude
import Data.Newtype (class Newtype)
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object as FO

type Uri
  = String

newtype Solution
  = Solution
  { id :: String
  , name :: Maybe String
  , description :: Maybe String
  , products :: Array Product
  , rules :: Array Rule
  , priceBooks :: Array PriceBook
  }

instance decodeJsonSolution :: DecodeJson Solution where
  decodeJson json = do
    o <- decodeJson json
    id <- o .: "id"
    name <- o .:? "name"
    description <- o .:? "description"
    products <- o .: "products"
    rules <- o .:? "rules" .!= []
    priceBooks <- o .: "priceBooks"
    pure $ Solution { id, name, description, rules, products, priceBooks }

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

newtype RuleConditionExpr
  = RuleConditionExpr { type_ :: String, expr :: String }

instance decodeJsonRuleConditionExpr :: DecodeJson RuleConditionExpr where
  decodeJson json = do
    o <- decodeJson json
    type_ <- o .: "type"
    expr <- o .: "expr"
    pure $ RuleConditionExpr { type_, expr }

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

newtype Currency
  = Currency { code :: String, country :: Maybe String }

instance decodeJsonCurrency :: DecodeJson Currency where
  decodeJson json = plainCode <|> currency
    where
    plainCode = (\code -> Currency { code, country: Nothing }) <$> decodeJson json

    currency = do
      o <- decodeJson json
      code <- o .: "code"
      country <- o .:? "country" .!= Nothing
      pure $ Currency { code, country }

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

-- TODO: Assert non-negative.
data RateCardCharge
  = RccSimple
    { unit :: UnitRef
    , price :: SimplePrice
    , segmentation :: Maybe PriceSegmentation
    , termOfPriceChangeInDays :: Maybe Int
    , monthlyMinimum :: Maybe Int
    }
  | RccMixed
    { units :: Array UnitRef
    , priceSegmentations :: Array PriceSegmentationByUnit
    , defaultPrices :: Array DefaultPriceByUnit
    , pricesPerDim :: Array PricesPerDimByUnit
    , monthlyMinimum :: Number
    , termOfPriceChangeInDays :: Int
    }
  | RccArray (Array RateCardCharge)

instance decodeJsonRateCardCharge :: DecodeJson RateCardCharge where
  decodeJson json = rccElement json <|> rccArray json
    where
    rccElement j = rccSimple j <|> rccMixed j

    rccArray j = RccArray <$> decodeArray rccElement j

    rccSimple j = RccSimple <$> decodeJson j

    rccMixed j = do
      o <- decodeJson j
      units <- o .: "units"
      priceSegmentations <- o .:? "priceSegmentations" .!= []
      defaultPrices <- o .:? "defaultPrices" .!= []
      pricesPerDim <- o .: "pricesPerDim"
      monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
      termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
      pure
        $ RccMixed
            { units
            , priceSegmentations
            , defaultPrices
            , pricesPerDim
            , monthlyMinimum
            , termOfPriceChangeInDays
            }

data SimplePrice
  = SimplePriceSegmented SegmentedPrice
  | SimplePriceByDim (Array PriceByDim)

instance decodeJsonSimplePrice :: DecodeJson SimplePrice where
  decodeJson json = segmented <|> byDim
    where
    segmented = SimplePriceSegmented <$> decodeJson json

    byDim = SimplePriceByDim <$> decodeJson json

data DimValue
  = DimFlat ConfigValue
  | DimMap (Map String ConfigValue)

instance decodeJsonDimValue :: DecodeJson DimValue where
  decodeJson json = dimFlat <|> dimMap
    where
    dimFlat = DimFlat <$> decodeJson json

    dimMap = do
      dimObj :: FO.Object ConfigValue <- decodeJson json
      let
        dim = Map.fromFoldable (FO.toUnfoldable dimObj :: Array _)
      pure $ DimMap dim

newtype PriceByDim
  = PriceByDim
  { dim :: DimValue
  , price :: SegmentedPrice
  , monthlyMinimum :: Number
  }

instance decodeJsonPriceByDim :: DecodeJson PriceByDim where
  decodeJson json = do
    o <- decodeJson json
    dim <- o .: "dim"
    price <- o .: "prices"
    monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
    pure $ PriceByDim { dim, price, monthlyMinimum }

newtype SegmentPrice
  = SegmentPrice
  { minimum :: Int, exclusiveMaximum :: Maybe Int, price :: Number
  }

instance decodeJsonSegmentPrice :: DecodeJson SegmentPrice where
  decodeJson json = SegmentPrice <$> decodeJson json

newtype PriceSegmentation
  = PriceSegmentation
  { segmentUnit :: UnitRef
  , period :: SegmentationPeriod
  , model :: SegmentationModel
  , segments :: Array Segment
  }

instance decodeJsonPriceSegmentation :: DecodeJson PriceSegmentation where
  decodeJson json = PriceSegmentation <$> decodeJson json

newtype PriceSegmentationByUnit
  = PriceSegmentationByUnit
  { unit :: UnitRef
  , segmentation :: PriceSegmentation
  }

instance decodeJsonPriceSegmentationByUnit :: DecodeJson PriceSegmentationByUnit where
  decodeJson json = PriceSegmentationByUnit <$> decodeJson json

newtype RateCard
  = RateCard
  { sku :: Sku
  , name :: Maybe String
  , description :: Maybe String
  , charge :: RateCardCharge
  }

instance decodeJsonRateCard :: DecodeJson RateCard where
  decodeJson json = RateCard <$> decodeJson json

newtype SegmentedPrice
  = SegmentedPrice (Array SegmentPrice)

instance decodeJsonSegmentedPrice :: DecodeJson SegmentedPrice where
  decodeJson json = fixed <|> segmented
    where
    mkSingleton p = SegmentedPrice [ SegmentPrice { minimum: 0, exclusiveMaximum: Nothing, price: p } ]

    fixed = mkSingleton <$> decodeJson json

    segmented = SegmentedPrice <$> decodeJson json

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

newtype DefaultPriceByUnit
  = DefaultPriceByUnit
  { unit :: UnitRef
  , price :: Number
  }

instance decodeJsonDefaultPriceByUnit :: DecodeJson DefaultPriceByUnit where
  decodeJson json = DefaultPriceByUnit <$> decodeJson json

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

newtype PriceByUnit
  = PriceByUnit
  { unit :: UnitRef
  , price :: SegmentedPrice
  }

instance decodeJsonPriceByUnit :: DecodeJson PriceByUnit where
  decodeJson = map PriceByUnit <<< decodeJson

newtype UnitRef
  = UnitRef { unitID :: String, product :: Maybe ProductRef }

instance decodeJsonUnitRef :: DecodeJson UnitRef where
  decodeJson json = UnitRef <$> plainID <|> full
    where
    plainID = (\id -> { unitID: id, product: Nothing }) <$> decodeJson json

    full = do
      o <- decodeJson json
      unitID <- o .: "unitID"
      product <- o .: "product"
      pure $ UnitRef { unitID, product }

newtype ProductRef
  = ProductRef { sku :: Sku, solutionURI :: Maybe Uri }

instance decodeJsonProductRef :: DecodeJson ProductRef where
  decodeJson = map ProductRef <<< decodeJson

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

data ConfigSchemaEntry
  = CseInteger
    { minimum :: Maybe Number
    , maximum :: Maybe Number
    , default :: Maybe Number
    }
  | CseString
    { minLength :: Maybe Int
    , maxLength :: Maybe Int
    , default :: Maybe String
    }
  | CseRegex
    { pattern :: String
    , default :: Maybe String
    }
  | CseArray
    { items :: ConfigSchemaEntry }
  | CseObject
    { properties :: Map String ConfigSchemaEntry }

instance decodeJsonConfigSchemaEntry :: DecodeJson ConfigSchemaEntry where
  decodeJson json = do
    o <- decodeJson json
    type_ <- o .: "type"
    case type_ of
      "integer" -> do
        minimum <- o .:? "minimum"
        maximum <- o .:? "maximum"
        default <- o .:? "default"
        pure $ CseInteger { minimum, maximum, default }
      "string" -> do
        minLength <- o .:? "minLength"
        maxLength <- o .:? "maxLength"
        default <- o .:? "default"
        pure $ CseString { minLength, maxLength, default }
      "regex" -> do
        pattern <- o .: "pattern"
        default <- o .:? "default"
        pure $ CseRegex { pattern, default }
      "array" -> do
        items <- o .: "items"
        pure $ CseArray { items }
      "object" -> do
        propertiesObj :: FO.Object ConfigSchemaEntry <- o .: "properties"
        let
          properties = Map.fromFoldable (FO.toUnfoldable propertiesObj :: Array _)
        pure $ CseObject { properties }
      _ -> Left (TypeMismatch "ConfigSchemaEntry")

data ConfigValue
  = CvInteger Int
  | CvString String

instance showConfigValue :: Show ConfigValue where
  show = case _ of
    CvInteger v -> show v
    CvString v -> v

instance decodeJsonConfigValue :: DecodeJson ConfigValue where
  decodeJson json =
    (CvInteger <$> decodeJson json)
      <|> (CvString <$> decodeJson json)

-- TODO: Add `schema` and `variable`.
newtype ProductVariable
  = ProductVariable
  { name :: String, path :: String
  }

instance decodeJsonProductVariable :: DecodeJson ProductVariable where
  decodeJson = map ProductVariable <<< decodeJson

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

newtype ProductFeature
  = ProductFeature
  { name :: Maybe String
  , description :: Maybe String
  , options :: Maybe (Array Json)
  }

instance decodeJsonProductFeature :: DecodeJson ProductFeature where
  decodeJson json = ProductFeature <$> decodeJson json

data BillingOption
  = Prepay
  | PostPay

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

data ContractTerm
  = Ongoing
  | Fixed

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

newtype Commercial
  = Commercial
  { billingOption :: BillingOption
  , contractTerm :: ContractTerm
  , paymentCurrency :: Currency
  , priceCurrency :: Currency
  }

instance decodeJsonCommercial :: DecodeJson Commercial where
  decodeJson json = Commercial <$> decodeJson json

newtype Address
  = Address {}

instance decodeJsonAddress :: DecodeJson Address where
  decodeJson json = Address <$> decodeJson json

newtype Contact
  = Contact
  { email :: String
  , name :: String
  , phone :: String
  }

instance decodeJsonContact :: DecodeJson Contact where
  decodeJson json = Contact <$> decodeJson json

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

newtype LegalEntity
  = LegalEntity
  { name :: String
  , address :: Address
  , country :: String
  }

instance decodeJsonLegalEntity :: DecodeJson LegalEntity where
  decodeJson json = LegalEntity <$> decodeJson json

newtype Seller
  = Seller
  { contacts :: { primary :: Contact, finance :: Contact, support :: Contact }
  , legalEntity :: LegalEntity
  }

instance decodeJsonSeller :: DecodeJson Seller where
  decodeJson json = Seller <$> decodeJson json

newtype BillingAccountRef
  = BillingAccountRef
  { billingAccountID :: String
  }

instance decodeJsonBillingAccountRef :: DecodeJson BillingAccountRef where
  decodeJson json = BillingAccountRef <$> decodeJson json

data ReturnCustomerCommercial
  = RccCommercial Commercial
  | RccBillingAccountRef BillingAccountRef

instance decodeJsonReturnCustomerCommercial :: DecodeJson ReturnCustomerCommercial where
  decodeJson json =
    (RccCommercial <$> decodeJson json)
      <|> (RccBillingAccountRef <$> decodeJson json)

type Date
  = String

newtype Validity
  = Validity
  { startDate :: Date
  , endDateExclusive :: Maybe Date
  }

instance decodeJsonValidity :: DecodeJson Validity where
  decodeJson json = Validity <$> decodeJson json

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

newtype PriceOverride
  = PriceOverride
  { basePriceBookRef :: PriceBookRef
  , charge :: RateCardCharge
  , validity :: Validity
  }

instance decodeJsonPriceOverride :: DecodeJson PriceOverride where
  decodeJson json = PriceOverride <$> decodeJson json

newtype PriceBookRef
  = PriceBookRef
  { priceBookID :: String, solutionURI :: Maybe Uri
  }

instance decodeJsonPriceBookRef :: DecodeJson PriceBookRef where
  decodeJson json = PriceBookRef <$> decodeJson json

newtype SalesforceAccountRef
  = SalesforceAccountRef
  { salesforceAccountID :: String
  }

instance decodeJsonSalesforceAccountRef :: DecodeJson SalesforceAccountRef where
  decodeJson json = SalesforceAccountRef <$> decodeJson json

newtype ReturnCustomerData
  = ReturnCustomerData
  { assets :: Array Asset
  , salesforceAccountRef :: SalesforceAccountRef
  }

instance decodeJsonReturnCustomerData :: DecodeJson ReturnCustomerData where
  decodeJson json = ReturnCustomerData <$> decodeJson json

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

data OrderStatus
  = OsQuoteNew
  | OsQuoteApprovalPending
  | OsQuoteApproved
  | OsQuoteAborted
  | OsQuoteSignPending
  | OsQuoteSigned
  | OsOrderPending
  | OsOrderOngoing
  | OsOrderAborted
  | OsOrderCompleted
  | OsProvisionPending
  | OsProvisionOngoging
  | OsProvisionCompleted
  | OsProvisionAborted

instance showOrderStatus :: Show OrderStatus where
  show = case _ of
    OsQuoteNew -> "ProvisionAborted"
    OsQuoteApprovalPending -> "ProvisionAborted"
    OsQuoteApproved -> "ProvisionAborted"
    OsQuoteAborted -> "ProvisionAborted"
    OsQuoteSignPending -> "ProvisionAborted"
    OsQuoteSigned -> "ProvisionAborted"
    OsOrderPending -> "ProvisionAborted"
    OsOrderOngoing -> "ProvisionAborted"
    OsOrderAborted -> "ProvisionAborted"
    OsOrderCompleted -> "ProvisionAborted"
    OsProvisionPending -> "ProvisionAborted"
    OsProvisionOngoging -> "ProvisionAborted"
    OsProvisionCompleted -> "ProvisionAborted"
    OsProvisionAborted -> "ProvisionAborted"

instance decodeJsonOrderStatus :: DecodeJson OrderStatus where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "QuoteNew" -> Right OsProvisionAborted
      "QuoteApprovalPending" -> Right OsProvisionAborted
      "QuoteApproved" -> Right OsProvisionAborted
      "QuoteAborted" -> Right OsProvisionAborted
      "QuoteSignPending" -> Right OsProvisionAborted
      "QuoteSigned" -> Right OsProvisionAborted
      "OrderPending" -> Right OsProvisionAborted
      "OrderOngoing" -> Right OsProvisionAborted
      "OrderAborted" -> Right OsProvisionAborted
      "OrderCompleted" -> Right OsProvisionAborted
      "ProvisionPending" -> Right OsProvisionAborted
      "ProvisionOngoging" -> Right OsProvisionAborted
      "ProvisionCompleted" -> Right OsProvisionAborted
      "ProvisionAborted" -> Right OsProvisionAborted
      _ -> Left (TypeMismatch "OrderStatus")

newtype OrderSummary
  = OrderSummary
  { estimatedUsageTotal :: Number
  , monthlyTotal :: Number
  , onetimeTotal :: Number
  }

instance decodeJsonOrderSummary :: DecodeJson OrderSummary where
  decodeJson json = OrderSummary <$> decodeJson json

newtype OrderSectionSummary
  = OrderSectionSummary
  { estimatedUsageSubTotal :: Number
  , monthlySubTotal :: Number
  , onetimeSubTotal :: Number
  }

instance decodeJsonOrderSectionSummary :: DecodeJson OrderSectionSummary where
  decodeJson json = OrderSectionSummary <$> decodeJson json

newtype Segment
  = Segment
  { minimum :: Int
  , exclusiveMaximum :: Maybe Int
  }

instance decodeJsonSegment :: DecodeJson Segment where
  decodeJson json = Segment <$> decodeJson json

newtype OrderLine
  = OrderLine
  { basePriceBookRef :: PriceBookRef
  , sku :: Sku
  , charge :: RateCardCharge
  , quantity :: Int
  }

derive instance newtypeOrderLine :: Newtype OrderLine _

instance decodeJsonOrderLine :: DecodeJson OrderLine where
  decodeJson json = OrderLine <$> decodeJson json

newtype OrderSection
  = OrderSection
  { solutionURI :: Uri
  , orderLines :: Array OrderLine
  , summary :: OrderSectionSummary
  }

instance decodeJsonOrderSection :: DecodeJson OrderSection where
  decodeJson json = OrderSection <$> decodeJson json

newtype OrderForm
  = OrderForm
  { id :: String
  , customer :: Customer
  , status :: OrderStatus
  , summary :: OrderSummary
  , sections :: Array OrderSection
  }

instance decodeJsonOrderForm :: DecodeJson OrderForm where
  decodeJson json = OrderForm <$> decodeJson json
