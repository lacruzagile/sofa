module Data.SmartSpec
  ( Address(..)
  , Asset(..)
  , BillingAccountRef(..)
  , BillingOption(..)
  , BillingUnit(..)
  , BillingUnitRef(..)
  , ChargeType(..)
  , Commercial(..)
  , ConfigSchemaEntry(..)
  , ConfigValue(..)
  , Contact(..)
  , ContractTerm(..)
  , Currency(..)
  , Customer(..)
  , Date(..)
  , DefaultUnitPriceByBillingUnit(..)
  , DimType(..)
  , DimTypeRef(..)
  , EstPriceSegment(..)
  , EstWeightByDim(..)
  , LegalEntity(..)
  , Meta(..)
  , MonthlyCharge(..)
  , MonthlyChargeOrderline(..)
  , MonthlyPriceOverride(..)
  , MonthlyPriceOverrideElem(..)
  , OnetimeCharge(..)
  , OnetimeChargeOrderline(..)
  , OnetimePriceOverride(..)
  , OnetimePriceOverrideElem(..)
  , OrderForm(..)
  , OrderLine(..)
  , OrderSection(..)
  , OrderSectionSummary(..)
  , OrderStatus(..)
  , OrderSummary(..)
  , Platform(..)
  , PriceBook(..)
  , PriceOverrides(..)
  , Product(..)
  , ProductCategory(..)
  , ProductFeature(..)
  , ProductInstance(..)
  , ProductOption(..)
  , ProductOptionType(..)
  , Purchaser(..)
  , RateCard(..)
  , RateCardCharge(..)
  , ReturnCustomerCommercial(..)
  , ReturnCustomerData(..)
  , SalesforceAccountRef(..)
  , Segmentation(..)
  , SegmentationByBillingUnit(..)
  , SegmentationModel(..)
  , SegmentationPeriod(..)
  , SegmentedPrice(..)
  , Seller(..)
  , SimpleCharge(..)
  , SimpleChargeRow(..)
  , Sku(..)
  , Solution(..)
  , UnitPriceByBillingUnit(..)
  , UnitPricePerDimByBillingUnit(..)
  , Uri(..)
  , UsageCharge(..)
  , UsageChargeOrderline(..)
  , UsageChargeRow(..)
  , UsagePriceOverride(..)
  , UsagePriceOverrideElem(..)
  , UsageSchemaRef(..)
  , UsageSchemaRefByBillingUnit(..)
  , Validity(..)
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson, (.:?), (.:), (.!=))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object as FO

type Uri
  = String

type Meta
  = { solutions :: Array Uri
    }

newtype Solution
  = Solution
  { description :: String
  , products :: Array Product
  , priceBooks :: Array PriceBook
  }

instance decodeJsonSolution :: DecodeJson Solution where
  decodeJson json = Solution <$> decodeJson json

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
  , name :: String
  , currency :: Currency
  , rateCardPathPrefix :: Maybe String
  , rateCards :: Maybe (Array RateCard)
  }

instance decodeJsonPriceBook :: DecodeJson PriceBook where
  decodeJson json = PriceBook <$> decodeJson json

data RateCardCharge
  = RateCardCharge
    { onetimeCharges :: Array OnetimeCharge
    , monthlyCharges :: Array MonthlyCharge
    , usageCharges :: Array UsageCharge
    }

instance decodeJsonRateCardCharge :: DecodeJson RateCardCharge where
  decodeJson json = do
    o <- decodeJson json
    onetimeCharges <- o .:? "onetimeCharges" .!= []
    monthlyCharges <- o .:? "monthlyCharges" .!= []
    usageCharges <- o .:? "usageCharges" .!= []
    pure $ RateCardCharge { onetimeCharges, monthlyCharges, usageCharges }

data RateCard
  = RateCardPath String
  | RateCard
    { sku :: Sku
    , name :: Maybe String
    , description :: Maybe String
    , charge :: RateCardCharge
    }

instance decodeJsonRateCard :: DecodeJson RateCard where
  decodeJson json = rateCardPath <|> rateCard
    where
    rateCardPath = RateCardPath <$> decodeJson json

    rateCard = do
      o <- decodeJson json
      sku <- o .: "sku"
      name <- o .:? "name"
      description <- o .:? "description"
      charge <- decodeJson json
      pure $ RateCard { sku, name, description, charge }

type SimpleChargeRow
  = ( billingUnitRef :: BillingUnitRef
    , price :: Number
    )

type SimpleCharge
  = Record SimpleChargeRow

newtype OnetimeCharge
  = OnetimeCharge SimpleCharge

instance decodeJsonOnetimeCharge :: DecodeJson OnetimeCharge where
  decodeJson json = OnetimeCharge <$> decodeJson json

newtype MonthlyCharge
  = MonthlyCharge SimpleCharge

instance decodeJsonMonthlyCharge :: DecodeJson MonthlyCharge where
  decodeJson json = MonthlyCharge <$> decodeJson json

type UsageChargeRow
  = ( termOfPriceChangeInDays :: Int
    , billingUnitRefs :: Maybe (Array BillingUnitRef)
    , usageSchemaRefByBillingUnit :: Maybe UsageSchemaRefByBillingUnit
    , segmentationByBillingUnit :: Maybe SegmentationByBillingUnit
    , defaultUnitPriceByBillingUnit :: Maybe DefaultUnitPriceByBillingUnit
    , unitPricePerDimByBillingUnit :: Array (UnitPricePerDimByBillingUnit)
    , monthlyMinimum :: Number
    )

newtype UsageCharge
  = UsageCharge (Record UsageChargeRow)

instance decodeJsonUsageCharge :: DecodeJson UsageCharge where
  decodeJson json = do
    o <- decodeJson json
    termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
    billingUnitRefs <- o .:? "billingUnitRefs"
    usageSchemaRefByBillingUnit <- o .:? "usageSchemaRefByBillingUnit"
    segmentationByBillingUnit <- o .:? "segmentationByBillingUnit"
    defaultUnitPriceByBillingUnit <- o .:? "defaultUnitPriceByBillingUnit"
    unitPricePerDimByBillingUnit <- o .:? "unitPricePerDimByBillingUnit" .!= []
    monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
    pure
      $ UsageCharge
          { termOfPriceChangeInDays
          , billingUnitRefs
          , usageSchemaRefByBillingUnit
          , segmentationByBillingUnit
          , defaultUnitPriceByBillingUnit
          , unitPricePerDimByBillingUnit
          , monthlyMinimum
          }

newtype DefaultUnitPriceByBillingUnit
  = DefaultUnitPriceByBillingUnit
  { billingUnitRef :: BillingUnitRef
  , defaultPrice :: Number
  }

instance decodeJsonDefaultUnitPriceByBillingUnit :: DecodeJson DefaultUnitPriceByBillingUnit where
  decodeJson json = DefaultUnitPriceByBillingUnit <$> decodeJson json

data SegmentedPrice
  = FixedPrice Number
  | SegmentedPrice
    { minimum :: Int
    , exclusiveMaximum :: Int
    , price :: Number
    }

instance decodeJsonSegmentedPrice :: DecodeJson SegmentedPrice where
  decodeJson json = fixedPrice <|> segmented
    where
    fixedPrice = FixedPrice <$> decodeJson json

    -- TODO: The fields are optional in the schema??
    segmented = do
      o <- decodeJson json
      minimum <- o .: "minimum"
      exclusiveMaximum <- o .: "exclusiveMaximum"
      price <- o .: "price"
      pure $ SegmentedPrice { minimum, exclusiveMaximum, price }

newtype UnitPriceByBillingUnit
  = UnitPriceByBillingUnit
  { billingUnitRef :: BillingUnitRef
  , price :: SegmentedPrice
  }

instance decodeJsonUnitPriceByBillingUnit :: DecodeJson UnitPriceByBillingUnit where
  decodeJson json = UnitPriceByBillingUnit <$> decodeJson json

newtype UnitPricePerDimByBillingUnit
  = UnitPricePerDimByBillingUnit
  { dim :: Map String ConfigValue
  , monthlyMinimum :: Number
  , unitPricesByBillingUnit :: Array UnitPriceByBillingUnit
  }

instance decodeJsonUnitPricePerDimByBillingUnit :: DecodeJson UnitPricePerDimByBillingUnit where
  decodeJson json = do
    o <- decodeJson json
    dimObj :: FO.Object ConfigValue <- o .: "dim"
    let
      dim = Map.fromFoldable (FO.toUnfoldable dimObj :: Array _)
    monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
    unitPricesByBillingUnit <- o .: "unitPricesByBillingUnit"
    pure $ UnitPricePerDimByBillingUnit { dim, monthlyMinimum, unitPricesByBillingUnit }

newtype SegmentationByBillingUnit
  = SegmentationByBillingUnit
  { billingUnitRef :: BillingUnitRef
  , segmentation :: Segmentation
  }

instance decodeJsonSegmentationByBillingUnit :: DecodeJson SegmentationByBillingUnit where
  decodeJson json = SegmentationByBillingUnit <$> decodeJson json

newtype UsageSchemaRefByBillingUnit
  = UsageSchemaRefByBillingUnit
  { billingUnitRef :: BillingUnitRef
  , usageSchemaRef :: UsageSchemaRef
  }

instance decodeJsonUsageSchemaRefByBillingUnit :: DecodeJson UsageSchemaRefByBillingUnit where
  decodeJson json = UsageSchemaRefByBillingUnit <$> decodeJson json

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

newtype Segmentation
  = Segmentation
  { usageSchemaRef :: UsageSchemaRef
  -- , specifiedDims :: Array …
  , unspecifiedDims :: Array DimTypeRef
  , period :: SegmentationPeriod
  , model :: SegmentationModel
  , segments :: Array SegmentedPrice
  }

instance decodeJsonSegmentation :: DecodeJson Segmentation where
  decodeJson json = Segmentation <$> decodeJson json

newtype BillingUnitRef
  = BillingUnitRef { billingUnitId :: String, solutionUri :: Maybe Uri }

instance decodeJsonBillingUnitRef :: DecodeJson BillingUnitRef where
  decodeJson json = BillingUnitRef <$> plainId <|> full
    where
    plainId = (\id -> { billingUnitId: id, solutionUri: Nothing }) <$> decodeJson json

    full = do
      o <- decodeJson json
      billingUnitId <- o .: "billingUnitID"
      solutionUri <- o .:? "solutionURI"
      pure $ BillingUnitRef { billingUnitId, solutionUri }

newtype BillingUnit
  = BillingUnit
  { id :: Maybe String
  , name :: Maybe String
  , chargeType :: ChargeType
  , description :: Maybe String
  }

instance decodeJsonBillingUnit :: DecodeJson BillingUnit where
  decodeJson json = BillingUnit <$> decodeJson json

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

newtype Product
  = Product
  { sku :: String
  , description :: String
  , configSchema :: Maybe (Map String ConfigSchemaEntry)
  , options :: Maybe (Array ProductOption)
  , features :: Maybe (Array ProductFeature)
  }

instance decodeJsonProduct :: DecodeJson Product where
  decodeJson json = do
    o <- decodeJson json
    sku <- o .: "sku"
    description <- o .: "description"
    configSchemaObj :: Maybe (FO.Object ConfigSchemaEntry) <- o .:? "configSchema"
    let
      configSchema = (\obj -> Map.fromFoldable (FO.toUnfoldable obj :: Array _)) <$> configSchemaObj
    options <- o .:? "options"
    features <- o .:? "features"
    pure
      $ Product
          { sku
          , description
          , configSchema
          , options
          , features
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
    , description :: Maybe String
    , name :: Maybe String
    , platform :: Maybe Platform
    , productCategory :: Maybe ProductCategory
    , accountProduct :: Maybe Boolean
    , billableProduct :: Maybe Boolean
    , commercialProduct :: Maybe Boolean
    }

instance decodeJsonSku :: DecodeJson Sku where
  decodeJson json = skuCode <|> productOption
    where
    skuCode = SkuCode <$> decodeJson json

    productOption = do
      o <- decodeJson json
      code <- o .: "code"
      description <- o .:? "description"
      name <- o .:? "name"
      platform <- o .:? "platform"
      productCategory <- o .:? "productCategory"
      accountProduct <- o .:? "accountProduct"
      billableProduct <- o .:? "billableProduct"
      commercialProduct <- o .:? "commercialProduct"
      pure
        $ Sku
            { code
            , description
            , name
            , platform
            , productCategory
            , accountProduct
            , billableProduct
            , commercialProduct
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

newtype DimTypeRef
  = DimTypeRef { dimTypeId :: String, solutionUri :: Maybe Uri }

instance decodeJsonDimTypeRef :: DecodeJson DimTypeRef where
  decodeJson json = DimTypeRef <$> plainId <|> full
    where
    plainId = (\id -> { dimTypeId: id, solutionUri: Nothing }) <$> decodeJson json

    full = do
      o <- decodeJson json
      dimTypeId <- o .: "dimTypeID"
      solutionUri <- o .:? "solutionURI"
      pure $ DimTypeRef { dimTypeId, solutionUri }

newtype DimType
  = DimType
  { id :: Maybe String
  , name :: Maybe String
  , description :: Maybe String
  , schema :: Map String ConfigSchemaEntry
  }

instance decodeJsonDimType :: DecodeJson DimType where
  decodeJson json = do
    o <- decodeJson json
    id <- o .: "id"
    name <- o .:? "name"
    description <- o .:? "description"
    schemaObj :: FO.Object ConfigSchemaEntry <- o .: "schema"
    let
      schema = Map.fromFoldable (FO.toUnfoldable schemaObj :: Array _)
    pure
      $ DimType
          { id
          , name
          , description
          , schema
          }

newtype UsageSchemaRef
  = UsageSchemaRef { usageSchemaId :: String, solutionUri :: Maybe Uri }

instance decodeJsonUsageSchemaRef :: DecodeJson UsageSchemaRef where
  decodeJson json = UsageSchemaRef <$> plainId <|> full
    where
    plainId = (\id -> { usageSchemaId: id, solutionUri: Nothing }) <$> decodeJson json

    full = do
      o <- decodeJson json
      usageSchemaId <- o .: "usageSchemaID"
      solutionUri <- o .:? "solutionURI"
      pure $ UsageSchemaRef { usageSchemaId, solutionUri }

newtype UsageSchema
  = UsageSchema
  { id :: Maybe String
  , name :: Maybe String
  , description :: Maybe String
  , dimTypeRefs :: Maybe (Array String)
  }

instance decodeJsonUsageSchema :: DecodeJson UsageSchema where
  decodeJson json = UsageSchema <$> decodeJson json

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
  , endDateExclusive :: Date
  }

instance decodeJsonValidity :: DecodeJson Validity where
  decodeJson json = Validity <$> decodeJson json

newtype MonthlyPriceOverrideElem
  = MonthlyPriceOverrideElem
  { validity :: Validity
  | SimpleChargeRow
  }

instance decodeJsonMonthlyPriceOverrideElem :: DecodeJson MonthlyPriceOverrideElem where
  decodeJson json = MonthlyPriceOverrideElem <$> decodeJson json

newtype MonthlyPriceOverride
  = MonthlyPriceOverride
  { id :: String
  , elements :: Array MonthlyPriceOverrideElem
  }

instance decodeJsonMonthlyPriceOverride :: DecodeJson MonthlyPriceOverride where
  decodeJson json = MonthlyPriceOverride <$> decodeJson json

newtype OnetimePriceOverrideElem
  = OnetimePriceOverrideElem
  { validity :: Validity
  | SimpleChargeRow
  }

instance decodeJsonOnetimePriceOverrideElem :: DecodeJson OnetimePriceOverrideElem where
  decodeJson json = OnetimePriceOverrideElem <$> decodeJson json

newtype OnetimePriceOverride
  = OnetimePriceOverride
  { id :: String
  , elements :: Array OnetimePriceOverrideElem
  }

instance decodeJsonOnetimePriceOverride :: DecodeJson OnetimePriceOverride where
  decodeJson json = OnetimePriceOverride <$> decodeJson json

newtype UsagePriceOverrideElem
  = UsagePriceOverrideElem
  { validity :: Validity
  | UsageChargeRow
  }

instance decodeJsonUsagePriceOverrideElem :: DecodeJson UsagePriceOverrideElem where
  decodeJson json = UsagePriceOverrideElem <$> decodeJson json

newtype UsagePriceOverride
  = UsagePriceOverride
  { id :: String
  , elements :: Array UsagePriceOverrideElem
  }

instance decodeJsonUsagePriceOverride :: DecodeJson UsagePriceOverride where
  decodeJson json = UsagePriceOverride <$> decodeJson json

newtype PriceOverrides
  = PriceOverrides
  { monthlyPriceOverrides :: Array MonthlyPriceOverride
  , onetimePriceOverrides :: Array OnetimePriceOverride
  , usagePriceOverrides :: Array UsagePriceOverride
  }

instance decodeJsonPriceOverrides :: DecodeJson PriceOverrides where
  decodeJson json = PriceOverrides <$> decodeJson json

newtype ProductInstance
  = ProductInstance
  {
  }

instance decodeJsonProductInstance :: DecodeJson ProductInstance where
  decodeJson json = ProductInstance <$> decodeJson json

newtype Asset
  = Asset
  { baseRateCardURI :: Uri
  , priceOverrides :: PriceOverrides
  , product :: ProductInstance
  , quantity :: Int
  }

instance decodeJsonAsset :: DecodeJson Asset where
  decodeJson json = Asset <$> decodeJson json

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

newtype OnetimeChargeOrderline
  = OnetimeChargeOrderline SimpleCharge

instance decodeJsonOnetimeChargeOrderline :: DecodeJson OnetimeChargeOrderline where
  decodeJson json = OnetimeChargeOrderline <$> decodeJson json

-- | Estimated semgement volume.
newtype EstPriceSegment
  = EstPriceSegment
  { minimum :: Int
  , exclusiveMaximum :: Int
  }

instance decodeJsonEstPriceSegment :: DecodeJson EstPriceSegment where
  decodeJson json = EstPriceSegment <$> decodeJson json

newtype MonthlyChargeOrderline
  = MonthlyChargeOrderline
  { estPriceSegment :: Maybe EstPriceSegment
  | SimpleChargeRow
  }

instance decodeJsonMonthlyChargeOrderline :: DecodeJson MonthlyChargeOrderline where
  decodeJson json = MonthlyChargeOrderline <$> decodeJson json

newtype EstWeightByDim
  = EstWeightByDim
  { -- dim :: 
    weight :: Number
  }

instance decodeJsonEstWeightByDim :: DecodeJson EstWeightByDim where
  decodeJson json = EstWeightByDim <$> decodeJson json

newtype UsageChargeOrderline
  = UsageChargeOrderline
  { -- Sum {i in dims} {j in billingUnits  w_ij * p_ij.
    estQuantity :: Int
  , -- Typically this is populated with operator market share data.
    estWeightByDim :: Array EstWeightByDim
  , estPriceSegment :: EstPriceSegment
  | UsageChargeRow
  }

instance decodeJsonUsageChargeOrderline :: DecodeJson UsageChargeOrderline where
  decodeJson json = UsageChargeOrderline <$> decodeJson json

newtype OrderLine
  = OrderLine
  { product :: ProductInstance
  , quantity :: Int
  , onetimeCharges :: Array OnetimeChargeOrderline
  , monthlyCharges :: Array MonthlyChargeOrderline
  , usageCharges :: Array UsageChargeOrderline
  }

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
