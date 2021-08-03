module Data.SmartSpec
  ( BillingUnit(..)
  , BillingUnitRef(..)
  , ChargeType(..)
  , ConfigSchemaEntry(..)
  , Currency(..)
  , DefaultUnitPriceByBillingUnit(..)
  , DimType(..)
  , DimTypeRef(..)
  , Meta(..)
  , MonthlyCharge(..)
  , OnetimeCharge(..)
  , Platform(..)
  , Price(..)
  , Product(..)
  , ProductCategory(..)
  , ProductFeature(..)
  , ProductOption(..)
  , ProductOptionType(..)
  , RateCard(..)
  , RateElementMonthly(..)
  , RateElementOnetime(..)
  , RateElementSimple(..)
  , RateElementUsage(..)
  , Segmentation(..)
  , SegmentationByBillingUnit(..)
  , SegmentationModel(..)
  , SegmentationPeriod(..)
  , SegmentedPrice(..)
  , Sku(..)
  , Solution(..)
  , Uri(..)
  , UsageCharge(..)
  , UsageSchemaRef(..)
  , UsageSchemaRefByBillingUnit(..)
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
  = { solutions :: Array String
    }

type Solution
  = { description :: String
    , dimTypes :: Array DimType
    , products :: Array Product
    , prices :: Array Price
    , billingUnits :: Array BillingUnit
    }

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

type Price
  = { name :: String
    , currency :: Currency
    , rateCardPathPrefix :: Maybe String
    , rateCards :: Maybe (Array RateCard)
    }

data RateCard
  = RateCardPath String
  | RateCardPeriodic
    { sku :: Sku
    , name :: Maybe String
    , currency :: Currency
    , onetimeCharge :: RateElementOnetime
    , onetimeCharges :: Maybe (Array OnetimeCharge)
    , monthlyCharge :: RateElementMonthly
    , monthlyCharges :: Maybe (Array MonthlyCharge)
    }
  | RateCardUsage
    { sku :: Sku
    , name :: Maybe String
    , currency :: Currency
    , usageCharge :: RateElementUsage
    , usageCharges :: Maybe (Array UsageCharge)
    }

instance decodeJsonRateCard :: DecodeJson RateCard where
  decodeJson json = rateCardPath <|> rateCardPeriodic <|> rateCardUsage
    where
    rateCardPath = RateCardPath <$> decodeJson json

    rateCardPeriodic = do
      o <- decodeJson json
      sku <- o .: "sku"
      name <- o .:? "name"
      currency <- o .: "currency"
      onetimeCharge <- o .: "onetimeCharge"
      onetimeCharges <- o .:? "onetimeCharges"
      monthlyCharge <- o .: "monthlyCharge"
      monthlyCharges <- o .:? "monthlyCharges"
      pure
        $ RateCardPeriodic
            { sku
            , name
            , currency
            , onetimeCharge
            , onetimeCharges
            , monthlyCharge
            , monthlyCharges
            }

    rateCardUsage = do
      o <- decodeJson json
      sku <- o .: "sku"
      name <- o .:? "name"
      currency <- o .: "currency"
      usageCharge <- o .: "usageCharge"
      usageCharges <- o .:? "usageCharges"
      pure
        $ RateCardUsage
            { sku
            , name
            , currency
            , usageCharge
            , usageCharges
            }

newtype OnetimeCharge
  = OnetimeCharge
  { id :: String, element :: RateElementOnetime
  }

instance decodeJsonOnetimeCharge :: DecodeJson OnetimeCharge where
  decodeJson json = OnetimeCharge <$> decodeJson json

newtype MonthlyCharge
  = MonthlyCharge
  { id :: String, element :: RateElementMonthly
  }

instance decodeJsonMonthlyCharge :: DecodeJson MonthlyCharge where
  decodeJson json = MonthlyCharge <$> decodeJson json

newtype UsageCharge
  = UsageCharge
  { id :: String, element :: RateElementUsage
  }

instance decodeJsonUsageCharge :: DecodeJson UsageCharge where
  decodeJson json = UsageCharge <$> decodeJson json

newtype RateElementSimple
  = RateElementSimple
  { billingUnitRef :: BillingUnitRef
  , price :: Number
  }

instance decodeJsonRateElementSimple :: DecodeJson RateElementSimple where
  decodeJson json = RateElementSimple <$> decodeJson json

type RateElementOnetime
  = RateElementSimple

type RateElementMonthly
  = RateElementSimple

newtype RateElementUsage
  = RateElementUsage
  { termOfPriceChangeInDays :: Int
  , dimTypeRef :: Maybe DimTypeRef
  , billingUnitRefs :: Maybe (Array BillingUnitRef)
  , usageSchemaRefByBillingUnit :: Maybe UsageSchemaRefByBillingUnit
  , segmentationByBillingUnit :: Maybe SegmentationByBillingUnit
  , defaultUnitPriceByBillingUnit :: Maybe DefaultUnitPriceByBillingUnit
  -- , unitPricePerDimByBillingUnit :: Maybe UnitPricePerDimByBillingUnit
  , monthlyMinimum :: Number
  }

instance decodeJsonRateElementUsage :: DecodeJson RateElementUsage where
  decodeJson json = do
    o <- decodeJson json
    termOfPriceChangeInDays <- o .:? "termOfPriceChangeInDays" .!= 0
    dimTypeRef <- o .:? "dimTypeRef"
    billingUnitRefs <- o .:? "billingUnitRefs"
    usageSchemaRefByBillingUnit <- o .:? "usageSchemaRefByBillingUnit"
    segmentationByBillingUnit <- o .:? "segmentationByBillingUnit"
    defaultUnitPriceByBillingUnit <- o .:? "defaultUnitPriceByBillingUnit"
    monthlyMinimum <- o .:? "monthlyMinimum" .!= 0.0
    pure
      $ RateElementUsage
          { termOfPriceChangeInDays
          , dimTypeRef
          , billingUnitRefs
          , usageSchemaRefByBillingUnit
          , segmentationByBillingUnit
          , defaultUnitPriceByBillingUnit
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

-- newtype UnitPricePerDimByBillingUnit = UnitPricePerDimByBillingUnit {
--   dim :: Json
--   , unitPricesByBillingUnit :: Array UnitPriceByBillingUnit
--                                                                     }
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

type BillingUnit
  = { id :: Maybe String
    , name :: Maybe String
    , chargeType :: ChargeType
    , description :: Maybe String
    }

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

type ProductFeature
  = { name :: Maybe String
    , description :: Maybe String
    , options :: Maybe (Array Json)
    }

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

type DimType
  = { id :: Maybe String
    , name :: Maybe String
    , description :: Maybe String
    , schema :: Maybe Json
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

type UsageSchema
  = { id :: Maybe String
    , name :: Maybe String
    , description :: Maybe String
    , dimTypeRefs :: Maybe (Array String)
    }
