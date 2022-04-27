module Test.Main (main) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object as FO
import Sofa.Data.BigNumber as BN
import Sofa.Data.ByteSize as ByteSize
import Sofa.Data.Currency (unsafeMkCurrency)
import Sofa.Data.Schema (isValidValue)
import Sofa.Data.SmartSpec as SS
import Sofa.Data.SubTotal (SubTotal(..), IndexedSubTotalEntry(..), calcSubTotal)
import Test.QuickCheck ((<?>))
import Test.QuickCheck as QC
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_ $ runSpec [ consoleReporter ]
    $ do
        describe "Smart Spec Data Model" do
          smartSpecPriceJsonCheck
          smartSpecPricePerSegJsonCheck
        describe "Sub Total" do
          describe "ChargeSimple" do
            calcSubTotalChargeSimple
          describe "ChargeSeg" do
            calcSubTotalChargeSegVolume
            calcSubTotalChargeSegTiered
        describe "Schema Validation" do
          describe "boolean" do
            validatesBoolean
          describe "integer" do
            validatesInteger
          describe "string" do
            validatesString
          describe "regex" do
            validatesRegex
          describe "const" do
            validatesConst
          describe "array" do
            validatesArray
          describe "object" do
            validatesObject
          describe "oneOf" do
            validatesOneOf
        describe "ByteSize" do
          describe "showPretty" do
            validatesByteSizeShowPretty

-- | A EUR charge currency.
eurChargeCurrency :: SS.ChargeCurrency
eurChargeCurrency = SS.ChargeCurrency (unsafeMkCurrency "EUR")

afn :: Number -> Additive BN.BigNumber
afn = Additive <<< BN.fromNumber

smartSpecPriceJsonCheck :: Spec Unit
smartSpecPriceJsonCheck =
  it "has consistent EncodeJson/DecodeJson on Price"
    $ quickCheck \x -> checkEncodeDecode (SS.Price x)

smartSpecPricePerSegJsonCheck :: Spec Unit
smartSpecPricePerSegJsonCheck =
  it "has consistent EncodeJson/DecodeJson on PricePerSeg"
    $ quickCheck \x -> checkEncodeDecode (SS.PricePerSeg x)

checkEncodeDecode :: forall a. DecodeJson a => EncodeJson a => Eq a => Show a => a -> QC.Result
checkEncodeDecode x = case decodeJson (encodeJson x) of
  Right decoded ->
    decoded == x
      <?> ( " x = "
            <> show x
            <> ", decoded = "
            <> show decoded
        )
  Left err -> false <?> (" x = " <> show x <> ": " <> printJsonDecodeError err)

calcSubTotalChargeSimple :: Spec Unit
calcSubTotalChargeSimple = do
  let
    charge = exampleChargeSimple

    unitId = SS.ChargeUnitId "uid"

    unitMap =
      Map.singleton unitId
        $ SS.ChargeUnit
            { id: unitId
            , title: Nothing
            , description: Nothing
            , kind: SS.CkOnetime
            , priceDimSchema: Nothing
            , reportDimSchema: Nothing
            }

    subTotal n =
      SubTotal
        { onetime: IndexedSubTotalEntry (Map.singleton eurChargeCurrency n)
        , monthly: IndexedSubTotalEntry Map.empty
        , quarterly: IndexedSubTotalEntry Map.empty
        , usage: IndexedSubTotalEntry Map.empty
        , segment: IndexedSubTotalEntry Map.empty
        }
  it "can calculate sub-total"
    $ subTotal { listPrice: afn 250.0, price: afn 225.0 }
        `shouldEqual`
          calcSubTotal 25 Map.empty unitMap eurChargeCurrency charge

calcSubTotalChargeSegVolume :: Spec Unit
calcSubTotalChargeSegVolume = do
  let
    charge = exampleChargeSeg SS.SegmentationModelVolume

    unitId = SS.ChargeUnitId "uid"

    unitMap =
      Map.singleton unitId
        ( SS.ChargeUnit
            { id: unitId
            , title: Nothing
            , description: Nothing
            , kind: SS.CkOnetime
            , priceDimSchema: Nothing
            , reportDimSchema: Nothing
            }
        )

    subTotal n =
      SubTotal
        { onetime: IndexedSubTotalEntry (Map.singleton eurChargeCurrency n)
        , monthly: IndexedSubTotalEntry Map.empty
        , quarterly: IndexedSubTotalEntry Map.empty
        , usage: IndexedSubTotalEntry Map.empty
        , segment: IndexedSubTotalEntry Map.empty
        }
  it "can calculate sub-total using volume segmentation"
    $ subTotal { listPrice: afn 25.0, price: afn 12.5 }
        `shouldEqual`
          calcSubTotal 25 Map.empty unitMap eurChargeCurrency charge

calcSubTotalChargeSegTiered :: Spec Unit
calcSubTotalChargeSegTiered = do
  let
    charge = exampleChargeSeg SS.SegmentationModelTiered

    unitId = SS.ChargeUnitId "uid"

    unitMap =
      Map.singleton unitId
        ( SS.ChargeUnit
            { id: unitId
            , title: Nothing
            , description: Nothing
            , kind: SS.CkOnetime
            , priceDimSchema: Nothing
            , reportDimSchema: Nothing
            }
        )

    subTotal n =
      SubTotal
        { onetime: IndexedSubTotalEntry (Map.singleton eurChargeCurrency n)
        , monthly: IndexedSubTotalEntry Map.empty
        , quarterly: IndexedSubTotalEntry Map.empty
        , usage: IndexedSubTotalEntry Map.empty
        , segment: IndexedSubTotalEntry Map.empty
        }
  it "can calculate sub-total using tiered segmentation, zero"
    $ subTotal { listPrice: afn 0.0, price: afn 0.0 }
        `shouldEqual`
          calcSubTotal 0 Map.empty unitMap eurChargeCurrency charge
  it "can calculate sub-total using tiered segmentation, one tier"
    $ subTotal { listPrice: afn 50.0, price: afn 45.0 }
        `shouldEqual`
          calcSubTotal 5 Map.empty unitMap eurChargeCurrency charge
  it "can calculate sub-total using tiered segmentation, two tiers (min)"
    $ subTotal { listPrice: afn 105.0, price: afn 94.0 }
        `shouldEqual`
          calcSubTotal 11 Map.empty unitMap eurChargeCurrency charge
  it "can calculate sub-total using tiered segmentation, two tiers (max)"
    $ subTotal { listPrice: afn 150.0, price: afn 130.0 }
        `shouldEqual`
          calcSubTotal 20 Map.empty unitMap eurChargeCurrency charge
  it "can calculate sub-total using tiered segmentation, three tiers (min)"
    $ subTotal { listPrice: afn 151.0, price: afn 130.5 }
        `shouldEqual`
          calcSubTotal 21 Map.empty unitMap eurChargeCurrency charge
  it "can calculate sub-total using tiered segmentation, three tiers (bigger)"
    $ subTotal { listPrice: afn 160.0, price: afn 135.0 }
        `shouldEqual`
          calcSubTotal 30 Map.empty unitMap eurChargeCurrency charge

exampleChargeSimple :: SS.Charge
exampleChargeSimple =
  SS.ChargeSingleUnit
    $ SS.ChargeSimple
        { unit: SS.ChargeUnitId "uid"
        , currency: Nothing
        , description: Nothing
        , listPrice: 10.0
        , price: 9.0
        , discount: Nothing
        }

exampleChargeSeg :: SS.SegmentationModel -> SS.Charge
exampleChargeSeg model =
  SS.ChargeSingleUnit
    $ SS.ChargeSeg
        { unit: SS.ChargeUnitId "uid"
        , currency: Nothing
        , description: Nothing
        , segmentation:
            SS.Segmentation
              { unit: Nothing
              , model
              , segments:
                  [ SS.Segment { minimum: 0, exclusiveMaximum: Just 11 }
                  , SS.Segment { minimum: 11, exclusiveMaximum: Just 21 }
                  , SS.Segment { minimum: 21, exclusiveMaximum: Nothing }
                  ]
              }
        , priceBySegment:
            [ SS.PricePerSeg
                { minimum: 0
                , exclusiveMaximum: Just 11
                , listPrice: 10.0
                , price: 9.0
                , discount: Just $ SS.DiscountPercentage 1.0
                }
            , SS.PricePerSeg
                { minimum: 11
                , exclusiveMaximum: Just 21
                , listPrice: 5.0
                , price: 4.0
                , discount: Just $ SS.DiscountAbsolute 1.0
                }
            , SS.PricePerSeg
                { minimum: 21
                , exclusiveMaximum: Nothing
                , listPrice: 1.0
                , price: 0.5
                , discount: Just $ SS.DiscountAbsolute 0.5
                }
            ]
        }

validatesBoolean :: Spec Unit
validatesBoolean = do
  it "validates boolean true"
    $ SS.CvBoolean true `shouldSatisfy` isValidValue schema
  it "validates boolean false"
    $ SS.CvBoolean false `shouldSatisfy` isValidValue schema
  where
  schema =
    SS.CseBoolean
      { title: Nothing
      , description: Nothing
      , "default": Nothing
      }

validatesInteger :: Spec Unit
validatesInteger = do
  it "validates integer"
    $ quickCheck \i -> isValidValue schema (SS.CvInteger i)
  where
  schema =
    SS.CseInteger
      { title: Nothing
      , description: Nothing
      , "default": Nothing
      , minimum: Nothing
      , maximum: Nothing
      , enum: []
      , widget: Nothing
      }

validatesString :: Spec Unit
validatesString = do
  it "validates plain string"
    $ quickCheck \i -> isValidValue schema (SS.CvString i)
  it "validates string pattern matching"
    $ SS.CvString "abcde" `shouldSatisfy` isValidValue schemaWithPattern
  it "validates string pattern not matching"
    $ SS.CvString "aaaa" `shouldNotSatisfy` isValidValue schemaWithPattern
  where
  -- The most basic string schema that allows all strings.
  schema =
    SS.CseString
      { title: Nothing
      , description: Nothing
      , minLength: Nothing
      , maxLength: Nothing
      , enum: []
      , pattern: Nothing
      , "default": Nothing
      , widget: Nothing
      }

  schemaWithPattern =
    SS.CseString
      { title: Nothing
      , description: Nothing
      , minLength: Nothing
      , maxLength: Nothing
      , enum: []
      , pattern: Just "^a.c"
      , "default": Nothing
      , widget: Nothing
      }

validatesRegex :: Spec Unit
validatesRegex = do
  it "validates regex matching"
    $ SS.CvString "abcde" `shouldSatisfy` isValidValue schema
  it "validates regex not matching"
    $ SS.CvString "aaaa" `shouldNotSatisfy` isValidValue schema
  where
  schema =
    SS.CseRegex
      { title: Nothing
      , description: Nothing
      , pattern: "^a.c"
      , "default": Nothing
      , widget: Nothing
      }

validatesConst :: Spec Unit
validatesConst = do
  it "validates const equal"
    $ SS.CvInteger 10 `shouldSatisfy` isValidValue schema
  it "validates const not equal"
    $ SS.CvInteger 5 `shouldNotSatisfy` isValidValue schema
  where
  schema =
    SS.CseConst
      { title: Nothing
      , description: Nothing
      , const: SS.CvInteger 10
      }

validatesArray :: Spec Unit
validatesArray = do
  it "validates array matching items"
    $ quickCheck \is -> isValidValue schema (SS.CvArray (SS.CvInteger <$> is))
  it "validates array not matching item"
    $ badValue `shouldNotSatisfy` isValidValue schema
  where
  badValue = SS.CvArray [ SS.CvInteger 4, SS.CvBoolean true ]

  schema =
    SS.CseArray
      { title: Nothing
      , description: Nothing
      , items:
          SS.CseInteger
            { title: Nothing
            , description: Nothing
            , "default": Nothing
            , minimum: Nothing
            , maximum: Nothing
            , enum: []
            , widget: Nothing
            }
      , widget: Nothing
      }

validatesObject :: Spec Unit
validatesObject = do
  it "validates object matching"
    $ okValue `shouldSatisfy` isValidValue schema
  it "validates object bad field"
    $ badFieldValue `shouldNotSatisfy` isValidValue schema
  it "validates object missing field"
    $ missingFieldValue `shouldNotSatisfy` isValidValue schema
  where
  okValue =
    SS.CvObject
      $ Map.fromFoldable
          [ Tuple "bool" (SS.CvBoolean true)
          , Tuple "int" (SS.CvInteger 5)
          ]

  badFieldValue =
    SS.CvObject
      $ Map.fromFoldable
          [ Tuple "bool" (SS.CvInteger 2)
          , Tuple "int" (SS.CvInteger 5)
          ]

  missingFieldValue =
    SS.CvObject
      $ Map.fromFoldable
          [ Tuple "bool" (SS.CvBoolean true)
          ]

  schema =
    SS.CseObject
      { title: Nothing
      , description: Nothing
      , properties:
          FO.fromHomogeneous
            { bool:
                SS.CseBoolean
                  { title: Nothing
                  , description: Nothing
                  , "default": Nothing
                  }
            , int:
                SS.CseInteger
                  { title: Nothing
                  , description: Nothing
                  , "default": Nothing
                  , minimum: Nothing
                  , maximum: Nothing
                  , enum: []
                  , widget: Nothing
                  }
            }
      , widget: Nothing
      }

validatesOneOf :: Spec Unit
validatesOneOf = do
  it "validates oneOf matching #1"
    $ SS.CvBoolean true `shouldSatisfy` isValidValue schema
  it "validates oneOf matching #2"
    $ SS.CvInteger 2 `shouldSatisfy` isValidValue schema
  it "validates oneOf not matching"
    $ SS.CvString "foo" `shouldNotSatisfy` isValidValue schema
  where
  schema =
    SS.CseOneOf
      { title: Nothing
      , description: Nothing
      , oneOf:
          [ SS.CseBoolean
              { title: Nothing
              , description: Nothing
              , "default": Nothing
              }
          , SS.CseInteger
              { title: Nothing
              , description: Nothing
              , "default": Nothing
              , minimum: Nothing
              , maximum: Nothing
              , enum: []
              , widget: Nothing
              }
          ]
      }

validatesByteSizeShowPretty :: Spec Unit
validatesByteSizeShowPretty = do
  it "validates showPretty for 120"
    $ ByteSize.showPretty 120.0 `shouldEqual` "120.0 B"
  it "validates showPretty for 1234.0"
    $ ByteSize.showPretty 1234.0 `shouldEqual` "1.2 KiB"
  it "validates showPretty for 9876543.0"
    $ ByteSize.showPretty 9876543.0 `shouldEqual` "9.4 MiB"
  it "validates showPretty for 9876543210.0"
    $ ByteSize.showPretty 9876543210.0 `shouldEqual` "9.2 GiB"
