module Test.Main where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, printJsonDecodeError)
import Data.Currency (unsafeMkCurrency)
import Data.BigNumber as BN
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.SmartSpec as SS
import Data.SubTotal (SubTotal(..), IndexedSubTotalEntry(..), calcSubTotal)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.QuickCheck ((<?>))
import Test.QuickCheck as QC
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
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
        , periodMinimum: Nothing
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
        , periodMinimum: Nothing
        }
