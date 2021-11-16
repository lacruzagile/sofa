module Test.Main where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Estimate (Estimate(..))
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
import Data.Map as Map

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

    currency = SS.ChargeCurrency (SS.Currency "EUR")

    unitId = SS.ChargeUnitId "uid"

    quantity n = Map.singleton unitId (Left $ Exact n)

    unitMap =
      Map.singleton unitId
        $ SS.ChargeUnit
            { id: unitId
            , title: Nothing
            , description: Nothing
            , kind: SS.CkOnetime
            , priceDimSchema: Nothing
            , reportDimSchemas: Nothing
            }

    subTotal n =
      SubTotal
        { onetime: IndexedSubTotalEntry (Map.singleton currency n)
        , monthly: IndexedSubTotalEntry Map.empty
        , quarterly: IndexedSubTotalEntry Map.empty
        , usage: IndexedSubTotalEntry Map.empty
        , segment: IndexedSubTotalEntry Map.empty
        }
  it "can calculate sub-total"
    $ subTotal (Exact { listPrice: Additive 250.0, price: Additive 225.0 })
        `shouldEqual`
          calcSubTotal (quantity 25) unitMap currency charge

calcSubTotalChargeSegVolume :: Spec Unit
calcSubTotalChargeSegVolume = do
  let
    charge = exampleChargeSeg SS.SegmentationModelVolume

    currency = SS.ChargeCurrency (SS.Currency "EUR")

    unitId = SS.ChargeUnitId "uid"

    quantity n = Map.singleton unitId (Left $ Exact n)

    unitMap =
      Map.singleton unitId
        ( SS.ChargeUnit
            { id: unitId
            , title: Nothing
            , description: Nothing
            , kind: SS.CkOnetime
            , priceDimSchema: Nothing
            , reportDimSchemas: Nothing
            }
        )

    subTotal n =
      SubTotal
        { onetime: IndexedSubTotalEntry (Map.singleton currency n)
        , monthly: IndexedSubTotalEntry Map.empty
        , quarterly: IndexedSubTotalEntry Map.empty
        , usage: IndexedSubTotalEntry Map.empty
        , segment: IndexedSubTotalEntry Map.empty
        }
  it "can calculate sub-total using volume segmentation"
    $ subTotal (Exact { listPrice: Additive 25.0, price: Additive 12.5 })
        `shouldEqual`
          calcSubTotal (quantity 25) unitMap currency charge

calcSubTotalChargeSegTiered :: Spec Unit
calcSubTotalChargeSegTiered = do
  let
    charge = exampleChargeSeg SS.SegmentationModelTiered

    currency = SS.ChargeCurrency (SS.Currency "EUR")

    unitId = SS.ChargeUnitId "uid"

    quantity n = Map.singleton unitId (Left $ Exact n)

    unitMap =
      Map.singleton unitId
        ( SS.ChargeUnit
            { id: unitId
            , title: Nothing
            , description: Nothing
            , kind: SS.CkOnetime
            , priceDimSchema: Nothing
            , reportDimSchemas: Nothing
            }
        )

    subTotal n =
      SubTotal
        { onetime: IndexedSubTotalEntry (Map.singleton currency n)
        , monthly: IndexedSubTotalEntry Map.empty
        , quarterly: IndexedSubTotalEntry Map.empty
        , usage: IndexedSubTotalEntry Map.empty
        , segment: IndexedSubTotalEntry Map.empty
        }
  it "can calculate sub-total using tiered segmentation, zero"
    $ subTotal (Exact { listPrice: Additive 0.0, price: Additive 0.0 })
        `shouldEqual`
          calcSubTotal (quantity 0) unitMap currency charge
  it "can calculate sub-total using tiered segmentation, one tier"
    $ subTotal (Exact { listPrice: Additive 50.0, price: Additive 45.0 })
        `shouldEqual`
          calcSubTotal (quantity 5) unitMap currency charge
  it "can calculate sub-total using tiered segmentation, two tiers (min)"
    $ subTotal (Exact { listPrice: Additive 105.0, price: Additive 94.0 })
        `shouldEqual`
          calcSubTotal (quantity 11) unitMap currency charge
  it "can calculate sub-total using tiered segmentation, two tiers (max)"
    $ subTotal (Exact { listPrice: Additive 150.0, price: Additive 130.0 })
        `shouldEqual`
          calcSubTotal (quantity 20) unitMap currency charge
  it "can calculate sub-total using tiered segmentation, three tiers (min)"
    $ subTotal (Exact { listPrice: Additive 151.0, price: Additive 130.5 })
        `shouldEqual`
          calcSubTotal (quantity 21) unitMap currency charge
  it "can calculate sub-total using tiered segmentation, three tiers (bigger)"
    $ subTotal (Exact { listPrice: Additive 160.0, price: Additive 135.0 })
        `shouldEqual`
          calcSubTotal (quantity 30) unitMap currency charge

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
        , termOfPriceChangeInDays: Nothing
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
        , termOfPriceChangeInDays: Nothing
        }
