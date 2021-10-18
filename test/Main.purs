module Test.Main where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Estimate (Estimate(..))
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.SmartSpec as SS
import Data.SubTotal (calcSubTotalEntry)
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
        describe "Smart Spec Data Model"
          $ do
              smartSpecPricePerSegmentJsonCheck
        describe "Sub Total"
          $ do
              calcSubTotalEntryVolume
              calcSubTotalEntryTiered

smartSpecPricePerSegmentJsonCheck :: Spec Unit
smartSpecPricePerSegmentJsonCheck =
  it "has consistent EncodeJson/DecodeJson on PricePerSegment"
    $ quickCheck \(x :: SS.PricePerSegment) -> checkEncodeDecode x

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

calcSubTotalEntryVolume :: Spec Unit
calcSubTotalEntryVolume =
  it "can calculate sub-total using volume segmentation"
    $ { listPrice: Exact (Additive 25.0), salesPrice: Exact (Additive 12.5) }
        `shouldEqual`
          calcSubTotalEntry (Exact 25) SS.SegmentationModelVolume
            examplePrice

calcSubTotalEntryTiered :: Spec Unit
calcSubTotalEntryTiered = do
  it "can calculate sub-total using tiered segmentation, zero"
    $ { listPrice: Exact (Additive 0.0), salesPrice: Exact (Additive 0.0) }
        `shouldEqual`
          calcSubTotalEntry (Exact 0) SS.SegmentationModelTiered
            examplePrice
  it "can calculate sub-total using tiered segmentation, one tier"
    $ { listPrice: Exact (Additive 50.0), salesPrice: Exact (Additive 45.0) }
        `shouldEqual`
          calcSubTotalEntry (Exact 5) SS.SegmentationModelTiered
            examplePrice
  it "can calculate sub-total using tiered segmentation, two tiers (min)"
    $ { listPrice: Exact (Additive 105.0), salesPrice: Exact (Additive 94.0) }
        `shouldEqual`
          calcSubTotalEntry (Exact 11) SS.SegmentationModelTiered
            examplePrice
  it "can calculate sub-total using tiered segmentation, two tiers (max)"
    $ { listPrice: Exact (Additive 150.0), salesPrice: Exact (Additive 130.0) }
        `shouldEqual`
          calcSubTotalEntry (Exact 20) SS.SegmentationModelTiered
            examplePrice
  it "can calculate sub-total using tiered segmentation, three tiers (min)"
    $ { listPrice: Exact (Additive 151.0), salesPrice: Exact (Additive 130.5) }
        `shouldEqual`
          calcSubTotalEntry (Exact 21) SS.SegmentationModelTiered
            examplePrice
  it "can calculate sub-total using tiered segmentation, three tiers (bigger)"
    $ { listPrice: Exact (Additive 160.0), salesPrice: Exact (Additive 135.0) }
        `shouldEqual`
          calcSubTotalEntry (Exact 30) SS.SegmentationModelTiered
            examplePrice

examplePrice :: SS.Price
examplePrice =
  SS.Price
    [ SS.PricePerSegment
        { minimum: 0
        , exclusiveMaximum: Just 11
        , listPrice: 10.0
        , salesPrice: 9.0
        , discount: Nothing
        }
    , SS.PricePerSegment
        { minimum: 11
        , exclusiveMaximum: Just 21
        , listPrice: 5.0
        , salesPrice: 4.0
        , discount: Nothing
        }
    , SS.PricePerSegment
        { minimum: 21
        , exclusiveMaximum: Nothing
        , listPrice: 1.0
        , salesPrice: 0.5
        , discount: Nothing
        }
    ]
