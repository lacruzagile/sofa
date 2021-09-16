module Test.Main where

import Prelude
import Data.Argonaut (printJsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.SmartSpec as SS
import Effect (Effect)
import Effect.Class.Console (log)
import Test.QuickCheck (quickCheck, (<?>))

main :: Effect Unit
main = do
  smartSpecPricePerSegmentJsonCheck

test :: String -> Effect Unit -> Effect Unit
test name run = do
  log name
  run

smartSpecPricePerSegmentJsonCheck :: Effect Unit
smartSpecPricePerSegmentJsonCheck =
  test "Test EncodeJson/DecodeJson on PricePerSegment"
    $ quickCheck \(x :: SS.PricePerSegment) -> case decodeJson (encodeJson x) of
        Right decoded ->
          decoded == x
            <?> ( " x = "
                  <> show x
                  <> ", decoded = "
                  <> show decoded
              )
        Left err -> false <?> printJsonDecodeError err
