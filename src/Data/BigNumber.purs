module Data.BigNumber
  ( BigNumber
  , Format(..)
  , fromInt
  , fromNumber
  , fromString
  , toNumber
  , toStringWith
  ) where

import Prelude
import Data.Maybe (Maybe(..))

foreign import data BigNumber :: Type

foreign import fromInt :: Int -> BigNumber

foreign import fromNumber :: Number -> BigNumber

foreign import _fromString :: (BigNumber -> Maybe BigNumber) -> Maybe BigNumber -> String -> Maybe BigNumber

foreign import toNumber :: BigNumber -> Number

foreign import bigNumberToFixed :: BigNumber -> String

foreign import bigNumberToFixedDp :: Int -> BigNumber -> String

foreign import bigNumberEq :: BigNumber -> BigNumber -> Boolean

foreign import bigNumberZero :: BigNumber

foreign import bigNumberOne :: BigNumber

foreign import bigNumberPlus :: BigNumber -> BigNumber -> BigNumber

foreign import bigNumberSub :: BigNumber -> BigNumber -> BigNumber

foreign import bigNumberMul :: BigNumber -> BigNumber -> BigNumber

foreign import bigNumberDiv :: BigNumber -> BigNumber -> BigNumber

foreign import bigNumberMod :: BigNumber -> BigNumber -> BigNumber

fromString :: String -> Maybe BigNumber
fromString = _fromString Just Nothing

data Format
  = Fixed Int

toString :: BigNumber -> String
toString = bigNumberToFixed

toStringWith :: Format -> BigNumber -> String
toStringWith (Fixed dp) n = bigNumberToFixedDp dp n

instance showBigNumber :: Show BigNumber where
  show = toString

instance eqBigNumber :: Eq BigNumber where
  eq = bigNumberEq

instance semiringBigNumber :: Semiring BigNumber where
  add = bigNumberPlus
  zero = bigNumberZero
  mul = bigNumberMul
  one = bigNumberOne

instance ringBigNumber :: Ring BigNumber where
  sub = bigNumberSub

instance commutativeRingBigNumber :: CommutativeRing BigNumber

instance euclideanRingBigNumber :: EuclideanRing BigNumber where
  degree = const 1
  div = bigNumberDiv
  mod = bigNumberMod
