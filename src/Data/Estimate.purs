module Data.Estimate (Estimate(..), isExact, isEstimate, toValue) where

import Prelude

-- | Representation of a value that is either _exact_ or _estimated_.
-- |
-- | The associated monoid instance extends the underlying monoid with the extra
-- | rule that appending two exact value produces an exact value, otherwise the
-- | result is an estimated value.
data Estimate a
  = Exact a
  | Estimate a

derive instance eqEstimate :: Eq a => Eq (Estimate a)

derive instance functorEstimate :: Functor Estimate

instance showEstimate :: Show a => Show (Estimate a) where
  show = case _ of
    Exact n -> show n
    Estimate n -> "~" <> show n

instance semigroupEstimate :: Semigroup a => Semigroup (Estimate a) where
  append a b = case a of
    Exact a' -> case b of
      Exact b' -> Exact (a' <> b')
      Estimate b' -> Estimate (a' <> b')
    Estimate a' -> Estimate (a' <> toValue b)

instance monoidEstimate :: Monoid a => Monoid (Estimate a) where
  mempty = Exact mempty

isExact :: forall a. Estimate a -> Boolean
isExact = case _ of
  Exact _ -> true
  Estimate _ -> false

isEstimate :: forall a. Estimate a -> Boolean
isEstimate = not <<< isExact

-- | Discards whether the value is exact or an estimate and returns just the
-- | value.
toValue :: forall a. Estimate a -> a
toValue = case _ of
  Exact x -> x
  Estimate x -> x
