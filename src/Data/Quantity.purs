-- A collection of data types and functions to make it easier to work with Smart
-- Spec quantities.
module Data.Quantity
  ( AggregatedQuantity
  , Quantity
  , QuantityMap
  , QuantityMapEntry
  , fromSmartSpecQuantity
  , toSmartSpecQuantity
  ) where

import Prelude
import Data.Array as A
import Data.Either (Either(..))
import Data.Estimate (Estimate(..))
import Data.Estimate as Est
import Data.List.Lazy (List)
import Data.Map (Map)
import Data.Map as Map
import Data.SmartSpec (ChargeUnitID, DimValue, QuantityPerDim(..), QuantityPerUnit(..))
import Data.Tuple (Tuple(..))

-- | The quantity is either set as an aggregate or individually for each of the
-- | unit's dimensions.
type QuantityMap
  = Map ChargeUnitID QuantityMapEntry

type QuantityMapEntry
  = Either Quantity (Map DimValue Quantity)

-- | The aggregated quantity has one quantity per unit. Note, if per-dimension
-- | quantities are used and a dimension quantity is missing then the aggregated
-- | value is `Nothing`.
type AggregatedQuantityMap
  = Map ChargeUnitID AggregatedQuantity

-- | A quantity is a non-negative integer that is either exact or estimated.
type Quantity
  = Estimate Int

-- | An aggregated quantity can be either the sum of constituent quantities
-- | (exact) or an estimation of the total quantity (estimate).
newtype AggregatedQuantity
  = AggregatedQuantity (Estimate Quantity)

fromSmartSpecQuantity :: Array QuantityPerUnit -> QuantityMap
fromSmartSpecQuantity = Map.unions <<< map fromQuantityPerUnit
  where
  toEst quantity estimated
    | estimated = Estimate quantity
    | otherwise = Exact quantity

  fromQuantityPerUnit qpu = case qpu of
    QuantityPerUnit { unit, quantity, estimated } -> Map.singleton unit (Left $ toEst quantity estimated)
    QuantityByDimPerUnit { unit, quantityByDim } -> Map.singleton unit (Right $ fromQuantityByDim quantityByDim)

  fromQuantityByDim = Map.fromFoldable <<< map fromQuantityPerDim

  fromQuantityPerDim (QuantityPerDim { dim, quantity, estimated }) = Tuple dim (toEst quantity estimated)

toSmartSpecQuantity :: QuantityMap -> Array QuantityPerUnit
toSmartSpecQuantity quantityMap = transform r1 quantityMap
  where
  toList :: forall k v. Map k v -> List (Tuple k v)
  toList = Map.toUnfoldable

  transform :: forall k v a. (Tuple k v -> a) -> Map k v -> Array a
  transform f = A.fromFoldable <<< map f <<< toList

  r1 :: Tuple ChargeUnitID (Either (Estimate Int) (Map DimValue (Estimate Int))) -> QuantityPerUnit
  r1 (Tuple unitID unitMap) = case unitMap of
    Left quantity ->
      QuantityPerUnit
        { unit: unitID
        , quantity: Est.toValue quantity
        , estimated: Est.isEstimate quantity
        }
    Right dimMap ->
      QuantityByDimPerUnit
        { unit: unitID
        , quantityByDim: transform r2 dimMap
        }

  r2 :: Tuple DimValue (Estimate Int) -> QuantityPerDim
  r2 (Tuple dim quantity) =
    QuantityPerDim
      { dim
      , quantity: Est.toValue quantity
      , estimated: Est.isEstimate quantity
      }
