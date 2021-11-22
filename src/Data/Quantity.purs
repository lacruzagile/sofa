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
import Data.List.Lazy (List)
import Data.Map (Map)
import Data.Map as Map
import Data.SmartSpec (ChargeUnitId, DimValue, EstimatedUsagePerDim(..), EstimatedUsage(..))
import Data.Tuple (Tuple(..))

-- | The quantity is either set as an aggregate or individually for each of the
-- | unit's dimensions.
type QuantityMap
  = Map ChargeUnitId QuantityMapEntry

type QuantityMapEntry
  = Either Quantity (Map DimValue Quantity)

-- | The aggregated quantity has one quantity per unit. Note, if per-dimension
-- | quantities are used and a dimension quantity is missing then the aggregated
-- | value is `Nothing`.
type AggregatedQuantityMap
  = Map ChargeUnitId AggregatedQuantity

-- | A quantity is a non-negative integer that is either exact or estimated.
type Quantity
  = Int

-- | An aggregated quantity can be either the sum of constituent quantities
-- | (exact) or an estimation of the total quantity (estimate).
newtype AggregatedQuantity
  = AggregatedQuantity Quantity

fromSmartSpecQuantity :: Array EstimatedUsage -> QuantityMap
fromSmartSpecQuantity = Map.unions <<< map fromEstimatedUsage
  where
  fromEstimatedUsage qpu = case qpu of
    EstimatedUsagePerUnit { unit, usage } -> Map.singleton unit (Left usage)
    EstimatedUsageByDimPerUnit { unit, usageByDim } -> Map.singleton unit (Right $ fromEstimatedUsageByDim usageByDim)

  fromEstimatedUsageByDim = Map.fromFoldable <<< map fromEstimatedUsagePerDim

  fromEstimatedUsagePerDim (EstimatedUsagePerDim { dim, usage }) = Tuple dim usage

toSmartSpecQuantity :: QuantityMap -> Array EstimatedUsage
toSmartSpecQuantity quantityMap = transform r1 quantityMap
  where
  toList :: forall k v. Map k v -> List (Tuple k v)
  toList = Map.toUnfoldable

  transform :: forall k v a. (Tuple k v -> a) -> Map k v -> Array a
  transform f = A.fromFoldable <<< map f <<< toList

  r1 :: Tuple ChargeUnitId (Either Quantity (Map DimValue Quantity)) -> EstimatedUsage
  r1 (Tuple unitId unitMap) = case unitMap of
    Left usage -> EstimatedUsagePerUnit { unit: unitId, usage }
    Right dimMap -> EstimatedUsageByDimPerUnit { unit: unitId, usageByDim: transform r2 dimMap }

  r2 :: Tuple DimValue Quantity -> EstimatedUsagePerDim
  r2 (Tuple dim usage) = EstimatedUsagePerDim { dim, usage }
