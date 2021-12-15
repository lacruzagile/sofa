-- A few helper functions to make it easier to work with Smart Spec charges and
-- charge units.
module Data.Charge
  ( ChargeUnitMap
  , chargeUnitLabel
  , description
  , dims
  , lookupChargeKind
  , productChargeUnitMap
  , unitIds
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.SmartSpec (Charge(..), ChargeSingleUnit(..), ChargeKind, ChargeUnit(..), ChargeUnitId(..), DimValue, PricePerDim(..), PricePerDimSeg(..), PricePerDimUnit(..), PricePerDimUnitOptSeg(..), PricePerDimUnitSeg(..), Product(..))
import Data.Tuple (Tuple(..))

type ChargeUnitMap
  = Map ChargeUnitId ChargeUnit

-- | Produces a map from unit IDs to the charge unit itself.
productChargeUnitMap :: Product -> ChargeUnitMap
productChargeUnitMap (Product { chargeUnits }) =
  Map.fromFoldable $ map (\u@(ChargeUnit { id }) -> Tuple id u)
    $ chargeUnits

-- | A suitable label for a unit. Uses the unit title, if available, otherwise
-- | its identifier.
chargeUnitLabel :: ChargeUnit -> String
chargeUnitLabel (ChargeUnit { id: ChargeUnitId id, title }) = fromMaybe id title

dims :: Charge -> Set DimValue
dims = case _ of
  ChargeSingleUnit c -> dimsSingleUnit c
  ChargeList { charges } -> Set.unions $ dimsSingleUnit <$> charges
  ChargeDimUnitOptSeg { priceByUnitByDim } ->
    let
      dimOf = case _ of
        PricePerDimUnitOptSeg (PricePerDimUnitSeg p) -> p.dim
        PricePerDimUnitOptNoSeg (PricePerDimUnit p) -> p.dim
    in
      Set.fromFoldable $ dimOf <$> priceByUnitByDim
  where
  dimsSingleUnit = case _ of
    ChargeSimple _ -> Set.empty
    ChargeDim { priceByDim } -> Set.fromFoldable $ (\(PricePerDim p) -> p.dim) <$> priceByDim
    ChargeSeg _ -> Set.empty
    ChargeDimSeg { priceBySegmentByDim } -> Set.fromFoldable $ (\(PricePerDimSeg p) -> p.dim) <$> priceBySegmentByDim

description :: Charge -> Maybe String
description = case _ of
  ChargeSingleUnit c -> case c of
    ChargeSimple { description: d } -> d
    ChargeDim { description: d } -> d
    ChargeSeg { description: d } -> d
    ChargeDimSeg { description: d } -> d
  ChargeList { description: d } -> d
  ChargeDimUnitOptSeg { description: d } -> d

-- | Finds the charge kind of the given identified charge unit.
lookupChargeKind :: ChargeUnitId -> ChargeUnitMap -> Maybe ChargeKind
lookupChargeKind unitId unitMap =
  (\(ChargeUnit u) -> u.kind)
    <$> Map.lookup unitId unitMap

-- Set of all charge unit IDs present within the given charge.
unitIds :: Charge -> Set ChargeUnitId
unitIds = case _ of
  ChargeSingleUnit c ->
    Set.singleton
      $ fromSingleUnit c
  ChargeList { charges } -> Set.fromFoldable $ fromSingleUnit <$> charges
  ChargeDimUnitOptSeg { units } -> Set.fromFoldable units
  where
  fromSingleUnit = case _ of
    ChargeSimple { unit } -> unit
    ChargeDim { unit } -> unit
    ChargeSeg { unit } -> unit
    ChargeDimSeg { unit } -> unit
