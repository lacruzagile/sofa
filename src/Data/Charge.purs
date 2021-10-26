-- A few helper functions to make it easier to work with Smart Spec charges and charge units.
module Data.Charge
  ( ChargeUnitMap
  , chargeUnitLabel
  , description
  , dims
  , lookupChargeType
  , periodMinimum
  , productChargeUnitMap
  , unitIDs
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.SmartSpec (Charge(..), ChargeSingleUnit(..), ChargeType, ChargeUnit(..), ChargeUnitID(..), DimValue, PricePerDim(..), PricePerDimSeg(..), PricePerDimUnit(..), PricePerDimUnitOptSeg(..), PricePerDimUnitSeg(..), Product(..))
import Data.Tuple (Tuple(..))

type ChargeUnitMap
  = Map ChargeUnitID ChargeUnit

-- | Produces a map from unit IDs to the charge unit itself.
productChargeUnitMap :: Product -> ChargeUnitMap
productChargeUnitMap (Product { chargeUnits }) =
  Map.fromFoldable $ map (\u@(ChargeUnit { id }) -> Tuple id u)
    $ chargeUnits

-- | A suitable label for a unit. Uses the unit name, if available, otherwise
-- | its identifier.
chargeUnitLabel :: ChargeUnit -> String
chargeUnitLabel (ChargeUnit { id: ChargeUnitID id, name }) = fromMaybe id name

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

periodMinimum :: Charge -> Maybe Number
periodMinimum = case _ of
  ChargeSingleUnit c -> case c of
    ChargeSimple { periodMinimum: n } -> n
    ChargeDim { periodMinimum: n } -> n
    ChargeSeg { periodMinimum: n } -> n
    ChargeDimSeg { periodMinimum: n } -> n
  ChargeList { periodMinimum: n } -> n
  ChargeDimUnitOptSeg { periodMinimum: n } -> n

-- | Finds the charge type of the given identified charge unit.
lookupChargeType :: ChargeUnitID -> ChargeUnitMap -> Maybe ChargeType
lookupChargeType unitID unitMap =
  (\(ChargeUnit u) -> u.chargeType)
    <$> Map.lookup unitID unitMap

-- Set of all charge unit IDs present within the given charge.
unitIDs :: Charge -> Set ChargeUnitID
unitIDs = case _ of
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
