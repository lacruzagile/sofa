module Data.SubTotal
  ( SubTotal(..)
  , SubTotalEntry(..)
  , calcSubTotal
  , calcSubTotalEntry
  , renderSubTotalEntry
  ) where

import Prelude
import Data.Array as A
import Data.Estimate (Estimate(..))
import Data.Estimate as Est
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Number.Format (toStringWith, fixed)
import Data.Ord (max)
import Data.SmartSpec (ChargeType(..), Currency(..), Price(..), PricePerSegment(..), SegmentationModel(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Widgets as Widgets

type SubTotalEntry
  = { listPrice :: Estimate (Additive Number)
    , salesPrice :: Estimate (Additive Number)
    }

-- | A sub-total price.
newtype SubTotal
  = SubTotal
  { onetime :: SubTotalEntry -- ^ Onetime price.
  , monthly :: SubTotalEntry -- ^ Monthly price.
  , quarterly :: SubTotalEntry -- ^ Quarterly price.
  , usage :: SubTotalEntry -- ^ Usage price.
  , segment :: SubTotalEntry -- ^ Monthly segment price.
  , quarterlySegment :: SubTotalEntry -- ^ Quarterly segment price.
  }

derive newtype instance semigroupSubTotal :: Semigroup SubTotal

derive newtype instance monoidSubTotal :: Monoid SubTotal

-- | Calculates the sub-total entry using the given information.
calcSubTotal :: Estimate Int -> SegmentationModel -> ChargeType -> Price -> SubTotal
calcSubTotal quantity segmentationModel chargeType =
  mkSubTotal chargeType
    <<< calcSubTotalEntry quantity segmentationModel

-- Create a `SubTotal` value for the given entry using the given charge type.
mkSubTotal :: ChargeType -> SubTotalEntry -> SubTotal
mkSubTotal chargeType entry = case chargeType of
  ChargeTypeOnetime -> SubTotal $ zero { onetime = entry }
  ChargeTypeMonthly -> SubTotal $ zero { monthly = entry }
  ChargeTypeQuarterly -> SubTotal $ zero { quarterly = entry }
  ChargeTypeUsage -> SubTotal $ zero { usage = entry }
  ChargeTypeSegment -> SubTotal $ zero { segment = entry }
  ChargeTypeQuarterlySegment -> SubTotal $ zero { quarterlySegment = entry }
  where
  SubTotal zero = mempty

-- | Calculates the sub-total entry for the given price. If a quantity and/or
-- | segmentation model is provided then the pricing is calculated accordingly.
calcSubTotalEntry :: Estimate Int -> SegmentationModel -> Price -> SubTotalEntry
calcSubTotalEntry quantity segmentationModel price = case segmentationModel of
  SegmentationModelTiered -> calcTieredEntry quantity price
  SegmentationModelVolume -> calcVolumeEntry quantity price
  _ -> calcTieredEntry quantity price

calcVolumeEntry :: Estimate Int -> Price -> SubTotalEntry
calcVolumeEntry quantity (Price pricePerSegment) = maybe mempty calc $ A.find inSegment pricePerSegment
  where
  q = Est.toValue quantity

  inSegment (PricePerSegment pps) = q >= pps.minimum && maybe true (q < _) pps.exclusiveMaximum

  calc :: PricePerSegment -> SubTotalEntry
  calc (PricePerSegment pps) =
    let
      { listPrice, salesPrice } = scaleEntryBy pps q
    in
      { listPrice: const listPrice <$> quantity
      , salesPrice: const salesPrice <$> quantity
      }

-- | Calculates a sub-total entry using the "tiered" segmentation model.
calcTieredEntry :: Estimate Int -> Price -> SubTotalEntry
calcTieredEntry quantity (Price pricePerSegment) =
  ( \{ listPrice, salesPrice } ->
      { listPrice: const listPrice <$> quantity
      , salesPrice: const salesPrice <$> quantity
      }
  )
    $ A.foldl accTot mempty
    $ A.takeWhile ge pricePerSegment
  where
  q = Est.toValue quantity

  ge (PricePerSegment pps) = q >= pps.minimum

  accTot acc (PricePerSegment pps) = case pps.exclusiveMaximum of
    Just exmax
      | exmax <= q ->
        let
          segmentSize = (fromMaybe 0 pps.exclusiveMaximum) - (max 1 pps.minimum)
        in
          acc <> scaleEntryBy pps segmentSize
    _ ->
      let
        qInSegment = if pps.minimum == 0 then q else q - pps.minimum + 1
      in
        acc <> scaleEntryBy pps qInSegment

scaleEntryBy ::
  forall r.
  { listPrice :: Number, salesPrice :: Number | r } ->
  Int ->
  { listPrice :: Additive Number, salesPrice :: Additive Number }
scaleEntryBy pps quantity =
  let
    q = Int.toNumber quantity
  in
    { listPrice: Additive $ q * pps.listPrice
    , salesPrice: Additive $ q * pps.salesPrice
    }

-- | Render a sub-total entry.
renderSubTotalEntry ::
  forall action slots m.
  Monad m =>
  Maybe Currency ->
  SubTotalEntry ->
  H.ComponentHTML action slots m
renderSubTotalEntry currency amount = case currency of
  Nothing -> HH.text "N/A"
  Just (Currency "") -> HH.text "N/A"
  Just (Currency code) ->
    if amount.listPrice == amount.salesPrice then
      HH.text $ showMonetary amount.listPrice <> " " <> code
    else
      Widgets.withTooltip Widgets.Top ("Without discounts: " <> showMonetary amount.listPrice)
        $ HH.span_
            [ HH.span [ HP.style "color:red" ] [ HH.text $ showMonetary amount.salesPrice ]
            , HH.text " "
            , HH.text code
            ]

showMonetary :: Estimate (Additive Number) -> String
showMonetary = showEst <<< map (\(Additive n) -> toStringWith (fixed 2) n)
  where
  showEst = case _ of
    Exact s -> s
    Estimate s -> "~" <> s
