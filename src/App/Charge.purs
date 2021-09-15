-- | A component that presents a view of product charges.
module App.Charge (Slot, Output, proxy, component) where

import Prelude
import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec as SS
import Data.String (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot
  = forall query. H.Slot query Output String

proxy :: Proxy "charge"
proxy = Proxy

type Input
  = State

type Output
  = SS.Charge

type State
  = { unitMap :: SS.ChargeUnitMap
    , charge :: SS.Charge
    }

data Action
  = NoOp

component :: forall query m. H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState = identity

render :: forall slots m. State -> H.ComponentHTML Action slots m
render { unitMap, charge } = case charge of
  SS.ChargeSimple r ->
    HH.dl_ $ renderDataItem "Price" (renderSimplePrice r.unit r.price)
      <> opt (renderDataItem "Segmentation" <<< renderSegmentation) r.segmentation
      <> renderDataItemString "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
      <> renderDataItemString "Monthly Minimum" (show r.monthlyMinimum)
  SS.ChargeMixed r ->
    HH.dl_ $ optArr (renderDataItem "Price Segmentations" <<< renderPriceSegmentationsByUnit) r.segmentationByUnit
      <> optArr (renderDataItem "Default Prices" <<< renderDefaultPrices) r.defaultPriceByUnit
      <> renderDataItem "Prices per Dimension" (renderPriceByUnitPerDim r.units r.priceByUnitByDim)
      <> renderDataItemString "Monthly Minimum" (show r.monthlyMinimum)
      <> renderDataItemString "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
  SS.ChargeArray rs -> HH.ol_ <<< map (\r -> HH.li_ [ renderRateCardCharge r ]) $ rs
  where
  opt :: forall a b. (a -> Array b) -> Maybe a -> Array b
  opt = maybe []

  -- Apply the given function if array is non-empty, otherwise return empty array.
  optArr :: forall a b. (Array a -> Array b) -> Array a -> Array b
  optArr f = case _ of
    [] -> []
    xs -> f xs

  renderDataItem label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ value ]
    ]

  renderDataItemString label value = renderDataItem label $ HH.text value

  renderSimplePrice :: SS.ChargeUnitRef -> SS.SimplePrice -> H.ComponentHTML Action slots m
  renderSimplePrice unit = case _ of
    SS.SimplePriceSegmented p -> renderPriceByUnitPerDim (A.singleton unit) (conv' p)
    SS.SimplePriceByDim p -> renderPriceByUnitPerDim (A.singleton unit) (map conv p)
    where
    conv (SS.PriceByDim p) =
      SS.PriceByUnitPerDim
        { dim: p.dim
        , prices: A.singleton $ SS.PriceByUnit { price: p.price, unit }
        , monthlyMinimum: 0.0
        }

    conv' price =
      A.singleton
        $ SS.PriceByUnitPerDim
            { dim: SS.DimValue SS.CvNull
            , prices: A.singleton $ SS.PriceByUnit { unit, price }
            , monthlyMinimum: 0.0
            }

  renderPriceByUnitPerDim ::
    Array SS.ChargeUnitRef ->
    Array SS.PriceByUnitPerDim ->
    H.ComponentHTML Action slots m
  renderPriceByUnitPerDim unitRefs ppd =
    HH.table_
      $ [ HH.tr_
            [ HH.th [ HP.colSpan $ A.length dims ] [ HH.text "Dimension" ]
            , HH.th [ HP.colSpan $ A.length units ] [ HH.text "Unit" ]
            , HH.th_ [ HH.text "Monthly Minimum" ]
            ]
        , HH.tr_ $ map (HH.th_ <<< A.singleton <<< HH.text) $ dims <> unitLabels <> [ "" ]
        ]
      <> map priceRow ppd
    where
    priceRow (SS.PriceByUnitPerDim p) =
      HH.tr_
        $ map (HH.td_ <<< A.singleton <<< HH.text)
        $ dimVals p.dim
        <> priceVals p.prices
        <> [ show p.monthlyMinimum ]

    units = A.mapMaybe (\(SS.ChargeUnitRef u) -> Map.lookup u.unitID unitMap) unitRefs

    unitLabels = SS.chargeUnitLabel <$> units

    -- Fatalistically assume that there is at least one unit defined, then
    -- fatalistically assume that all units use the same dimensions.
    dims = case A.head units of
      Just (SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) }) -> A.fromFoldable $ Map.keys $ o.properties
      _ -> [ "" ]

    dimVals :: SS.DimValue -> Array String
    dimVals = case _ of
      SS.DimValue (SS.CvObject m) -> (\d -> maybe "N/A" show $ Map.lookup d m) <$> dims
      SS.DimValue v -> [ show v ]

    showSegmentedPrice :: SS.Price -> String
    showSegmentedPrice (SS.Price p) = joinWith ", " $ map showPricePerSegment p

    priceVal :: SS.PriceByUnit -> String
    priceVal (SS.PriceByUnit { price }) = showSegmentedPrice price

    priceVals :: Array SS.PriceByUnit -> Array String
    priceVals = map priceVal <<< A.sortBy (comparing (_.unitID <<< unwrap <<< _.unit <<< unwrap))

  renderPriceSegmentationsByUnit :: Array SS.PriceSegmentationPerUnit -> H.ComponentHTML Action slots m
  renderPriceSegmentationsByUnit = HH.ul_ <<< map (HH.li_ <<< A.singleton <<< renderPriceSegmentationPerUnit)

  renderPriceSegmentationPerUnit :: SS.PriceSegmentationPerUnit -> H.ComponentHTML Action slots m
  renderPriceSegmentationPerUnit (SS.PriceSegmentationPerUnit p) =
    HH.dl_
      $ renderDataItemString "Unit" (showChargeUnitRef p.unit)
      <> renderDataItem "Segmentation" (renderSegmentation p.segmentation)

  renderSegmentation :: SS.PriceSegmentation -> H.ComponentHTML Action slots m
  renderSegmentation (SS.PriceSegmentation p) =
    HH.dl_
      $ renderDataItemString "Segment Unit" (showChargeUnitRef p.unit)
      <> renderDataItemString "Period" (show p.period)
      <> renderDataItemString "Model" (show p.model)
      <> renderDataItem "Segments" (HH.ul_ $ segment <$> p.segments)
    where
    segment s = HH.li_ [ HH.text $ showSegment s ]

  renderDefaultPrices :: Array SS.DefaultPricePerUnit -> H.ComponentHTML Action slots m
  renderDefaultPrices ps =
    HH.table_
      $ [ HH.tr_
            [ HH.th_ [ HH.text "Unit" ]
            , HH.th_ [ HH.text "Price" ]
            ]
        ]
      <> map mkRow ps
    where
    mkRow (SS.DefaultPricePerUnit p) =
      HH.tr_
        [ HH.td_ [ HH.text $ showChargeUnitRef p.unit ]
        , HH.td_ [ HH.text $ show p.price ]
        ]

  renderRateCardCharge :: SS.Charge -> H.ComponentHTML Action slots m
  renderRateCardCharge = case _ of
    SS.ChargeSimple r ->
      HH.dl_ $ renderDataItem "Price" (renderSimplePrice r.unit r.price)
        <> opt (renderDataItem "Segmentation" <<< renderSegmentation) r.segmentation
        <> renderDataItemString "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
        <> renderDataItemString "Monthly Minimum" (show r.monthlyMinimum)
    SS.ChargeMixed r ->
      HH.dl_ $ optArr (renderDataItem "Price Segmentations" <<< renderPriceSegmentationsByUnit) r.segmentationByUnit
        <> optArr (renderDataItem "Default Prices" <<< renderDefaultPrices) r.defaultPriceByUnit
        <> renderDataItem "Prices per Dimension" (renderPriceByUnitPerDim r.units r.priceByUnitByDim)
        <> renderDataItemString "Monthly Minimum" (show r.monthlyMinimum)
        <> renderDataItemString "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
    SS.ChargeArray rs -> HH.ol_ <<< map (\r -> HH.li_ [ renderRateCardCharge r ]) $ rs

showChargeUnitRef :: SS.ChargeUnitRef -> String
showChargeUnitRef (SS.ChargeUnitRef unit) =
  unit.unitID
    <> (maybe "" (\p -> " [" <> showProductRef p <> "]") unit.product)

showProductRef :: SS.ProductRef -> String
showProductRef (SS.ProductRef p) = showSkuCode p.sku <> (maybe "" (\s -> " [" <> show s <> "]") p.solutionURI)

showSkuCode :: SS.Sku -> String
showSkuCode = case _ of
  SS.SkuCode c -> c
  SS.Sku s -> s.code

showPricePerSegment :: SS.PricePerSegment -> String
showPricePerSegment (SS.PricePerSegment p) = case p.discount of
  Nothing -> noDiscount
  Just d -> discount d
  where
  noDiscount =
    show p.listPrice
      <> " ["
      <> show p.minimum
      <> ","
      <> maybe "" show p.exclusiveMaximum
      <> ")"

  discount d = noDiscount <> " (discount " <> showDiscount d <> ")"

  showDiscount = case _ of
    SS.DiscountPercentage d -> show d <> "%"
    SS.DiscountAbsolute d -> show d

showSegment :: SS.Segment -> String
showSegment (SS.Segment s) = "[" <> show s.minimum <> "," <> maybe "" show s.exclusiveMaximum <> ")"

handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  NoOp -> pure unit
