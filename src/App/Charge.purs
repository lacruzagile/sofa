-- | A component that presents a view of product charges.
module App.Charge (Slot, Output, proxy, component) where

import Prelude
import App.EditableSegmentPrice as EditableSegmentPrice
import Css as Css
import Data.Array (mapWithIndex)
import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec as SS
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Widgets as Widgets

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "charge"
proxy = Proxy

type Slots
  = ( editableSegmentPrice :: EditableSegmentPrice.Slot PriceIndex )

type PriceIndex
  = { chargeIdx :: Int, dimIdx :: Int, unitIdx :: Int, priceIdx :: Int }

type Input
  = State

type Output
  = SS.Charge

type State
  = { unitMap :: SS.ChargeUnitMap
    , charge :: SS.Charge
    }

data Action
  = SetCustomPrice PriceIndex SS.PricePerSegment

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState = identity

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { unitMap, charge } = case charge of
  SS.ChargeArray rs -> HH.ol_ <<< A.mapWithIndex (\i r -> HH.li_ [ renderChargeElement i r ]) $ rs
  where
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

  renderPriceByUnitPerDim ::
    Int ->
    Array SS.ChargeUnitRef ->
    Array SS.PriceByUnitPerDim ->
    H.ComponentHTML Action Slots m
  renderPriceByUnitPerDim chargeIdx unitRefs ppd =
    HH.table_
      $ [ HH.tr_
            $ (if A.null dims then [] else [ HH.th [ HP.colSpan $ A.length dims ] [ HH.text "Dimension" ] ])
            <> [ HH.th [ HP.colSpan $ A.length units ] [ HH.text "Unit" ]
              , HH.th_ [ HH.text "Monthly Minimum" ]
              ]
        , HH.tr_ $ map (HH.th_ <<< A.singleton) $ (HH.text <$> dims) <> unitLabels <> [ HH.text "" ]
        ]
      <> mapWithIndex priceRow ppd
    where
    priceRow dimIdx (SS.PriceByUnitPerDim p) =
      HH.tr_
        $ map (HH.td_ <<< A.singleton)
        $ (if A.null dims then [] else HH.text <$> dimVals p.dim)
        <> A.mapWithIndex (priceVal dimIdx) p.prices
        <> [ HH.text $ show p.monthlyMinimum ]

    units = A.mapMaybe lookupUnit unitRefs

    lookupUnit (SS.ChargeUnitRef u) = Map.lookup u.unitID unitMap

    unitLabels = mkLabel <$> units
      where
      mkLabel u =
        Widgets.withTooltip Widgets.Top (show $ _.chargeType $ unwrap u)
          $ HH.text
          $ SS.chargeUnitLabel u

    -- Fatalistically assume that there is at least one unit defined, then
    -- fatalistically assume that all units use the same dimensions.
    dims = case A.head units of
      Just (SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) }) -> A.fromFoldable $ Map.keys o.properties
      Just (SS.ChargeUnit { priceDimSchema: Just _ }) -> [ "" ]
      _ -> []

    dimVals :: SS.DimValue -> Array String
    dimVals = case _ of
      SS.DimValue (SS.CvObject m) -> (\d -> maybe "N/A" show $ Map.lookup d m) <$> dims
      SS.DimValue v -> [ show v ]

    priceVal :: Int -> Int -> SS.PriceByUnit -> H.ComponentHTML Action Slots m
    priceVal dimIdx unitIdx (SS.PriceByUnit { price: (SS.Price p) }) = HH.ul_ $ A.mapWithIndex priceValEntry p
      where
      idx priceIdx = { chargeIdx, dimIdx, unitIdx, priceIdx }

      priceValEntry priceIdx = HH.li_ <<< A.singleton <<< renderEditablePriceSegment (idx priceIdx)

  renderEditablePriceSegment :: PriceIndex -> SS.PricePerSegment -> H.ComponentHTML Action Slots m
  renderEditablePriceSegment priceIdx price =
    HH.slot EditableSegmentPrice.proxy priceIdx EditableSegmentPrice.component price
      $ SetCustomPrice priceIdx

  renderPriceSegmentationsByUnit :: Array SS.PriceSegmentationPerUnit -> H.ComponentHTML Action Slots m
  renderPriceSegmentationsByUnit = HH.ul_ <<< map (HH.li_ <<< A.singleton <<< renderPriceSegmentationPerUnit)

  renderPriceSegmentationPerUnit :: SS.PriceSegmentationPerUnit -> H.ComponentHTML Action Slots m
  renderPriceSegmentationPerUnit (SS.PriceSegmentationPerUnit p) =
    HH.dl_
      $ renderDataItemString "Unit" (showChargeUnitRef p.unit)
      <> renderDataItem "Segmentation" (renderSegmentation p.segmentation)

  renderSegmentation :: SS.PriceSegmentation -> H.ComponentHTML Action Slots m
  renderSegmentation (SS.PriceSegmentation p) =
    HH.dl_
      $ renderDataItemString "Segment Unit" (showChargeUnitRef p.unit)
      <> renderDataItemString "Period" (show p.period)
      <> renderDataItemString "Model" (show p.model)
      <> renderDataItem "Segments" (HH.ul_ $ renderSegment <$> p.segments)
    where
    renderSegment s = HH.li_ [ HH.text $ showSegment s ]

  renderDefaultPrices :: Array SS.DefaultPricePerUnit -> H.ComponentHTML Action Slots m
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

  renderChargeElement :: Int -> SS.ChargeElement -> H.ComponentHTML Action Slots m
  renderChargeElement chargeIdx (SS.ChargeElement c) =
    HH.section [ HP.class_ Css.charge ]
      [ renderPriceByUnitPerDim chargeIdx c.units c.priceByUnitByDim
      , HH.dl_
          $ optArr (renderDataItem "Price Segmentations" <<< renderPriceSegmentationsByUnit) c.segmentationByUnit
          <> optArr (renderDataItem "Default Prices" <<< renderDefaultPrices) c.defaultPriceByUnit
          <> renderDataItemString "Monthly Minimum" (show c.monthlyMinimum)
          <> renderDataItemString "Term of Price Change" (show c.termOfPriceChangeInDays <> " days")
      ]

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

showSegment :: SS.Segment -> String
showSegment (SS.Segment s) = "[" <> show s.minimum <> "," <> maybe "" show s.exclusiveMaximum <> ")"

handleAction :: forall m. Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  SetCustomPrice { chargeIdx, dimIdx, unitIdx, priceIdx } pps ->
    let
      updatePrice p@(SS.Price p') = fromMaybe p $ SS.Price <$> A.modifyAt priceIdx (\_ -> pps) p'

      updatePriceByUnit (SS.PriceByUnit p) = SS.PriceByUnit $ p { price = updatePrice p.price }

      updatePriceByUnitPerDim p@(SS.PriceByUnitPerDim p') =
        fromMaybe p
          $ do
              prices <- A.modifyAt unitIdx updatePriceByUnit p'.prices
              pure $ SS.PriceByUnitPerDim p' { prices = prices }

      updateChargeElement c@(SS.ChargeElement c') =
        fromMaybe c
          $ do
              priceByUnitByDim <- A.modifyAt dimIdx updatePriceByUnitPerDim c'.priceByUnitByDim
              pure $ SS.ChargeElement $ c' { priceByUnitByDim = priceByUnitByDim }
    in
      do
        st' <-
          H.modify \st ->
            st
              { charge =
                case st.charge of
                  SS.ChargeArray cs ->
                    fromMaybe st.charge
                      $ SS.ChargeArray
                      <$> A.modifyAt chargeIdx updateChargeElement cs
              }
        H.raise st'.charge
