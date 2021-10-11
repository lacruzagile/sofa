-- | A component that presents a view of product charges and quantity.
module App.Charge (Slot, Output, QuantityMap, proxy, component) where

import Prelude
import App.EditableQuantity as EditableQuantity
import App.EditableSegmentPrice as EditableSegmentPrice
import Control.Alternative (guard)
import Css as Css
import Data.Array (mapWithIndex)
import Data.Array as A
import Data.Either (Either(..))
import Data.Estimate (Estimate(..))
import Data.Estimate as Est
import Data.Foldable (fold, traverse_)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Number.Format (fixed, toStringWith)
import Data.SmartSpec (DimValue)
import Data.SmartSpec as SS
import Data.Tuple (Tuple(..))
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
  = ( editableSegmentPrice :: EditableSegmentPrice.Slot PriceIndex
    , editableQuantity :: EditableQuantity.Slot QuantityIndex
    )

type PriceIndex
  = { chargeIdx :: Int, dimIdx :: Int, unitIdx :: Int, priceIdx :: Int }

type QuantityIndex
  = { unitRef :: SS.ChargeUnitRef, dim :: Maybe SS.DimValue }

type Input
  = { unitMap :: SS.ChargeUnitMap
    , charge :: SS.Charge
    , quantity :: QuantityMap
    }

type Output
  = { charge :: SS.Charge, quantity :: QuantityMap }

type State
  = { unitMap :: SS.ChargeUnitMap
    , charge :: SS.Charge
    , quantity :: QuantityMap
    , aggregatedQuantity :: AggregatedQuantityMap
    }

-- | The quantity is either set as an aggregate or individually for each of the
-- | unit's dimensions.
type QuantityMap
  = Map SS.ChargeUnitRef (Either (Estimate Int) (Map SS.DimValue (Estimate Int)))

-- | The aggregated quantity has one quantity per unit. Note, if per-dimension
-- | quantities are used and a dimension quantity is missing then the aggregated
-- | value is `Nothing`.
type AggregatedQuantityMap
  = Map SS.ChargeUnitRef AggregatedQuantity

data QuantityType a
  = QtWAP a
  | QtSum a

qtToValue :: forall a. QuantityType a -> a
qtToValue = case _ of
  QtWAP q -> q
  QtSum q -> q

type AggregatedQuantity
  = Estimate (QuantityType Int)

data Action
  = SetCustomPrice PriceIndex SS.PricePerSegment
  | SetQuantity QuantityIndex (Maybe (Estimate Int))

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState input =
  { unitMap: input.unitMap
  , charge: input.charge
  , quantity: input.quantity
  , aggregatedQuantity: aggregateQuantity input.quantity
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { unitMap, charge, quantity, aggregatedQuantity } = case charge of
  SS.ChargeArray rs ->
    HH.ul [ HP.class_ Css.blocklist ] <<< A.mapWithIndex (\i r -> HH.li_ [ renderChargeElement i r ])
      $ rs
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
    Array SS.EstimatedWAPPerUnit ->
    Array SS.PriceByUnitPerDim ->
    H.ComponentHTML Action Slots m
  renderPriceByUnitPerDim chargeIdx unitRefs wapPerUnit ppd =
    HH.table_
      $ [ HH.tr_
            $ fillDims true [ HH.text "Dimension" ]
            <> [ HH.th [ HP.colSpan $ A.length units ] [ HH.text "Unit" ]
              , HH.th_ [ HH.text "Monthly Minimum" ]
              ]
        , HH.tr_ $ map (HH.th_ <<< A.singleton) $ (HH.text <$> dims) <> unitLabels <> [ HH.text "" ]
        ]
      <> mapWithIndex priceRow ppd
      <> totalQuantityRow (A.head ppd)
    where
    fillDims header els
      | A.null dims = []
      | header = [ HH.th [ HP.colSpan $ A.length dims ] els ]
      | otherwise = [ HH.td [ HP.colSpan $ A.length dims ] els ]

    priceRow dimIdx (SS.PriceByUnitPerDim p) =
      HH.tr_
        $ map (HH.td_ <<< A.singleton)
        $ (if A.null dims then [] else HH.text <$> dimVals p.dim)
        <> A.mapWithIndex (priceQuantityVal p.dim dimIdx) p.prices
        <> [ HH.text $ show p.periodMinimum ]

    totalQuantityRow Nothing = []

    totalQuantityRow (Just (SS.PriceByUnitPerDim p)) =
      [ HH.tr [ HP.style "border-top: 1px solid black" ]
          $ fillDims false [ HH.text "" ]
          <> map (HH.td_ <<< A.singleton <<< footerQuantityVal) p.prices
          <> [ HH.td_ [ HH.text "" ] ]
      ]

    footerQuantityVal (SS.PricePerUnit { unit: unitRef }) = case Map.lookup unitRef aggregatedQuantity of
      Nothing -> HH.text ""
      Just q ->
        HH.ul [ HP.class_ Css.priceList ]
          $ let
              qvalue = qtToValue <$> q

              renderQuantity = renderEditableQuantity { unitRef, dim: Nothing } (Just $ qvalue)
            in
              case Est.toValue q of
                QtWAP _ ->
                  [ HH.li_ [ HH.text "WAP quantity: ", renderQuantity ] ]
                    <> renderWAP unitRef qvalue
                QtSum _ ->
                  [ HH.li_ [ HH.text "Total quantity: ", renderQuantity ] ]
                    <> renderTotalPrice unitRef

    renderWAP unitRef q =
      maybe [] renderWAP'
        $ A.find (\(SS.EstimatedWAPPerUnit v) -> v.unit == unitRef) wapPerUnit
      where
      renderWAP' (SS.EstimatedWAPPerUnit { wap: SS.EstimatedWAP wap }) =
        [ HH.li_ [ HH.text "WAP: ", HH.text $ showMonetary $ calcWAP q wap ]
        ]

    renderTotalPrice unitRef = renderTotalPrice' $ calc $ Map.lookup unitRef quantity
      where
      calc Nothing = Exact 0.0

      calc (Just (Left _)) = Exact 0.0

      calc (Just (Right dimMap)) =
        map (\(Additive n) -> n) $ fold
          $ do
              let
                SS.ChargeArray c = charge
              SS.ChargeElement ce <- List.fromFoldable c
              SS.PriceByUnitPerDim pbupd <- List.fromFoldable ce.priceByUnitByDim
              SS.PricePerUnit p <- List.fromFoldable pbupd.prices
              guard $ p.unit == unitRef
              case Map.lookup pbupd.dim dimMap of
                Nothing -> mempty
                Just q -> pure $ calcTotal q p.price

      renderTotalPrice' tot =
        [ HH.li_ [ HH.text "Total price: ", HH.text $ showMonetary tot ]
        ]

    calcTotal :: Estimate Int -> SS.Price -> Estimate (Additive Number)
    calcTotal quant (SS.Price pricePerSegment) =
      (\tot -> (Additive <<< const tot) <$> quant)
        $ A.foldl accTot 0.0
        $ A.takeWhile ge pricePerSegment
      where
      q = Est.toValue quant

      toPrice pps = fromMaybe pps.listPrice pps.salesPrice

      accTot :: Number -> SS.PricePerSegment -> Number
      accTot acc (SS.PricePerSegment pps) = case pps.exclusiveMaximum of
        Just exmax
          | exmax <= q ->
            let
              segmentSize = (fromMaybe 0 pps.exclusiveMaximum - 1) - pps.minimum
            in
              acc + Int.toNumber segmentSize * toPrice pps
        _ ->
          let
            qInSegment = if pps.minimum == 0 then q else q - pps.minimum + 1
          in
            acc + Int.toNumber qInSegment * toPrice pps

      ge (SS.PricePerSegment pps) = Est.toValue quant >= pps.minimum

    showMonetary :: Estimate Number -> String
    showMonetary = showEst <<< map (\n -> toStringWith (fixed 3) n)
      where
      showEst = case _ of
        Exact s -> s
        Estimate s -> "~" <> s

    calcWAP quant =
      (\wap -> const wap <$> quant)
        <<< A.foldl accWAP 0.0
        <<< A.takeWhile ge
      where
      q :: Int
      q = Est.toValue quant

      accWAP :: Number -> SS.EstimatedWAPPerSegment -> Number
      accWAP acc (SS.EstimatedWAPPerSegment wap) = case wap.exclusiveMaximum of
        Just exmax
          | exmax <= q ->
            let
              segmentSize = (fromMaybe 0 wap.exclusiveMaximum - 1) - wap.minimum
            in
              acc + Int.toNumber segmentSize * wap.price
        _ ->
          let
            qInSegment = if wap.minimum == 0 then q else q - wap.minimum + 1
          in
            acc + Int.toNumber qInSegment * wap.price

      -- | Whether the given quantity is above or in the given segment.
      ge (SS.EstimatedWAPPerSegment wap) = q >= wap.minimum

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

    findDimQuantity :: SS.ChargeUnitRef -> SS.DimValue -> Maybe (Estimate Int)
    findDimQuantity unitRef dim = do
      q <- Map.lookup unitRef quantity
      case q of
        Left _ -> Nothing
        Right dimMap -> Map.lookup dim dimMap

    priceQuantityVal :: SS.DimValue -> Int -> Int -> SS.PricePerUnit -> H.ComponentHTML Action Slots m
    priceQuantityVal dim dimIdx unitIdx pbu =
      HH.ul [ HP.class_ Css.priceList ]
        $ [ HH.li [ HP.style "border-bottom:1px solid darkgray" ]
              [ HH.text "Quantity: "
              , renderEditableQuantity { unitRef, dim: Just dim } (findDimQuantity unitRef dim)
              ]
          ]
        <> A.mapWithIndex priceQuantityValEntry ps
      where
      -- TODO: Use currency.
      SS.PricePerUnit { unit: unitRef, price: (SS.Price ps) } = pbu

      idx priceIdx = { chargeIdx, dimIdx, unitIdx, priceIdx }

      priceQuantityValEntry priceIdx price = HH.li_ [ renderEditablePriceSegment (idx priceIdx) price ]

  renderEditablePriceSegment :: PriceIndex -> SS.PricePerSegment -> H.ComponentHTML Action Slots m
  renderEditablePriceSegment priceIdx price =
    HH.slot EditableSegmentPrice.proxy priceIdx EditableSegmentPrice.component price
      $ SetCustomPrice priceIdx

  renderEditableQuantity :: QuantityIndex -> Maybe (Estimate Int) -> H.ComponentHTML Action Slots m
  renderEditableQuantity quantityIdx qty =
    HH.slot EditableQuantity.proxy quantityIdx EditableQuantity.component qty
      $ SetQuantity quantityIdx

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
      [ renderPriceByUnitPerDim chargeIdx c.units c.estimatedWAPByUnit c.priceByUnitByDim
      , HH.dl_
          $ optArr (renderDataItem "Price Segmentations" <<< renderPriceSegmentationsByUnit) c.segmentationByUnit
          <> optArr (renderDataItem "Default Prices" <<< renderDefaultPrices) c.defaultPriceByUnit
          <> renderDataItemString "Period Minimum" (show c.periodMinimum)
          <> renderDataItemString "Term of Price Change" (show c.termOfPriceChangeInDays <> " days")
      ]

showChargeUnitRef :: SS.ChargeUnitRef -> String
showChargeUnitRef (SS.ChargeUnitRef unit) =
  unit.unitID
    <> (maybe "" (\p -> " [" <> showProductRef p <> "]") unit.product)

showProductRef :: SS.ProductRef -> String
showProductRef (SS.ProductRef p) = show p.sku <> (maybe "" (\s -> " [" <> show s <> "]") p.solutionURI)

showSegment :: SS.Segment -> String
showSegment (SS.Segment s) = "[" <> show s.minimum <> "," <> maybe "" show s.exclusiveMaximum <> ")"

aggregateQuantity :: QuantityMap -> AggregatedQuantityMap
aggregateQuantity quantityMap =
  Map.fromFoldable
    $ do
        Tuple unitRef dimMap <- Map.toUnfoldable quantityMap :: List _
        let
          q = case dimMap of
            Left q' -> QtWAP <$> q'
            Right m ->
              map (\(Additive n) -> QtSum n)
                $ List.foldl (\a b -> a <> map Additive b) mempty
                $ Map.values m
        pure $ Tuple unitRef q

handleAction :: forall m. Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  SetCustomPrice { chargeIdx, dimIdx, unitIdx, priceIdx } pps ->
    let
      updatePrice p@(SS.Price p') = fromMaybe p $ SS.Price <$> A.modifyAt priceIdx (\_ -> pps) p'

      updatePriceByUnit (SS.PricePerUnit p) = SS.PricePerUnit $ p { price = updatePrice p.price }

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
        H.raise { charge: st'.charge, quantity: st'.quantity }
  SetQuantity _ Nothing -> pure unit
  SetQuantity { dim, unitRef } (Just q) -> do
    st' <-
      H.modify \st ->
        let
          quantity' =
            let
              updateUnit = Map.update updateDim unitRef

              updateDim dimMap = case dim of
                Nothing -> Just $ Left q
                Just dim' -> case dimMap of
                  Left _ -> Just $ Right $ Map.singleton dim' q
                  Right m -> Just $ Right $ Map.insert dim' q m
            in
              updateUnit st.quantity
        in
          st
            { quantity = quantity'
            , aggregatedQuantity = aggregateQuantity quantity'
            }
    -- If we are setting a dimension specific quantity then override the value
    -- of the aggregate unit quantity component.
    when (isJust dim)
      $ let
          q' = map (map qtToValue) $ Map.lookup unitRef st'.aggregatedQuantity
        in
          H.tell EditableQuantity.proxy { unitRef, dim: Nothing } (EditableQuantity.SetQuantity q')
    when (isNothing dim)
      $ traverse_ (\dim' -> H.tell EditableQuantity.proxy { unitRef, dim: Just dim' } (EditableQuantity.SetQuantity Nothing)) (unitDims unitRef st'.charge)
    H.raise { charge: st'.charge, quantity: st'.quantity }

unitDims :: SS.ChargeUnitRef -> SS.Charge -> List DimValue
unitDims unitRef (SS.ChargeArray ces) = unravelled
  where
  unravelled = do
    SS.ChargeElement ce <- List.fromFoldable ces
    SS.PriceByUnitPerDim { dim, prices } <- List.fromFoldable ce.priceByUnitByDim
    SS.PricePerUnit { unit: unitRef' } <- List.fromFoldable prices
    if unitRef == unitRef' then mempty else pure dim