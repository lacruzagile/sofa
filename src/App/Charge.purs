-- | A component that presents a view of product charges and quantity.
module App.Charge (Slot, Output, proxy, component) where

import Prelude
import App.EditablePrice as EditablePrice
import App.EditableQuantity as EditableQuantity
import Css as Css
import Data.Array (mapWithIndex)
import Data.Array as A
import Data.Charge as Charge
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (findMapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Quantity (QuantityMap, Quantity)
import Data.Set as Set
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
  = ( editablePrice :: EditablePrice.Slot PriceIndex
    , editableQuantity :: EditableQuantity.Slot QuantityIndex
    )

type PriceIndex
  = { chargeIdx :: Int, subChargeIdx :: Int, dimIdx :: Int, unitIdx :: Int, segIdx :: Int }

type QuantityIndex
  = { unitId :: SS.ChargeUnitId, dim :: Maybe SS.DimValue }

type Input
  = { unitMap :: Charge.ChargeUnitMap
    , defaultCurrency :: SS.ChargeCurrency
    , charges :: Array SS.Charge
    , quantity :: Quantity -- ^ The product quantity.
    , estimatedUsage :: QuantityMap -- ^ The product usage.
    }

type Output
  = { charges :: Array SS.Charge, estimatedUsage :: QuantityMap }

type State
  = { unitMap :: Charge.ChargeUnitMap
    , defaultCurrency :: SS.ChargeCurrency
    , charges :: Array SS.Charge
    , quantity :: Quantity
    , estimatedUsage :: QuantityMap
    , aggregatedQuantity :: AggregatedQuantityMap
    }

-- -- | The quantity is either set as an aggregate or individually for each of the
-- -- | unit's dimensions.
-- type QuantityMap
--   = Map SS.ChargeUnitId (Either Quantity (Map SS.DimValue Quantity))
-- | The aggregated quantity has one quantity per unit. Note, if per-dimension
-- | quantities are used and a dimension quantity is missing then the aggregated
-- | value is `Nothing`.
type AggregatedQuantityMap
  = Map SS.ChargeUnitId AggregatedQuantity

data QuantityType a
  = QtWAP a
  | QtSum a

qtToValue :: forall a. QuantityType a -> a
qtToValue = case _ of
  QtWAP q -> q
  QtSum q -> q

type AggregatedQuantity
  = QuantityType Int

data Action
  = SetCustomPrice PriceIndex SS.Price
  | SetQuantity QuantityIndex (Maybe Int)

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
  , defaultCurrency: input.defaultCurrency
  , charges: input.charges
  , quantity: input.quantity
  , estimatedUsage: input.estimatedUsage
  , aggregatedQuantity: aggregateQuantity input.estimatedUsage
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { unitMap, defaultCurrency, charges, quantity, estimatedUsage, aggregatedQuantity } =
  HH.ul [ HP.class_ Css.blocklist ]
    $ A.mapWithIndex (\i r -> HH.li_ [ renderCharge i r ]) charges
  where
  opt :: forall a b. (a -> Array b) -> Maybe a -> Array b
  opt = maybe []

  renderDataItem label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ value ]
    ]

  renderDataItemString label value = renderDataItem label $ HH.text value

  renderEditablePrice :: PriceIndex -> SS.Price -> Maybe SS.ChargeCurrency -> H.ComponentHTML Action Slots m
  renderEditablePrice priceIdx price currency =
    HH.slot EditablePrice.proxy priceIdx EditablePrice.component input
      $ SetCustomPrice priceIdx
    where
    input :: EditablePrice.Input
    input = { price: price, currency: fromMaybe defaultCurrency currency }

  renderEditableQuantity :: QuantityIndex -> Maybe Quantity -> H.ComponentHTML Action Slots m
  renderEditableQuantity quantityIdx qty =
    HH.slot EditableQuantity.proxy quantityIdx EditableQuantity.component qty
      $ SetQuantity quantityIdx

  findDimQuantity :: SS.ChargeUnitId -> SS.DimValue -> Maybe Quantity
  findDimQuantity unitId dim = do
    q <- Map.lookup unitId estimatedUsage
    case q of
      Left q' -> pure q'
      Right dimMap -> Map.lookup dim dimMap

  renderEstimatedVolume :: QuantityIndex -> SS.DimValue -> Maybe (H.ComponentHTML Action Slots m)
  renderEstimatedVolume quantityIdx dim = do
    chargeKind <- Charge.lookupChargeKind quantityIdx.unitId unitMap
    if chargeKind /= SS.CkUsage then
      Nothing
    else
      let
        usage = findDimQuantity quantityIdx.unitId dim
      in
        Just $ renderEditableQuantity quantityIdx usage

  renderTotalEstimatedVolume :: SS.ChargeUnitId -> Maybe (H.ComponentHTML Action Slots m)
  renderTotalEstimatedVolume unitId = do
    chargeKind <- Charge.lookupChargeKind unitId unitMap
    if chargeKind /= SS.CkUsage then
      Nothing
    else
      Just $ HH.text $ fromMaybe "N/A"
        $ do
            q <- Map.lookup unitId aggregatedQuantity
            pure $ show $ qtToValue q

  renderDimVals :: Array String -> SS.DimValue -> Array (H.ComponentHTML Action Slots m)
  renderDimVals dimKeys (SS.DimValue v) = renderConfigValue dimKeys v

  renderConfigValue :: Array String -> SS.ConfigValue -> Array (H.ComponentHTML Action Slots m)
  renderConfigValue dimKeys = go
    where
    go = case _ of
      SS.CvObject m -> A.concat $ (\d -> maybe [ HH.text "N/A" ] go $ Map.lookup d m) <$> dimKeys
      SS.CvArray ms -> [ HH.ul [ HP.class_ Css.priceList ] $ (HH.li_ <<< go) <$> ms ]
      v -> [ HH.text $ show v ]

  renderSingleUnitCharge :: Int -> Int -> SS.ChargeSingleUnit -> Array (H.ComponentHTML Action Slots m)
  renderSingleUnitCharge chargeIdx subChargeIdx fullCharge = case fullCharge of
    SS.ChargeSimple c -> renderChargeSimple c
    SS.ChargeDim c -> renderChargeDim c
    SS.ChargeSeg c -> renderChargeSeg c
    SS.ChargeDimSeg _c -> [ HH.text "TODO" ]
    where
    unitId = case fullCharge of
      SS.ChargeSimple c -> c.unit
      SS.ChargeDim c -> c.unit
      SS.ChargeSeg c -> c.unit
      SS.ChargeDimSeg c -> c.unit

    nullDim = SS.DimValue SS.CvNull

    renderUnitHdr :: String -> SS.ChargeUnitId -> H.ComponentHTML Action Slots m
    renderUnitHdr kind u = HH.h4_ [ HH.text $ showChargeUnitRef u, HH.sup_ [ HH.text " (", HH.text kind, HH.text ")" ] ]

    renderChargeSimple charge =
      [ renderUnitHdr "ChargeSimple" charge.unit
      , HH.text "Price: "
      , renderEditablePrice
          { chargeIdx, subChargeIdx, dimIdx: 0, unitIdx: 0, segIdx: 0 }
          (SS.Price { price: charge.price, listPrice: charge.listPrice, discount: charge.discount })
          charge.currency
      ]
        <> maybe [] (\q -> [ HH.br_, HH.text "Estimated Volume: ", q ])
            (renderEstimatedVolume { unitId: charge.unit, dim: Nothing } nullDim)

    renderChargeDim charge =
      [ renderUnitHdr "ChargeDim" charge.unit
      , HH.table_
          $ [ HH.tr_ $ thColSpan (A.length dims) [ HH.text "Dimension" ] <> [ HH.th_ [ HH.text "Price" ] ]
            , HH.tr_ $ map (HH.th_ <<< A.singleton <<< HH.text) dims <> [ HH.th_ [] ]
            ]
          <> A.mapWithIndex renderChargeRow charge.priceByDim
          <> renderTotalEstimatedVolumeRow
      ]
      where
      unit = Map.lookup charge.unit unitMap

      renderTotalEstimatedVolumeRow =
        maybe []
          ( \q ->
              [ HH.tr_
                  $ thColSpanAlignRight (A.length dims)
                      [ Widgets.withTooltip Widgets.Top "Total Estimated Usage" (HH.text "Total #")
                      ]
                  <> [ HH.td_ [ q ] ]
              ]
          )
          (renderTotalEstimatedVolume unitId)

      -- Fatalistically assume that all units use the same dimensions.
      dims = case unit of
        Just (SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) }) -> A.fromFoldable $ Map.keys o.properties
        Just (SS.ChargeUnit { priceDimSchema: Just _ }) -> [ "" ]
        _ -> []

      renderChargeRow dimIdx (SS.PricePerDim p) =
        HH.tr_
          $ map (HH.td_ <<< A.singleton) (if A.null dims then [] else renderDimVals dims p.dim)
          <> [ HH.td_
                $ [ renderEditablePrice pIdx
                      price
                      charge.currency
                  ]
                <> maybe [] (\q -> [ HH.text " × ", q ]) (renderEstimatedVolume qIdx p.dim)
            ]
        where
        pIdx = { chargeIdx, subChargeIdx, dimIdx, unitIdx: 0, segIdx: 0 }

        qIdx = { unitId: charge.unit, dim: Just p.dim }

        price = SS.Price { price: p.price, listPrice: p.listPrice, discount: p.discount }

    renderChargeSeg c =
      [ renderUnitHdr "ChargeSeg" c.unit
      , HH.table_ $ [ HH.tr_ [ HH.th_ [ HH.text "Segment" ], HH.th_ [ HH.text "Price" ] ] ]
          <> map renderChargeSegRow segments
      , HH.text "Segmentation model: "
      , HH.text $ show model
      ]
        <> maybe []
            (\q -> [ HH.br_, HH.text "Estimated Volume: ", q ])
            (renderEstimatedVolume { unitId: c.unit, dim: Nothing } nullDim)
      where
      SS.Segmentation { model, segments } = c.segmentation

      renderPrice segIdx p =
        renderEditablePrice
          { chargeIdx, subChargeIdx, dimIdx: 0, unitIdx: 0, segIdx }
          (SS.Price { price: p.price, listPrice: p.listPrice, discount: p.discount })
          c.currency

      renderChargeSegRow seg@(SS.Segment { minimum }) =
        HH.tr_
          [ HH.td_ [ HH.text $ showSegment seg ]
          , HH.td_
              [ fromMaybe (HH.text "N/A")
                  $ findMapWithIndex
                      ( \i (SS.PricePerSeg p) ->
                          if p.minimum == minimum then
                            Just (renderPrice i p)
                          else
                            Nothing
                      )
                      c.priceBySegment
              ]
          ]

  renderChargeInner :: Int -> SS.Charge -> Array (H.ComponentHTML Action Slots m)
  renderChargeInner chargeIdx fullCharge = case fullCharge of
    SS.ChargeSingleUnit c -> renderSingleUnitCharge chargeIdx 0 c
    SS.ChargeList c -> A.intercalate [ HH.hr_ ] $ A.mapWithIndex (renderSingleUnitCharge chargeIdx) c.charges
    SS.ChargeDimUnitOptSeg c -> renderChargeDimUnitOptSeg c
    where
    renderChargeDimUnitOptSeg charge =
      [ HH.table_
          $ [ HH.tr_
                $ thColSpan (A.length dims) [ HH.text "Dimension" ]
                <> [ HH.th [ HP.colSpan $ A.length units ] [ HH.text "Unit" ]
                  , HH.th_ [ HH.text "Monthly Minimum" ]
                  ]
            , HH.tr_ $ map (HH.th_ <<< A.singleton) $ (HH.text <$> dims) <> renderUnitLabels <> [ HH.text "" ]
            ]
          <> mapWithIndex renderChargeRow charge.priceByUnitByDim
          <> totalEstimatedRow
      ]
      where
      units = A.mapMaybe (\id -> Map.lookup id unitMap) charge.units

      renderUnitLabels = mkLabel <$> units
        where
        mkLabel u =
          Widgets.withTooltip Widgets.Top (show $ _.kind $ unwrap u)
            $ HH.text
            $ Charge.chargeUnitLabel u

      -- Fatalistically assume that there is at least one unit defined, then
      -- fatalistically assume that all units use the same dimensions.
      dims = case A.head units of
        Just (SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) }) -> A.fromFoldable $ Map.keys o.properties
        Just (SS.ChargeUnit { priceDimSchema: Just _ }) -> [ "" ]
        _ -> []

      totalEstimatedRow
        | A.all A.null totalEstimatedCells = []
        | otherwise =
          [ HH.tr_
              $ thColSpanAlignRight (A.length dims)
                  [ Widgets.withTooltip Widgets.Top "Total Estimated Usage" (HH.text "Total #")
                  ]
              <> (HH.td_ <$> totalEstimatedCells)
          ]

      totalEstimatedCells = map (maybe [] A.singleton <<< renderTotalEstimatedVolume) charge.units

      currency unitId =
        A.findMap
          ( \(SS.ChargeCurrencyPerUnit c) ->
              if c.unit == unitId then
                Just c.currency
              else
                Nothing
          )
          charge.currencyByUnit

      renderChargeRow dimIdx = case _ of
        SS.PricePerDimUnitOptSeg (SS.PricePerDimUnitSeg p) ->
          HH.tr_
            $ map (HH.td_ <<< A.singleton)
            $ (if A.null dims then [] else renderDimVals dims p.dim)
            <> A.mapWithIndex (renderPriceBySegmentPerUnit p.dim dimIdx) p.priceBySegmentByUnit
            <> [ HH.text $ show p.periodMinimum ]
        SS.PricePerDimUnitOptNoSeg (SS.PricePerDimUnit p) ->
          HH.tr_
            $ map (HH.td_ <<< A.singleton)
            $ (if A.null dims then [] else renderDimVals dims p.dim)
            <> A.mapWithIndex (renderPricePerUnit p.dim dimIdx) p.priceByUnit
            <> [ HH.text $ show p.periodMinimum ]

      renderPriceBySegmentPerUnit _dim _dimIdx _priceIdx (SS.PricePerUnitSeg _ppus) = HH.text "TODO"

      renderPricePerUnit dim dimIdx unitIdx (SS.PricePerUnit ppu) =
        HH.div_
          $ [ renderEditablePrice pIdx price (currency ppu.unit) ]
          <> maybe [] (\q -> [ HH.text " × ", q ]) (renderEstimatedVolume qIdx dim)
        where
        pIdx = { chargeIdx, subChargeIdx: 0, dimIdx, unitIdx, segIdx: 0 }

        qIdx = { unitId: ppu.unit, dim: Just dim }

        price = SS.Price { price: ppu.price, listPrice: ppu.listPrice, discount: ppu.discount }

  renderCharge :: Int -> SS.Charge -> H.ComponentHTML Action Slots m
  renderCharge chargeIdx charge =
    HH.section [ HP.class_ Css.charge ]
      $ renderChargeInner chargeIdx charge
      <> [ HH.dl_
            $ opt (renderDataItemString "Description") (Charge.description charge)
            <> opt (renderDataItemString "Period Minimum" <<< show) (Charge.periodMinimum charge)
        -- <> renderDataItemString "Term of Price Change" (show c.termOfPriceChangeInDays <> " days")
        ]

thColSpan :: forall m. Int -> Array (H.ComponentHTML Action Slots m) -> Array (H.ComponentHTML Action Slots m)
thColSpan colSpan els
  | colSpan == 0 = []
  | otherwise = [ HH.th [ HP.colSpan colSpan, HP.style "text-align:center" ] els ]

thColSpanAlignRight :: forall m. Int -> Array (H.ComponentHTML Action Slots m) -> Array (H.ComponentHTML Action Slots m)
thColSpanAlignRight colSpan els
  | colSpan == 0 = []
  | otherwise = [ HH.th [ HP.colSpan colSpan, HP.style "text-align:right" ] els ]

showChargeUnitRef :: SS.ChargeUnitId -> String
showChargeUnitRef (SS.ChargeUnitId id) = id

showSegment :: SS.Segment -> String
showSegment (SS.Segment s) = "[" <> show s.minimum <> "," <> maybe "∞" show s.exclusiveMaximum <> ")"

aggregateQuantity :: QuantityMap -> AggregatedQuantityMap
aggregateQuantity quantityMap =
  Map.fromFoldable
    $ do
        Tuple unitId dimMap <- Map.toUnfoldable quantityMap :: List _
        let
          q = case dimMap of
            Left q' -> QtWAP q'
            Right m ->
              (\(Additive n) -> QtSum n)
                $ List.foldl (\a b -> a <> Additive b) mempty
                $ Map.values m
        pure $ Tuple unitId q

handleAction :: forall m. Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  SetCustomPrice { chargeIdx, subChargeIdx, dimIdx, unitIdx, segIdx } (SS.Price price) ->
    let
      updatePrice ::
        forall r.
        { listPrice :: Number, price :: Number, discount :: Maybe SS.Discount | r } ->
        { listPrice :: Number, price :: Number, discount :: Maybe SS.Discount | r }
      updatePrice p =
        p
          { price = price.price
          , listPrice = price.listPrice
          , discount = price.discount
          }

      updatePricePerDim (SS.PricePerDim p) = SS.PricePerDim $ updatePrice p

      updatePricePerSeg (SS.PricePerSeg p) = SS.PricePerSeg $ updatePrice p

      updatePriceBySegment ::
        forall r.
        { priceBySegment :: Array SS.PricePerSeg | r } ->
        { priceBySegment :: Array SS.PricePerSeg | r }
      updatePriceBySegment c =
        c
          { priceBySegment =
            fromMaybe c.priceBySegment
              $ A.modifyAt segIdx updatePricePerSeg c.priceBySegment
          }

      updatePricePerUnitSeg (SS.PricePerUnitSeg p) = SS.PricePerUnitSeg $ updatePriceBySegment p

      updatePricePerUnit (SS.PricePerUnit p) = SS.PricePerUnit $ updatePrice p

      updatePricePerDimUnitSeg (SS.PricePerDimUnitSeg p) =
        SS.PricePerDimUnitSeg
          $ p
              { priceBySegmentByUnit =
                fromMaybe p.priceBySegmentByUnit
                  $ A.modifyAt unitIdx updatePricePerUnitSeg p.priceBySegmentByUnit
              }

      updatePricePerDimUnit (SS.PricePerDimUnit p) =
        SS.PricePerDimUnit
          $ p
              { priceByUnit =
                fromMaybe p.priceByUnit
                  $ A.modifyAt unitIdx updatePricePerUnit p.priceByUnit
              }

      updatePricePerDimUnitOptSeg = case _ of
        SS.PricePerDimUnitOptSeg p -> SS.PricePerDimUnitOptSeg $ updatePricePerDimUnitSeg p
        SS.PricePerDimUnitOptNoSeg p -> SS.PricePerDimUnitOptNoSeg $ updatePricePerDimUnit p

      updateSingleUnitCharge = case _ of
        SS.ChargeSimple c -> SS.ChargeSimple $ updatePrice c
        SS.ChargeDim c ->
          SS.ChargeDim
            $ c
                { priceByDim = fromMaybe c.priceByDim $ A.modifyAt dimIdx updatePricePerDim c.priceByDim
                }
        SS.ChargeSeg c -> SS.ChargeSeg $ updatePriceBySegment c
        SS.ChargeDimSeg c -> SS.ChargeDimSeg $ c { priceBySegmentByDim = c.priceBySegmentByDim }

      updateCharge = case _ of
        SS.ChargeSingleUnit c -> SS.ChargeSingleUnit $ updateSingleUnitCharge c
        SS.ChargeList c ->
          SS.ChargeList
            $ c
                { charges =
                  fromMaybe c.charges
                    $ A.modifyAt subChargeIdx updateSingleUnitCharge c.charges
                }
        SS.ChargeDimUnitOptSeg c ->
          SS.ChargeDimUnitOptSeg
            $ c
                { priceByUnitByDim =
                  fromMaybe c.priceByUnitByDim
                    $ A.modifyAt dimIdx updatePricePerDimUnitOptSeg c.priceByUnitByDim
                }
    in
      do
        st' <-
          H.modify \st ->
            st
              { charges = fromMaybe st.charges $ A.modifyAt chargeIdx updateCharge st.charges
              }
        H.raise { charges: st'.charges, estimatedUsage: st'.estimatedUsage }
  SetQuantity _ Nothing -> pure unit
  SetQuantity { dim, unitId } (Just q) -> do
    st' <-
      H.modify \st ->
        let
          estimatedUsage' =
            let
              updateUnit = Map.update updateDim unitId

              updateDim dimMap = case dim of
                Nothing -> Just $ Left q
                Just dim' -> case dimMap of
                  Left _ -> Just $ Right $ Map.singleton dim' q
                  Right m -> Just $ Right $ Map.insert dim' q m
            in
              updateUnit st.estimatedUsage
        in
          st
            { estimatedUsage = estimatedUsage'
            , aggregatedQuantity = aggregateQuantity estimatedUsage'
            }
    -- If we are setting a dimension specific quantity then override the value
    -- of the aggregate unit quantity component.
    when (isJust dim)
      $ let
          q' = qtToValue <$> Map.lookup unitId st'.aggregatedQuantity
        in
          H.tell EditableQuantity.proxy { unitId, dim: Nothing } (EditableQuantity.SetQuantity q')
    -- If we are setting an aggregated quantity then override the dimension
    -- specific values.
    when (isNothing dim)
      $ traverse_
          ( \dim' ->
              H.tell EditableQuantity.proxy
                { unitId, dim: Just dim' }
                (EditableQuantity.SetQuantity Nothing)
          )
      $ Set.unions
      $ Charge.dims
      <$> st'.charges
    -- Inform the parent component (typically the order form) about the new
    -- prices and quantity.
    H.raise { charges: st'.charges, estimatedUsage: st'.estimatedUsage }
