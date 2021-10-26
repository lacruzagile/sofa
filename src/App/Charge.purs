-- | A component that presents a view of product charges and quantity.
module App.Charge (Slot, Output, QuantityMap, proxy, component) where

import Prelude
import App.EditablePrice as EditablePrice
import App.EditableQuantity as EditableQuantity
import Css as Css
import Data.Array (mapWithIndex)
import Data.Array as A
import Data.Charge as Charge
import Data.Either (Either(..))
import Data.Estimate (Estimate)
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (findMapWithIndex)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.SmartSpec as SS
import Data.SubTotal as SubTotal
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
  = { unitID :: SS.ChargeUnitID, dim :: Maybe SS.DimValue }

type Input
  = { unitMap :: Charge.ChargeUnitMap
    , defaultCurrency :: SS.ChargeCurrency
    , charges :: Array SS.Charge
    , quantity :: QuantityMap
    }

type Output
  = { charges :: Array SS.Charge, quantity :: QuantityMap }

type State
  = { unitMap :: Charge.ChargeUnitMap
    , defaultCurrency :: SS.ChargeCurrency
    , charges :: Array SS.Charge
    , quantity :: QuantityMap
    , aggregatedQuantity :: AggregatedQuantityMap
    }

-- | The quantity is either set as an aggregate or individually for each of the
-- | unit's dimensions.
type QuantityMap
  = Map SS.ChargeUnitID (Either (Estimate Int) (Map SS.DimValue (Estimate Int)))

-- | The aggregated quantity has one quantity per unit. Note, if per-dimension
-- | quantities are used and a dimension quantity is missing then the aggregated
-- | value is `Nothing`.
type AggregatedQuantityMap
  = Map SS.ChargeUnitID AggregatedQuantity

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
  = SetCustomPrice PriceIndex SS.Price
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
  , defaultCurrency: input.defaultCurrency
  , charges: input.charges
  , quantity: input.quantity
  , aggregatedQuantity: aggregateQuantity input.quantity
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { unitMap, defaultCurrency, charges, quantity, aggregatedQuantity } =
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

  renderEditableQuantity :: QuantityIndex -> Maybe (Estimate Int) -> H.ComponentHTML Action Slots m
  renderEditableQuantity quantityIdx qty =
    HH.slot EditableQuantity.proxy quantityIdx EditableQuantity.component qty
      $ SetQuantity quantityIdx

  renderDimVals :: Array String -> SS.DimValue -> Array (H.ComponentHTML Action Slots m)
  renderDimVals dimKeys (SS.DimValue v) = renderConfigValue dimKeys v

  renderConfigValue :: Array String -> SS.ConfigValue -> Array (H.ComponentHTML Action Slots m)
  renderConfigValue dimKeys = go
    where
    go = case _ of
      SS.CvObject m -> A.concat $ (\d -> maybe [ HH.text "N/A" ] go $ Map.lookup d m) <$> dimKeys
      SS.CvArray ms -> [ HH.ul [ HP.class_ Css.priceList ] $ (HH.li_ <<< go) <$> ms ]
      v -> [ HH.text $ show v ]

  renderTotalQuantity :: SS.ChargeUnitID -> H.ComponentHTML Action Slots m
  renderTotalQuantity unitID =
    HH.text $ fromMaybe "N/A"
      $ do
          q <- Map.lookup unitID aggregatedQuantity
          pure $ show $ qtToValue <$> q

  renderSingleUnitCharge :: Int -> Int -> SS.ChargeSingleUnit -> Array (H.ComponentHTML Action Slots m)
  renderSingleUnitCharge chargeIdx subChargeIdx fullCharge = case fullCharge of
    SS.ChargeSimple c -> renderChargeSimple c
    SS.ChargeDim c -> renderChargeDim c
    SS.ChargeSeg c -> renderChargeSeg c
    SS.ChargeDimSeg _c -> [ HH.text "TODO" ]
    where
    unitID = case fullCharge of
      SS.ChargeSimple c -> c.unit
      SS.ChargeDim c -> c.unit
      SS.ChargeSeg c -> c.unit
      SS.ChargeDimSeg c -> c.unit

    nullDim = SS.DimValue SS.CvNull

    findDimQuantity :: SS.DimValue -> Maybe (Estimate Int)
    findDimQuantity dim = do
      q <- Map.lookup unitID quantity
      case q of
        Left _ -> Nothing
        Right dimMap -> Map.lookup dim dimMap

    renderUnitHdr :: String -> SS.ChargeUnitID -> H.ComponentHTML Action Slots m
    renderUnitHdr kind u = HH.h4_ [ HH.text $ showChargeUnitRef u, HH.sup_ [ HH.text " (", HH.text kind, HH.text ")" ] ]

    renderTotalPrice :: H.ComponentHTML Action Slots m
    renderTotalPrice =
      SubTotal.renderSubTotalText
        $ SubTotal.calcSubTotal quantity unitMap defaultCurrency (SS.ChargeSingleUnit fullCharge)

    renderChargeSimple charge =
      [ renderUnitHdr "ChargeSimple" charge.unit
      , HH.text "Price: "
      , renderEditablePrice
          { chargeIdx, subChargeIdx, dimIdx: 0, unitIdx: 0, segIdx: 0 }
          (SS.Price { price: charge.price, listPrice: charge.listPrice, discount: charge.discount })
          charge.currency
      , HH.br_
      , HH.text "Quantity: "
      , renderEditableQuantity { unitID: charge.unit, dim: Nothing } (findDimQuantity nullDim)
      , HH.br_
      , HH.text "Total: "
      , renderTotalPrice
      ]

    renderChargeDim charge =
      [ renderUnitHdr "ChargeDim" charge.unit
      , HH.table_
          $ [ HH.tr_ $ thColSpan (A.length dims) [ HH.text "Dimension" ] <> [ HH.th_ [ HH.text "Price" ] ]
            , HH.tr_ $ map (HH.th_ <<< A.singleton <<< HH.text) dims <> [ HH.th_ [] ]
            ]
          <> A.mapWithIndex renderChargeRow charge.priceByDim
          <> [ HH.tr_ $ thColSpanAlignRight (A.length dims) [ HH.text "Total #" ] <> [ HH.td_ [ renderTotalQuantity unitID ] ] ]
          <> [ HH.tr_ $ thColSpanAlignRight (A.length dims) [ HH.text "Total Price" ] <> [ HH.td_ [ renderTotalPrice ] ] ]
      ]
      where
      unit = Map.lookup charge.unit unitMap

      -- Fatalistically assume that all units use the same dimensions.
      dims = case unit of
        Just (SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) }) -> A.fromFoldable $ Map.keys o.properties
        Just (SS.ChargeUnit { priceDimSchema: Just _ }) -> [ "" ]
        _ -> []

      renderChargeRow dimIdx (SS.PricePerDim p) =
        HH.tr_
          $ map (HH.td_ <<< A.singleton) (if A.null dims then [] else renderDimVals dims p.dim)
          <> [ HH.td_
                [ renderEditablePrice pIdx
                    price
                    charge.currency
                , HH.text " × "
                , renderEditableQuantity qIdx (findDimQuantity p.dim)
                ]
            ]
        where
        pIdx = { chargeIdx, subChargeIdx, dimIdx, unitIdx: 0, segIdx: 0 }

        qIdx = { unitID: charge.unit, dim: Just p.dim }

        price = SS.Price { price: p.price, listPrice: p.listPrice, discount: p.discount }

    renderChargeSeg c =
      [ renderUnitHdr "ChargeSeg" c.unit
      , HH.table_ $ [ HH.tr_ [ HH.th_ [ HH.text "Segment" ], HH.th_ [ HH.text "Price" ] ] ]
          <> map renderChargeSegRow segments
      , HH.text "Segmentation model: "
      , HH.text $ show model
      , HH.br_
      , HH.text "Quantity: "
      , renderEditableQuantity { unitID: c.unit, dim: Nothing } (findDimQuantity nullDim)
      , HH.br_
      , HH.text "Total: "
      , renderTotalPrice
      ]
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
          <> [ HH.tr_
                $ thColSpanAlignRight (A.length dims) [ HH.text "Total #" ]
                <> map (HH.td_ <<< A.singleton <<< renderTotalQuantity) charge.units
            ]
      , HH.text "Total: "
      , renderTotalPrice
      ]
      where
      units = A.mapMaybe (\id -> Map.lookup id unitMap) charge.units

      renderUnitLabels = mkLabel <$> units
        where
        mkLabel u =
          Widgets.withTooltip Widgets.Top (show $ _.chargeType $ unwrap u)
            $ HH.text
            $ Charge.chargeUnitLabel u

      renderTotalPrice :: H.ComponentHTML Action Slots m
      renderTotalPrice =
        SubTotal.renderSubTotalText
          $ SubTotal.calcSubTotal quantity unitMap defaultCurrency fullCharge

      -- Fatalistically assume that there is at least one unit defined, then
      -- fatalistically assume that all units use the same dimensions.
      dims = case A.head units of
        Just (SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) }) -> A.fromFoldable $ Map.keys o.properties
        Just (SS.ChargeUnit { priceDimSchema: Just _ }) -> [ "" ]
        _ -> []

      currency unitID =
        A.findMap
          ( \(SS.ChargeCurrencyPerUnit c) ->
              if c.unit == unitID then
                Just c.currency
              else
                Nothing
          )
          charge.currencyByUnit

      findDimQuantity :: SS.ChargeUnitID -> SS.DimValue -> Maybe (Estimate Int)
      findDimQuantity unitID dim = do
        q <- Map.lookup unitID quantity
        case q of
          Left q' -> pure q'
          Right dimMap -> Map.lookup dim dimMap

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
          [ renderEditablePrice pIdx price (currency ppu.unit)
          , HH.text " × "
          , renderEditableQuantity qIdx (findDimQuantity ppu.unit dim)
          ]
        where
        pIdx = { chargeIdx, subChargeIdx: 0, dimIdx, unitIdx, segIdx: 0 }

        qIdx = { unitID: ppu.unit, dim: Just dim }

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

showChargeUnitRef :: SS.ChargeUnitID -> String
showChargeUnitRef (SS.ChargeUnitID id) = id

showSegment :: SS.Segment -> String
showSegment (SS.Segment s) = "[" <> show s.minimum <> "," <> maybe "∞" show s.exclusiveMaximum <> ")"

aggregateQuantity :: QuantityMap -> AggregatedQuantityMap
aggregateQuantity quantityMap =
  Map.fromFoldable
    $ do
        Tuple unitID dimMap <- Map.toUnfoldable quantityMap :: List _
        let
          q = case dimMap of
            Left q' -> QtWAP <$> q'
            Right m ->
              map (\(Additive n) -> QtSum n)
                $ List.foldl (\a b -> a <> map Additive b) mempty
                $ Map.values m
        pure $ Tuple unitID q

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
        H.raise { charges: st'.charges, quantity: st'.quantity }
  SetQuantity _ Nothing -> pure unit
  SetQuantity { dim, unitID } (Just q) -> do
    st' <-
      H.modify \st ->
        let
          quantity' =
            let
              updateUnit = Map.update updateDim unitID

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
          q' = map (map qtToValue) $ Map.lookup unitID st'.aggregatedQuantity
        in
          H.tell EditableQuantity.proxy { unitID, dim: Nothing } (EditableQuantity.SetQuantity q')
    -- If we are setting an aggregated quantity then override the dimension
    -- specific values.
    when (isNothing dim)
      $ traverse_
          ( \dim' ->
              H.tell EditableQuantity.proxy
                { unitID, dim: Just dim' }
                (EditableQuantity.SetQuantity Nothing)
          )
      $ Set.unions
      $ Charge.dims
      <$> st'.charges
    -- Inform the parent component (typically the order form) about the new
    -- prices and quantity.
    H.raise { charges: st'.charges, quantity: st'.quantity }
