-- | A component that presents a view of product charges and quantity.
module Sofa.App.Charge (Output, Slot, component, proxy) where

import Prelude
import DOM.HTML.Indexed as HTML
import Data.Array as A
import Data.Either (Either(..))
import Data.FoldableWithIndex (findMapWithIndex)
import Data.Int (even)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Set as Set
import Data.String (Pattern(..), stripSuffix) as S
import Data.String.Utils (stripChars) as S
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.EditablePrice as EditablePrice
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.Charge as Charge
import Sofa.Data.Currency (numberFormatter)
import Sofa.Data.Quantity (QuantityMap, Quantity)
import Sofa.Data.Schema as Schema
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "charge"
proxy = Proxy

type Slots
  = ( editablePrice :: EditablePrice.Slot PriceIndex
    )

type PriceIndex
  = { chargeIdx :: Int, subChargeIdx :: Int, dimIdx :: Int, unitIdx :: Int, segIdx :: Int }

type QuantityIndex
  = { unitId :: SS.ChargeUnitId, dim :: Maybe SS.DimValue }

type Input
  = { unitMap :: Charge.ChargeUnitMap
    , defaultCurrency :: SS.ChargeCurrency
    , charges :: Array SS.Charge
    , estimatedUsage :: QuantityMap
    , priceOnly :: Boolean
    -- ^ Whether to only show the prices, i.e., without quantities.
    , readOnly :: Boolean
    -- ^ Whether editing the quantity and price should be allowed.
    }

type Output
  = { charges :: Array SS.Charge, estimatedUsage :: QuantityMap }

type State
  = { unitMap :: Charge.ChargeUnitMap
    , defaultCurrency :: SS.ChargeCurrency
    , charges :: Array SS.Charge
    , estimatedUsage :: QuantityMap
    , priceOnly :: Boolean
    -- ^ Whether to only show the prices, i.e., without quantities.
    , readOnly :: Boolean
    -- ^ Whether editing the quantity and price should be allowed.
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
  = NoOp
  | SetCustomPrice PriceIndex SS.Price
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
  , estimatedUsage: input.estimatedUsage
  , priceOnly: input.priceOnly
  , readOnly: input.readOnly
  , aggregatedQuantity: aggregateQuantity input.estimatedUsage
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render { unitMap
, defaultCurrency
, charges
, estimatedUsage
, aggregatedQuantity
, priceOnly
, readOnly
} =
  HH.ul_
    $ A.mapWithIndex (\i r -> HH.li_ [ renderCharge i r ]) charges
  where
  renderEditablePrice :: PriceIndex -> SS.Price -> Maybe SS.ChargeCurrency -> H.ComponentHTML Action Slots m
  renderEditablePrice priceIdx price currency =
    HH.slot EditablePrice.proxy priceIdx EditablePrice.component input
      $ SetCustomPrice priceIdx
    where
    input :: EditablePrice.Input
    input =
      { price: price
      , currency: fromMaybe defaultCurrency currency
      , readOnly
      }

  findDimQuantity :: SS.ChargeUnitId -> SS.DimValue -> Maybe Quantity
  findDimQuantity unitId dim = do
    q <- Map.lookup unitId estimatedUsage
    case q of
      Left q' -> pure q'
      Right dimMap -> Map.lookup dim dimMap

  renderTotalEstimatedVolume :: SS.ChargeUnitId -> Maybe (H.ComponentHTML Action Slots m)
  renderTotalEstimatedVolume unitId = do
    chargeKind <- Charge.lookupChargeKind unitId unitMap
    if chargeKind /= SS.CkUsage then
      Nothing
    else
      Just $ HH.text $ fromMaybe "N/A"
        $ do
            q <- Map.lookup unitId aggregatedQuantity
            pure $ numberFormatter $ Int.toNumber $ qtToValue q

  renderDimVals :: Array String -> SS.DimValue -> Array (H.ComponentHTML Action Slots m)
  renderDimVals dimKeys (SS.DimValue v) = renderConfigValue dimKeys v

  renderConfigValue :: Array String -> SS.ConfigValue -> Array (H.ComponentHTML Action Slots m)
  renderConfigValue dimKeys = go
    where
    go = case _ of
      SS.CvObject m -> A.concat $ (\d -> maybe [ HH.text "N/A" ] go $ Map.lookup d m) <$> dimKeys
      SS.CvArray ms -> [ HH.ul_ $ (HH.li_ <<< go) <$> ms ]
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

    renderChargeSimple charge = case Map.lookup charge.unit unitMap of
      Nothing -> [ HH.text $ "Error: Unit " <> show charge.unit <> " not found." ]
      Just unit@(SS.ChargeUnit { kind }) ->
        [ HH.table_
            [ HH.thead_ [ HH.tr [ HP.classes borderedBelow ] (thUnitLabel unit) ]
            , HH.tbody_
                [ HH.tr [ Css.classes [ "h-16", "bg-honey-100" ] ]
                    [ td_ [ renderEditablePrice pIdx price charge.currency ]
                    , td_ [ renderChargeUnit' kind qIdx nullDim ]
                    ]
                ]
            ]
        ]
      where
      pIdx :: PriceIndex
      pIdx = { chargeIdx, subChargeIdx: 0, dimIdx: 0, unitIdx: 0, segIdx: 0 }

      qIdx :: QuantityIndex
      qIdx = { unitId: charge.unit, dim: Nothing }

      price =
        SS.Price
          { listPrice: charge.listPrice
          , price: charge.price
          , discount: charge.discount
          }

    renderChargeDim charge = case Map.lookup charge.unit unitMap of
      Nothing -> [ HH.text $ "Error: Unit " <> show charge.unit <> " not defined." ]
      Just unit -> renderWithUnit unit
      where
      renderWithUnit :: SS.ChargeUnit -> Array (H.ComponentHTML Action Slots m)
      renderWithUnit unit =
        [ HH.table_
            $ [ HH.thead_
                  [ HH.tr [ HP.classes borderedBelow ]
                      $ thColSpan
                          (A.length dimLabels)
                          (gappedRight <> centered)
                          [ HH.text "Dimension" ]
                      <> thUnitLabel unit
                  , HH.tr [ HP.classes borderedBelow ]
                      $ map (th_ <<< A.singleton) dimLabels
                      <> thUnitSubLabels priceOnly unit
                  ]
              ]
            <> A.mapWithIndex (renderChargeRow unit) charge.priceByDim
            <> renderTotalEstimatedVolumeRow
        ]
        where
        renderTotalEstimatedVolumeRow
          | priceOnly = []
          | otherwise =
            maybe []
              ( \q ->
                  [ tfoot
                      [ HH.tr_
                          $ thTotalEstimatedUsageLabel (A.length dimLabels)
                          <> [ td_ [], td_ [ q ] ]
                      ]
                  ]
              )
              (renderTotalEstimatedVolume unitId)

        dimLabels = unitDimLabels unit

        dimKeys = unitDimKeys unit

        renderChargeRow (SS.ChargeUnit u) dimIdx (SS.PricePerDim p) =
          HH.tr
            [ Css.classes
                [ "h-16"
                , if even dimIdx then "bg-honey-100" else ""
                ]
            ]
            $ map tdOne_ (if A.null dimKeys then [] else renderDimVals dimKeys p.dim)
            <> [ td_ [ renderEditablePrice pIdx price charge.currency ]
              , td_ [ renderChargeUnit' u.kind qIdx p.dim ]
              ]
          where
          pIdx = { chargeIdx, subChargeIdx, dimIdx, unitIdx: 0, segIdx: 0 }

          qIdx = { unitId: charge.unit, dim: Just p.dim }

          price = SS.Price { price: p.price, listPrice: p.listPrice, discount: p.discount }

    renderChargeSeg c = case Map.lookup c.unit unitMap of
      Nothing -> [ HH.text $ "Error: Unit " <> show c.unit <> " not found." ]
      Just unit ->
        [ HH.table_
            $ [ HH.thead_
                  [ HH.tr [ HP.classes borderedBelow ] (thUnitLabel unit)
                  , HH.tr_
                      [ if priceOnly then HH.text "" else th_ [ HH.text "Est. Usage" ]
                      , th_ [ HH.text "Price" ]
                      , th_ [ renderSegmentLabel ]
                      ]
                  ]
              , HH.tbody_
                  $ ( if priceOnly then
                        [ HH.text "" ]
                      else
                        [ HH.tr [ Css.classes [ "h-16", "bg-honey-100" ] ]
                            [ td_ [ renderChargeUnit { unitId: c.unit, dim: Nothing } nullDim ]
                            , td_ []
                            , td_ []
                            ]
                        ]
                    )
                  <> map renderChargeSegRow segments
              ]
        ]
      where
      SS.Segmentation { model, segments } = c.segmentation

      renderSegmentLabel =
        Tooltip.render
          (Tooltip.defaultInput { text = "Segmentation model: " <> show model })
          (HH.text "Segment")

      renderPrice segIdx p =
        renderEditablePrice
          { chargeIdx, subChargeIdx, dimIdx: 0, unitIdx: 0, segIdx }
          (SS.Price { price: p.price, listPrice: p.listPrice, discount: p.discount })
          c.currency

      renderChargeSegRow seg@(SS.Segment { minimum }) =
        HH.tr [ Css.classes [ "h-16", "bg-honey-100" ] ]
          [ if priceOnly then HH.text "" else td_ []
          , td_
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
          , td_ [ HH.text $ showSegment seg ]
          ]

  renderChargeInner :: Int -> SS.Charge -> Array (H.ComponentHTML Action Slots m)
  renderChargeInner chargeIdx fullCharge = case fullCharge of
    SS.ChargeSingleUnit c -> renderSingleUnitCharge chargeIdx 0 c
    SS.ChargeList c -> A.intercalate [ HH.hr_ ] $ A.mapWithIndex (renderSingleUnitCharge chargeIdx) c.charges
    SS.ChargeDimUnitOptSeg c -> renderChargeDimUnitOptSeg c
    where
    renderChargeDimUnitOptSeg charge =
      [ HH.table_
          $ [ HH.thead_
                [ HH.tr [ HP.classes borderedBelow ]
                    $ thColSpan
                        (A.length dimLabels)
                        (gappedRight <> centered)
                        [ HH.text "Dimension" ]
                    <> A.concatMap thUnitLabel units
                , HH.tr [ HP.classes borderedBelow ]
                    $ map (th [] [] <<< A.singleton) dimLabels
                    <> A.concatMap (thUnitSubLabels priceOnly) units
                ]
            ]
          <> A.mapWithIndex renderChargeRow charge.priceByUnitByDim
          <> renderTotalEstimatedRow
      ]
      where
      units = A.mapMaybe (\id -> Map.lookup id unitMap) $ Set.toUnfoldable charge.units

      -- Fatalistically assume that there is at least one unit defined, then
      -- fatalistically assume that all units use the same dimensions.
      dimLabels = maybe [] unitDimLabels (A.head units)

      -- Fatalistically assume that there is at least one unit defined, then
      -- fatalistically assume that all units use the same dimensions.
      dimKeys = maybe [] unitDimKeys (A.head units)

      renderTotalEstimatedRow
        | priceOnly = []
        | A.all A.null totalEstimatedCells = []
        | otherwise =
          [ HH.tfoot_
              [ HH.tr [ HP.classes borderedAbove ]
                  $ thTotalEstimatedUsageLabel (A.length dimLabels)
                  <> A.concatMap
                      ( \c ->
                          [ td_ []
                          , td
                              ( if readOnly then
                                  []
                                else
                                  -- When editable then we have to adjust for the input box padding.
                                  [ Css.c "pl-5" ]
                              )
                              c
                          ]
                      )
                      totalEstimatedCells
              ]
          ]

      totalEstimatedCells =
        map (maybe [] A.singleton <<< renderTotalEstimatedVolume)
          $ A.fromFoldable charge.units

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
            $ map tdOne_
            $ (if A.null dimKeys then [] else renderDimVals dimKeys p.dim)
            <> A.mapWithIndex (renderPriceBySegmentPerUnit p.dim dimIdx) p.priceBySegmentByUnit
        SS.PricePerDimUnitOptNoSeg (SS.PricePerDimUnit p) ->
          HH.tr
            [ Css.classes
                [ "h-16"
                , if even dimIdx then "bg-honey-100" else ""
                ]
            ]
            $ map tdOne_
            $ renderDimVals dimKeys p.dim
            <> A.concat (A.mapWithIndex (renderPricePerUnit p.dim dimIdx) p.priceByUnit)

      renderPriceBySegmentPerUnit _dim _dimIdx _priceIdx (SS.PricePerUnitSeg _ppus) = HH.text "TODO"

      renderPricePerUnit dim dimIdx unitIdx (SS.PricePerUnit ppu) =
        [ renderEditablePrice pIdx price (currency ppu.unit)
        , renderChargeUnit qIdx dim
        ]
        where
        pIdx = { chargeIdx, subChargeIdx: 0, dimIdx, unitIdx, segIdx: 0 }

        qIdx = { unitId: ppu.unit, dim: Just dim }

        price = SS.Price { price: ppu.price, listPrice: ppu.listPrice, discount: ppu.discount }

  renderCharge :: Int -> SS.Charge -> H.ComponentHTML Action Slots m
  renderCharge chargeIdx charge =
    HH.section_
      $ [ case Charge.description charge of
            Nothing -> HH.text ""
            Just d -> HH.p [ Css.class_ "text-stormy-300" ] [ HH.text d ]
        ]
      <> renderChargeInner chargeIdx charge

  -- | Renders the charge unit for the given quantity index. The charge kind is
  -- | looked up from the `unitMap`.
  renderChargeUnit qIdx dim =
    fromMaybe' renderError do
      chargeKind <- Charge.lookupChargeKind qIdx.unitId unitMap
      pure $ renderChargeUnit' chargeKind qIdx dim
    where
    renderError _ =
      Tooltip.render
        ( Tooltip.defaultInput
            { text =
              "Solution file refers to undefined charge unit "
                <> show qIdx.unitId
                <> "."
            }
        )
        $ Tooltip.contentWithIcon
        $ HH.span
            [ Css.class_ "text-raspberry-500" ]
            [ HH.text "Error" ]

  -- | Renders the charge unit for the given charge kind and quantity index.
  renderChargeUnit' chargeKind qIdx dim = case chargeKind of
    SS.CkOnetime -> HH.text "/Item"
    SS.CkMonthly -> HH.text "/Month"
    SS.CkQuarterly -> HH.text "/Quarter"
    SS.CkUsage
      | priceOnly -> HH.text "/Item"
    SS.CkUsage
      | otherwise ->
        let
          usage = findDimQuantity qIdx.unitId dim
        in
          renderEditableUsage readOnly qIdx usage
    SS.CkSegment -> HH.text "/Segment" -- ???

renderEditableUsage :: forall m. Boolean -> QuantityIndex -> Maybe Quantity -> H.ComponentHTML Action Slots m
renderEditableUsage readOnly quantityIdx qty
  | readOnly = HH.text (maybe "0" (numberFormatter <<< Int.toNumber) qty)
  | otherwise =
    HH.input
      [ HP.type_ HP.InputText
      , Css.classes
          [ "nectary-input"
          , "px-2"
          , "w-24"
          ]
      , HP.value (maybe "0" (numberFormatter <<< Int.toNumber) qty)
      , HE.onValueChange
          ( maybe NoOp (SetQuantity quantityIdx <<< Just)
              <<< parseQuantity
          )
      ]
    where
    parseQuantity :: String -> Maybe Int
    parseQuantity = parseMagInt <<< S.stripChars ", "
      where
      parseMagInt s = case S.stripSuffix (S.Pattern "M") s of
        Just s' -> (1_000_000 * _) <$> Int.fromString s'
        Nothing -> case S.stripSuffix (S.Pattern "k") s of
          Just s' -> (1_000 * _) <$> Int.fromString s'
          Nothing -> Int.fromString s

thUnitLabel :: forall w i. SS.ChargeUnit -> Array (HH.HTML w i)
thUnitLabel unit@(SS.ChargeUnit u) =
  thColSpan 2 (maybeGapped <> centered)
    [ Tooltip.render
        (Tooltip.defaultInput { text = show u.kind })
        (HH.text $ Charge.chargeUnitLabel unit <> " unit")
    ]
  where
  maybeGapped = maybe [] (\_ -> gappedLeft) u.priceDimSchema

thUnitSubLabels :: forall w i. Boolean -> SS.ChargeUnit -> Array (HH.HTML w i)
thUnitSubLabels priceOnly (SS.ChargeUnit u) = case u.kind of
  SS.CkUsage
    | not priceOnly -> [ thPrice, th_ [ HH.text "Est. Usage" ] ]
  _ -> [ thPrice, th_ [] ]
  where
  thPrice = th maybeGapped [] [ HH.text "Price" ]

  maybeGapped = maybe [] (\_ -> gappedLeft) u.priceDimSchema

thTotalEstimatedUsageLabel :: forall w i. Int -> Array (HH.HTML w i)
thTotalEstimatedUsageLabel numDims = case numDims of
  0 -> thLabel
  _ -> thColSpan (numDims - 1) [] [] <> thLabel
  where
  thLabel =
    [ th_
        [ Tooltip.render
            (Tooltip.defaultInput { text = "Total Estimated Usage" })
            (HH.text "Total #")
        ]
    ]

unitDimLabels :: forall w i. SS.ChargeUnit -> Array (HH.HTML w i)
unitDimLabels = case _ of
  SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) } ->
    let
      withDesc cse label = case SS.configSchemaEntryDescription cse of
        Nothing -> HH.text label
        Just description ->
          Tooltip.render
            (Tooltip.defaultInput { text = description, width = Just "20rem" })
            (Tooltip.contentWithIcon $ HH.text label)

      dimLabel key cse = withDesc cse $ fromMaybe key $ Schema.getTitle cse
    in
      FO.values $ FO.mapWithKey dimLabel o.properties
  SS.ChargeUnit { priceDimSchema: Just _ } -> [ HH.text "" ]
  _ -> []

unitDimKeys :: SS.ChargeUnit -> Array String
unitDimKeys = case _ of
  SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) } -> FO.keys o.properties
  _ -> []

tfoot :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
tfoot = HH.thead [ Css.classes [ "border-t", "border-stormy-200" ] ]

thBaseClasses ∷ Array HH.ClassName
thBaseClasses =
  Css.cs
    [ "py-3"
    , "px-3"
    , "box-content"
    , "font-semibold"
    , "text-stormy-200"
    ]

th :: forall w i. Array HH.ClassName -> HH.Node HTML.HTMLth w i
th classes props = HH.th $ [ HP.classes (thBaseClasses <> classes) ] <> props

th_ :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
th_ = th [] []

td :: forall w i. Array HH.ClassName -> Array (HH.HTML w i) -> HH.HTML w i
td classes = HH.td [ HP.classes ([ Css.c "px-3" ] <> classes) ]

td_ :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
td_ = td []

tdOne_ :: forall w i. HH.HTML w i -> HH.HTML w i
tdOne_ e = td_ [ e ]

thColSpan ::
  forall w i.
  Int ->
  Array HH.ClassName ->
  Array (HH.HTML w i) ->
  Array (HH.HTML w i)
thColSpan colSpan classes els
  | colSpan == 0 = []
  | otherwise = [ th classes [ HP.colSpan colSpan ] els ]

borderedBelow :: Array HH.ClassName
borderedBelow = [ Css.c "border-b", Css.c "border-stormy-200" ]

borderedAbove ∷ Array HH.ClassName
borderedAbove = [ Css.c "border-t", Css.c "border-stormy-200" ]

centered :: Array HH.ClassName
centered = [ Css.c "text-center" ]

-- | CSS classes the produce a gap in the table row border to the right of the
-- | current cell.
gappedRight :: Array HH.ClassName
gappedRight = [ Css.c "border-r-8", Css.c "border-transparent" ]

-- | CSS classes the produce a gap in the table row border to the left of the
-- | current cell.
gappedLeft :: Array HH.ClassName
gappedLeft = [ Css.c "border-l-8", Css.c "border-transparent" ]

showSegment :: SS.Segment -> String
showSegment (SS.Segment s) = case s.exclusiveMaximum of
  Nothing -> "from " <> show s.minimum
  Just emax -> show s.minimum <> " to " <> show (emax - 1)

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
  NoOp -> pure unit
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
    -- Inform the parent component (typically the order form) about the new
    -- prices and quantity.
    H.raise { charges: st'.charges, estimatedUsage: st'.estimatedUsage }
