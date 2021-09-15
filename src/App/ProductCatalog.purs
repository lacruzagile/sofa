module App.ProductCatalog (Slot, proxy, component) where

import Prelude
import Css as Css
import Data.Array (concatMap, fromFoldable, head, length, mapMaybe, singleton, sortBy)
import Data.Loadable (Loadable(..), getJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec as SS
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "productCatalog"
proxy = Proxy

type State
  = Loadable SS.ProductCatalog

data Action
  = ClearState
  | CheckToLoad
  | LoadProductCatalog String

component ::
  forall query input output m.
  MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = initialize
            }
    }

initialState :: forall input. input -> State
initialState = const Idle

initialize :: Maybe Action
initialize = Just $ LoadProductCatalog "v1alpha1/examples/product-catalog.normalized.json"

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = HH.section_ [ HH.article_ content ]
  where
  opt :: forall a b. (a -> Array b) -> Maybe a -> Array b
  opt = maybe []

  -- Apply the given function if array is non-empty, otherwise return empty array.
  optArr :: forall a b. (Array a -> Array b) -> Array a -> Array b
  optArr f = case _ of
    [] -> []
    xs -> f xs

  blockList = HH.ul [ HP.class_ Css.blocklist ]

  dataItem label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ HH.text value ]
    ]

  dataItemRaw label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ value ]
    ]

  renderSku :: SS.Sku -> H.ComponentHTML Action slots m
  renderSku = case _ of
    SS.SkuCode s ->
      HH.dl [ HP.class_ Css.hblock ]
        $ dataItem "Code" s
    SS.Sku s ->
      HH.dl [ HP.class_ Css.hblock ]
        $ dataItem "Code" s.code
        <> dataItem "Name" s.name
        <> opt (dataItem "Description") s.description
        <> dataItem "Product Category" (show s.productCategory)
        <> dataItem "Platform" (show s.platform)

  productOption :: SS.ProductOption -> H.ComponentHTML Action slots m
  productOption (SS.ProdOptSkuCode s) = HH.li_ [ HH.text s ]

  productOption (SS.ProductOption po) =
    HH.li_
      [ HH.dl_
          ( dataItemRaw "SKU" (renderSku po.sku)
              <> opt (dataItem "Name") po.name
              <> dataItem "Required" (show po.required)
              <> dataItem "Quote Line Visible" (show po.quoteLineVisible)
              <> dataItem "Quantity" (show po.quantity)
              <> dataItem "Min Quantity" (show po.minQuantity)
              <> dataItem "Max Quantity" (show po.maxQuantity)
              <> dataItem "Selected by Default" (show po.selectedByDefault)
              <> dataItem "Type" (show po.type_)
          )
      ]

  productOptions :: Maybe (Array SS.ProductOption) -> Array (H.ComponentHTML Action slots m)
  productOptions = maybe [] (html <<< blockList <<< map productOption)
    where
    html x =
      [ HH.dt_ [ HH.text "Product Options" ]
      , HH.dd_ [ x ]
      ]

  configSchemaEntry :: SS.ConfigSchemaEntry -> H.ComponentHTML Action slots m
  configSchemaEntry = case _ of
    SS.CseInteger v -> renderInteger v
    SS.CseString v -> renderString v
    SS.CseRegex v -> renderRegex v
    SS.CseConst v -> renderConst v
    SS.CseArray v -> renderArray v
    SS.CseObject v -> renderObject v
    SS.CseOneOf v -> renderOneOf v
    where
    renderInteger v =
      HH.dl_
        ( opt (dataItem "Minimum" <<< show) v.minimum
            <> opt (dataItem "Maximum" <<< show) v.maximum
            <> opt (dataItem "Default" <<< show) v.default
        )

    renderString v =
      HH.dl_
        ( opt (dataItem "Minimum Length" <<< show) v.minLength
            <> opt (dataItem "Maximum Length" <<< show) v.maxLength
            <> opt (dataItem "Default") v.default
        )

    renderRegex v =
      HH.dl_
        ( dataItem "Pattern" v.pattern
            <> opt (dataItem "Default") v.default
        )

    renderConst v = HH.text $ show $ v.const

    renderArray v = HH.dl_ $ dataItemRaw "Items" $ HH.dl_ $ renderInner v.items

    renderObject v = configSchema v.properties

    renderOneOf v =
      HH.dl_
        [ HH.dt_ [ HH.text "One Of" ]
        , HH.dd_ [ HH.dl_ $ concatMap renderInner $ v.oneOf ]
        ]

    renderInner v = dataItemRaw (showCseTypeName v) (configSchemaEntry v)

  configSchema :: Map String SS.ConfigSchemaEntry -> H.ComponentHTML Action slots m
  configSchema = HH.dl_ <<< concatMap entry <<< Map.toUnfoldable
    where
    entry (Tuple k e) =
      [ HH.dt_ [ HH.text k, HH.text " (", HH.text $ showCseTypeName e, HH.text ")" ]
      , HH.dd_ [ configSchemaEntry e ]
      ]

  product :: SS.Product -> H.ComponentHTML Action slots m
  product (SS.Product p) =
    HH.li_
      [ HH.dl_ $ dataItem "SKU" p.sku
          <> opt (dataItem "Name") p.name
          <> opt (dataItem "Description") p.description
          <> opt (dataItemRaw "Attributes" <<< renderConfigValues) p.attr
          <> opt (dataItemRaw "Configuration Schema" <<< configSchema) p.configSchema
          <> productOptions p.options
          <> opt (dataItem "Features" <<< const "TODO") p.features
          <> opt (dataItem "Variables" <<< const "TODO") p.variables
          <> dataItemRaw "Units" (renderSpecUnits p.chargeUnits)
          <> opt (dataItem "Rules" <<< const "TODO") p.rules
      ]

  renderSpecUnit :: SS.ChargeUnit -> H.ComponentHTML Action slots m
  renderSpecUnit (SS.ChargeUnit u) =
    HH.dl_
      $ dataItem "Id" u.id
      <> opt (dataItem "Name") u.name
      <> opt (dataItem "Description") u.description
      <> dataItem "Charge Type" (show u.chargeType)
      <> opt (dataItemRaw "Price Dimension Schema" <<< configSchemaEntry) u.priceDimSchema

  renderSpecUnits :: Array SS.ChargeUnit -> H.ComponentHTML Action slots m
  renderSpecUnits = blockList <<< map (HH.li_ <<< singleton <<< renderSpecUnit)

  renderConfigValues :: Map String SS.ConfigValue -> H.ComponentHTML Action slots m
  renderConfigValues = HH.dl_ <<< concatMap (uncurry entry) <<< Map.toUnfoldable
    where
    entry k v = dataItem k (show v)

  renderSimplePrice :: SS.ChargeUnitMap -> SS.ChargeUnitRef -> SS.SimplePrice -> H.ComponentHTML Action slots m
  renderSimplePrice unitMap unit = case _ of
    SS.SimplePriceSegmented p -> renderPriceByUnitPerDim unitMap (singleton unit) (conv' p)
    SS.SimplePriceByDim p -> renderPriceByUnitPerDim unitMap (singleton unit) (map conv p)
    where
    conv (SS.PriceByDim p) =
      SS.PriceByUnitPerDim
        { dim: p.dim
        , prices: singleton $ SS.PriceByUnit { price: p.price, unit }
        , monthlyMinimum: 0.0
        }

    conv' price =
      singleton
        $ SS.PriceByUnitPerDim
            { dim: SS.DimValue SS.CvNull
            , prices: singleton $ SS.PriceByUnit { unit, price }
            , monthlyMinimum: 0.0
            }

  renderSegmentation :: SS.PriceSegmentation -> H.ComponentHTML Action slots m
  renderSegmentation (SS.PriceSegmentation p) =
    HH.dl_
      $ dataItem "Segment Unit" (showChargeUnitRef p.unit)
      <> dataItem "Period" (show p.period)
      <> dataItem "Model" (show p.model)
      <> dataItemRaw "Segments" (HH.ul_ $ segment <$> p.segments)
    where
    segment s = HH.li_ [ HH.text $ showSegment s ]

  renderPriceSegmentationPerUnit :: SS.PriceSegmentationPerUnit -> H.ComponentHTML Action slots m
  renderPriceSegmentationPerUnit (SS.PriceSegmentationPerUnit p) =
    HH.dl_
      $ dataItem "Unit" (showChargeUnitRef p.unit)
      <> dataItemRaw "Segmentation" (renderSegmentation p.segmentation)

  renderPriceSegmentationsByUnit :: Array SS.PriceSegmentationPerUnit -> H.ComponentHTML Action slots m
  renderPriceSegmentationsByUnit = HH.ul_ <<< map (HH.li_ <<< singleton <<< renderPriceSegmentationPerUnit)

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

  renderPriceByUnitPerDim ::
    SS.ChargeUnitMap ->
    Array SS.ChargeUnitRef ->
    Array SS.PriceByUnitPerDim ->
    H.ComponentHTML Action slots m
  renderPriceByUnitPerDim unitMap unitRefs ppd =
    HH.table_
      $ [ HH.tr_
            [ HH.th [ HP.colSpan $ length dims ] [ HH.text "Dimension" ]
            , HH.th [ HP.colSpan $ length units ] [ HH.text "Unit" ]
            , HH.th_ [ HH.text "Monthly Minimum" ]
            ]
        , HH.tr_ $ map (HH.th_ <<< singleton <<< HH.text) $ dims <> unitLabels <> [ "" ]
        ]
      <> map priceRow ppd
    where
    priceRow (SS.PriceByUnitPerDim p) =
      HH.tr_
        $ map (HH.td_ <<< singleton <<< HH.text)
        $ dimVals p.dim
        <> priceVals p.prices
        <> [ show p.monthlyMinimum ]

    units = mapMaybe (\(SS.ChargeUnitRef u) -> Map.lookup u.unitID unitMap) unitRefs

    unitLabels = SS.chargeUnitLabel <$> units

    -- Fatalistically assume that there is at least one unit defined, then
    -- fatalistically assume that all units use the same dimensions.
    dims = case head units of
      Just (SS.ChargeUnit { priceDimSchema: Just (SS.CseObject o) }) -> fromFoldable $ Map.keys $ o.properties
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
    priceVals = map priceVal <<< sortBy (comparing (_.unitID <<< unwrap <<< _.unit <<< unwrap))

  renderRateCardCharge :: SS.ChargeUnitMap -> SS.Charge -> H.ComponentHTML Action slots m
  renderRateCardCharge unitMap = case _ of
    SS.ChargeSimple r ->
      HH.dl_ $ dataItemRaw "Price" (renderSimplePrice unitMap r.unit r.price)
        <> opt (dataItemRaw "Segmentation" <<< renderSegmentation) r.segmentation
        <> dataItem "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
        <> dataItem "Monthly Minimum" (show r.monthlyMinimum)
    SS.ChargeMixed r ->
      HH.dl_ $ optArr (dataItemRaw "Price Segmentations" <<< renderPriceSegmentationsByUnit) r.segmentationByUnit
        <> optArr (dataItemRaw "Default Prices" <<< renderDefaultPrices) r.defaultPriceByUnit
        <> dataItemRaw "Prices per Dimension" (renderPriceByUnitPerDim unitMap r.units r.priceByUnitByDim)
        <> dataItem "Monthly Minimum" (show r.monthlyMinimum)
        <> dataItem "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
    SS.ChargeArray rs -> HH.ol_ <<< map (\r -> HH.li_ [ renderRateCardCharge unitMap r ]) $ rs

  renderRateCard :: SS.ChargeUnitMap -> SS.RateCard -> H.ComponentHTML Action slots m
  renderRateCard unitMap (SS.RateCard r) =
    HH.li_ $ dataItemRaw "SKU" (renderSku r.sku)
      <> opt (dataItem "Name") r.name
      <> opt (dataItem "Description") r.description
      <> dataItemRaw "Charge" (renderRateCardCharge unitMap r.charge)

  renderRateCards :: Map String SS.ChargeUnitMap -> Array SS.RateCard -> H.ComponentHTML Action slots m
  renderRateCards prodMap = blockList <<< map (\rc@(SS.RateCard { sku }) -> renderRateCard (fromMaybe Map.empty $ Map.lookup (showSkuCode sku) prodMap) rc)

  renderCurrency :: SS.Currency -> String
  renderCurrency (SS.Currency c) =
    c.code
      <> maybe "" (\c' -> "\"" <> c' <> "\"") c.country

  renderPriceBookCurrency :: Map String SS.ChargeUnitMap -> SS.PriceBookCurrency -> H.ComponentHTML Action slots m
  renderPriceBookCurrency prodMap (SS.PriceBookCurrency p) =
    HH.li_
      [ HH.dl_
          ( dataItem "Currency" (renderCurrency p.currency)
              <> opt (dataItemRaw "Rate Cards" <<< renderRateCards prodMap) p.rateCards
          )
      ]

  renderPriceBookVersion :: Map String SS.ChargeUnitMap -> SS.PriceBookVersion -> H.ComponentHTML Action slots m
  renderPriceBookVersion prodMap (SS.PriceBookVersion p) =
    HH.li_
      [ HH.dl_
          ( dataItem "Version" p.version
              <> (dataItemRaw "Currencies" $ HH.ul_ $ map (renderPriceBookCurrency prodMap) p.byCurrency)
          )
      ]

  renderPrice :: Map String SS.ChargeUnitMap -> SS.PriceBook -> H.ComponentHTML Action slots m
  renderPrice prodMap (SS.PriceBook p) =
    HH.li_
      [ HH.dl_
          ( dataItem "ID" p.id
              <> dataItem "Name" p.name
              <> opt (dataItem "Description") p.description
              <> (dataItemRaw "Versions" $ HH.ul_ $ map (renderPriceBookVersion prodMap) p.byVersion)
          )
      ]

  error err =
    [ HH.div [ HP.class_ Css.card ]
        [ HH.header_
            [ HH.h3_ [ HH.text "Error" ]
            ]
        , HH.footer_
            [ HH.text err
            ]
        ]
    ]

  idle = [ HH.p [ HP.class_ Css.landing ] [ HH.text "Idle …" ] ]

  loading = [ HH.p [ HP.class_ Css.landing ] [ HH.text "Loading …" ] ]

  defRender ::
    forall a.
    Loadable a ->
    (a -> Array (H.ComponentHTML Action slots m)) ->
    Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> idle
    ToLoad _ -> idle
    Loading -> loading
    Loaded dat -> rend dat
    Error err -> error err

  solution :: SS.Solution -> H.ComponentHTML Action slots m
  solution (SS.Solution sol) =
    HH.li_
      [ HH.dl_
          $ dataItem "ID" sol.id
          <> opt (dataItem "Name") sol.name
          <> opt (dataItem "Description") sol.description
          <> dataItemRaw "Price Books" (blockList (map (renderPrice prodMap) sol.priceBooks))
          <> dataItemRaw "Products" (blockList (map product sol.products))
      ]
    where
    prodMap =
      Map.fromFoldable
        $ map (\p@(SS.Product { sku }) -> Tuple sku (SS.productChargeUnits p))
        $ sol.products

  productCatalog (SS.ProductCatalog pc) =
    [ HH.h1_ [ HH.text (fromMaybe "Unnamed Product Catalog" pc.name) ]
    , HH.h2_ [ HH.text "Description" ]
    , HH.p_ [ HH.text $ fromMaybe "No description" pc.description ]
    , HH.h2_ [ HH.text "Solutions" ]
    , blockList <<< map solution <<< fromFoldable $ pc.solutions
    ]

  content = defRender state productCatalog

showSkuCode :: SS.Sku -> String
showSkuCode = case _ of
  SS.SkuCode c -> c
  SS.Sku s -> s.code

showChargeUnitRef :: SS.ChargeUnitRef -> String
showChargeUnitRef (SS.ChargeUnitRef unit) =
  unit.unitID
    <> (maybe "" (\p -> " [" <> showProductRef p <> "]") unit.product)

showProductRef :: SS.ProductRef -> String
showProductRef (SS.ProductRef p) = showSkuCode p.sku <> (maybe "" (\s -> " [" <> show s <> "]") p.solutionURI)

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

showCseTypeName :: SS.ConfigSchemaEntry -> String
showCseTypeName = case _ of
  SS.CseInteger _ -> "integer"
  SS.CseString _ -> "string"
  SS.CseRegex _ -> "regex"
  SS.CseConst _ -> "const"
  SS.CseArray _ -> "array"
  SS.CseObject _ -> "object"
  SS.CseOneOf _ -> "oneOf"

handleAction ::
  forall o m.
  MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  ClearState -> H.put Idle
  CheckToLoad -> do
    state <- H.get
    case state of
      ToLoad url -> loadCatalog url
      _ -> pure unit
  LoadProductCatalog url -> loadCatalog url
  where
  loadCatalog url = do
    H.modify_ \_ -> Loading
    res <- H.liftAff $ getJson url
    H.modify_ \_ -> res
