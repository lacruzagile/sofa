module App.ProductCatalog (Slot, proxy, component) where

import Prelude
import Css as Css
import Data.Array (concatMap, fromFoldable, head, length, mapMaybe, singleton, sortBy)
import Data.Loadable (Loadable(..), getJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec (productUnits)
import Data.SmartSpec as SS
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "productCatalog"
proxy = Proxy

type State
  = Loadable SS.ProductCatalog

data Action
  = LoadProductCatalog String

component ::
  forall query input output m.
  MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = Idle

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.section [ HP.classes [ Css.flex, Css.five ] ]
    [ HH.aside [ HP.classes [ Css.full, Css.fifth1000, Css.sideMenu ] ]
        [ HH.h2_ [ HH.text "Catalogs" ]
        , HH.div
            [ HP.classes [ Css.flex, Css.two, Css.three500, Css.five800, Css.one1000 ] ]
            [ HH.button
                [ HP.classes [ Css.button, Css.success ]
                , HE.onClick \_ -> LoadProductCatalog "v1alpha1/examples/product-catalog.cloud.json"
                ]
                [ HH.text "Sinch Cloud" ]
            ]
        ]
    , HH.article [ HP.classes [ Css.full, Css.fourFifth1000 ] ] content
    ]
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

    renderArray v = HH.dl_ $ dataItemRaw "Items" $ HH.dl_ $ renderInner v.items

    renderObject v = HH.div_ $ configSchema $ pure v.properties

    renderOneOf v = HH.dl_ $ concatMap renderInner $ v.oneOf

    renderInner v = dataItemRaw (showCseTypeName v) (configSchemaEntry v)

  configSchema :: Maybe (Map String SS.ConfigSchemaEntry) -> Array (H.ComponentHTML Action slots m)
  configSchema = maybe [] (html <<< HH.dl_ <<< concatMap entry <<< Map.toUnfoldable)
    where
    entry (Tuple k e) =
      [ HH.dt_ [ HH.text k, HH.text " (", HH.text $ showCseTypeName e, HH.text ")" ]
      , HH.dd_ [ configSchemaEntry e ]
      ]

    html schema =
      [ HH.dt_ [ HH.text "Configuration Schema" ]
      , HH.dd_ [ schema ]
      ]

  product :: SS.Product -> H.ComponentHTML Action slots m
  product (SS.Product p) =
    HH.li_
      [ HH.dl_ $ dataItem "SKU" p.sku
          <> opt (dataItem "Name") p.name
          <> opt (dataItem "Description") p.description
          <> opt (dataItemRaw "Attributes" <<< configValues) p.attr
          <> configSchema p.configSchema
          <> productOptions p.options
          <> opt (dataItem "Features" <<< const "TODO") p.features
          <> opt (dataItem "Variables" <<< const "TODO") p.variables
          <> dataItemRaw "Units" (specUnits p.units)
          <> opt (dataItem "Rules" <<< const "TODO") p.rules
      ]

  specUnit :: SS.SpecUnit -> H.ComponentHTML Action slots m
  specUnit (SS.SpecUnit u) =
    HH.dl_
      $ dataItem "Id" u.id
      <> opt (dataItem "Name") u.name
      <> opt (dataItem "Description") u.description
      <> dataItem "Charge Type" (show u.chargeType)
      <> opt (dataItemRaw "Price Dimension Schema" <<< configSchemaEntry) u.priceDimSchema

  specUnits :: Array SS.SpecUnit -> H.ComponentHTML Action slots m
  specUnits = blockList <<< map (HH.li_ <<< singleton <<< specUnit)

  segmentedPrice :: SS.SegmentedPrice -> H.ComponentHTML Action slots m
  segmentedPrice (SS.SegmentedPrice ps) = blockList $ map (\p -> HH.li_ [ HH.text $ showSegmentPrice p ]) ps

  configValues :: Map String SS.ConfigValue -> H.ComponentHTML Action slots m
  configValues = HH.dl_ <<< concatMap (uncurry entry) <<< Map.toUnfoldable
    where
    entry k v = dataItem k (show v)

  dimValue :: SS.DimValue -> H.ComponentHTML Action slots m
  dimValue = case _ of
    SS.DimFlat v -> HH.text (show v)
    SS.DimMap m -> configValues m

  priceByDim :: SS.PriceByDim -> H.ComponentHTML Action slots m
  priceByDim (SS.PriceByDim r) =
    HH.dl_
      $ dataItemRaw "Dimension" (dimValue r.dim)
      <> dataItem "Monthly Minimum" (show r.monthlyMinimum)
      <> dataItemRaw "Prices" (segmentedPrice r.price)

  simplePrice :: SS.SimplePrice -> H.ComponentHTML Action slots m
  simplePrice = case _ of
    SS.SimplePriceSegmented p -> segmentedPrice p
    SS.SimplePriceByDim p -> HH.ul_ $ map priceByDim p

  segmentation :: SS.PriceSegmentation -> H.ComponentHTML Action slots m
  segmentation (SS.PriceSegmentation p) =
    HH.dl_
      $ dataItem "Segment Unit" (showUnitRef p.segmentUnit)
      <> dataItem "Period" (show p.period)
      <> dataItem "Model" (show p.model)
      <> dataItemRaw "Segments" (HH.ul_ $ segment <$> p.segments)
    where
    segment s = HH.li_ [ HH.text $ showSegment s ]

  priceSegmentationByUnit :: SS.PriceSegmentationByUnit -> H.ComponentHTML Action slots m
  priceSegmentationByUnit (SS.PriceSegmentationByUnit p) =
    HH.dl_
      $ dataItem "Unit" (showUnitRef p.unit)
      <> dataItemRaw "Segmentation" (segmentation p.segmentation)

  priceSegmentationsByUnit :: Array SS.PriceSegmentationByUnit -> H.ComponentHTML Action slots m
  priceSegmentationsByUnit = HH.ul_ <<< map (HH.li_ <<< singleton <<< priceSegmentationByUnit)

  defaultPrices :: Array SS.DefaultPriceByUnit -> H.ComponentHTML Action slots m
  defaultPrices ps =
    HH.table_
      $ [ HH.tr_
            [ HH.th_ [ HH.text "Unit" ]
            , HH.th_ [ HH.text "Price" ]
            ]
        ]
      <> map mkRow ps
    where
    mkRow (SS.DefaultPriceByUnit p) =
      HH.tr_
        [ HH.td_ [ HH.text $ showUnitRef p.unit ]
        , HH.td_ [ HH.text $ show p.price ]
        ]

  pricesPerDim ::
    SS.SpecUnitMap ->
    Array SS.UnitRef ->
    Array SS.PricesPerDimByUnit ->
    H.ComponentHTML Action slots m
  pricesPerDim unitMap unitRefs ppd =
    HH.table_
      $ [ HH.tr_
            [ HH.th [ HP.colSpan $ length dims ] [ HH.text "Dimension" ]
            , HH.th [ HP.colSpan $ length units ] [ HH.text "Unit" ]
            ]
        , HH.tr_ $ map (HH.th_ <<< singleton <<< HH.text) $ dims <> unitLabels
        ]
      <> map priceRow ppd
    where
    priceRow (SS.PricesPerDimByUnit p) =
      HH.tr_
        $ map (HH.td_ <<< singleton <<< HH.text)
        $ dimVals p.dim
        <> priceVals p.prices

    units = mapMaybe (\(SS.UnitRef u) -> Map.lookup u.unitID unitMap) unitRefs

    unitLabels = SS.specUnitLabel <$> units

    -- Fatalistically assume that there is at least one unit defined, then
    -- fatalistically assume that all units use the same dimensions.
    dims = case head units of
      Just (SS.SpecUnit { priceDimSchema: Just (SS.CseObject o) }) -> fromFoldable $ Map.keys $ o.properties
      _ -> [ "" ]

    dimVals :: SS.DimValue -> Array String
    dimVals = case _ of
      SS.DimFlat v -> [ show v ]
      SS.DimMap m -> fromFoldable $ map show $ Map.values m

    showSegmentedPrice :: SS.SegmentedPrice -> String
    showSegmentedPrice (SS.SegmentedPrice p) = joinWith ", " $ map showSegmentPrice p

    priceVal :: SS.PriceByUnit -> String
    priceVal (SS.PriceByUnit { price }) = showSegmentedPrice price

    priceVals :: Array SS.PriceByUnit -> Array String
    priceVals = map priceVal <<< sortBy (comparing (_.unitID <<< unwrap <<< _.unit <<< unwrap))

  rateCardCharge :: SS.SpecUnitMap -> SS.RateCardCharge -> H.ComponentHTML Action slots m
  rateCardCharge unitMap = case _ of
    SS.RccSimple r ->
      HH.dl_ $ dataItem "Unit" (showUnitRef r.unit)
        <> dataItemRaw "Price" (simplePrice r.price)
        <> opt (dataItemRaw "Segmentation" <<< segmentation) r.segmentation
        <> opt (dataItem "Term of Price Change" <<< \n -> show n <> " days") r.termOfPriceChangeInDays
        <> opt (dataItem "Monthly Minimum" <<< show) r.monthlyMinimum
    SS.RccMixed r ->
      HH.dl_ $ optArr (dataItemRaw "Price Segmentations" <<< priceSegmentationsByUnit) r.priceSegmentations
        <> optArr (dataItemRaw "Default Prices" <<< defaultPrices) r.defaultPrices
        <> dataItemRaw "Prices per Dimension" (pricesPerDim unitMap r.units r.pricesPerDim)
        <> dataItem "Monthly Minimum" (show r.monthlyMinimum)
        <> dataItem "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
    SS.RccArray rs -> HH.ol_ <<< map (\r -> HH.li_ [ rateCardCharge unitMap r ]) $ rs

  rateCard :: SS.SpecUnitMap -> SS.RateCard -> H.ComponentHTML Action slots m
  rateCard unitMap (SS.RateCard r) =
    HH.li_ $ dataItemRaw "SKU" (renderSku r.sku)
      <> opt (dataItem "Name") r.name
      <> opt (dataItem "Description") r.description
      <> dataItemRaw "Charge" (rateCardCharge unitMap r.charge)

  rateCards :: Map String SS.SpecUnitMap -> Array SS.RateCard -> H.ComponentHTML Action slots m
  rateCards prodMap = blockList <<< map (\rc@(SS.RateCard { sku }) -> rateCard (fromMaybe Map.empty $ Map.lookup (showSkuCode sku) prodMap) rc)

  currency :: SS.Currency -> String
  currency (SS.Currency c) =
    c.code
      <> maybe "" (\c' -> "\"" <> c' <> "\"") c.country

  renderPrice :: Map String SS.SpecUnitMap -> SS.PriceBook -> H.ComponentHTML Action slots m
  renderPrice prodMap (SS.PriceBook p) =
    HH.li_
      [ HH.dl_
          ( dataItem "ID" p.id
              <> dataItem "Plan" p.plan
              <> dataItem "Version" p.version
              <> opt (dataItem "Description") p.description
              <> dataItem "Currency" (currency p.currency)
              <> opt (dataItemRaw "Rate Cards" <<< rateCards prodMap) p.rateCards
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

  idle = [ HH.p [ HP.class_ Css.landing ] [ HH.text "Select a product catalog to display…" ] ]

  loading = [ HH.p [ HP.class_ Css.landing ] [ HH.text "Loading …" ] ]

  defRender ::
    forall a.
    Loadable a ->
    (a -> Array (H.ComponentHTML Action slots m)) ->
    Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> idle
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
        $ map (\p@(SS.Product { sku }) -> Tuple sku (productUnits p))
        $ sol.products

  productCatalog (SS.ProductCatalog pc) =
    [ HH.h1_ [ HH.text (maybe "Unnamed Product Catalog" identity pc.name) ]
    , HH.h2_ [ HH.text "Description" ]
    , HH.p_ [ HH.text $ maybe "No description" identity pc.description ]
    , HH.h2_ [ HH.text "Solutions" ]
    , blockList <<< map solution <<< fromFoldable $ pc.solutions
    ]

  content = defRender state productCatalog

showSkuCode :: SS.Sku -> String
showSkuCode = case _ of
  SS.SkuCode c -> c
  SS.Sku s -> s.code

showUnitRef :: SS.UnitRef -> String
showUnitRef (SS.UnitRef unit) =
  unit.unitID
    <> (maybe "" (\p -> " [" <> showProductRef p <> "]") unit.product)

showProductRef :: SS.ProductRef -> String
showProductRef (SS.ProductRef p) = showSkuCode p.sku <> (maybe "" (\s -> " [" <> show s <> "]") p.solutionURI)

showSegmentPrice :: SS.SegmentPrice -> String
showSegmentPrice (SS.SegmentPrice p) =
  show p.price
    <> " ["
    <> show p.minimum
    <> ","
    <> maybe "" show p.exclusiveMaximum
    <> ")"

showSegment :: SS.Segment -> String
showSegment (SS.Segment s) = "[" <> show s.minimum <> "," <> maybe "" show s.exclusiveMaximum <> ")"

showCseTypeName :: SS.ConfigSchemaEntry -> String
showCseTypeName = case _ of
  SS.CseInteger _ -> "integer"
  SS.CseString _ -> "string"
  SS.CseRegex _ -> "regex"
  SS.CseArray _ -> "array"
  SS.CseObject _ -> "object"
  SS.CseOneOf _ -> "oneOf"

handleAction ::
  forall o m.
  MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  LoadProductCatalog url -> do
    H.modify_ \_ -> Loading
    res <- H.liftAff $ getJson url
    H.modify_ \_ -> res
