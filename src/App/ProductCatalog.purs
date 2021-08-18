module App.ProductCatalog (Slot, proxy, component) where

import Prelude
import Affjax (printError)
import Css as Css
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Data.Array (concatMap, fromFoldable, singleton)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.SmartSpec as SS
import Data.Tuple (uncurry)
import Data.Variant (default, on)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.Ajax (_affjaxError, _notFound, _parseError)
import Simple.Ajax as AJX
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "productCatalog"
proxy = Proxy

type State
  = SubState SS.ProductCatalog

data SubState a
  = Idle
  | Success a
  | Loading
  | Error String

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

  sku :: SS.Sku -> H.ComponentHTML Action slots m
  sku (SS.SkuCode s) =
    HH.dl [ HP.class_ Css.hblock ]
      $ dataItem "Code" s

  sku (SS.Sku s) =
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
          ( dataItemRaw "SKU" (sku po.sku)
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

  configSchemaEntry :: String -> SS.ConfigSchemaEntry -> Array (H.ComponentHTML Action slots m)
  configSchemaEntry k = case _ of
    SS.CseInteger v ->
      [ HH.dt_ [ HH.text k, HH.text " (integer)" ]
      , HH.dd_ [ renderInteger v ]
      ]
    SS.CseString v ->
      [ HH.dt_ [ HH.text k, HH.text " (string)" ]
      , HH.dd_ [ renderString v ]
      ]
    SS.CseRegex v ->
      [ HH.dt_ [ HH.text k, HH.text " (regex)" ]
      , HH.dd_ [ renderRegex v ]
      ]
    SS.CseArray v ->
      [ HH.dt_ [ HH.text k, HH.text " (array)" ]
      , HH.dd_ [ renderArray v ]
      ]
    SS.CseObject v ->
      [ HH.dt_ [ HH.text k, HH.text " (object)" ]
      , HH.dd_ [ renderObject v ]
      ]
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

    renderArray v =
      HH.dl_
        ( dataItemRaw "Items" (renderAny v.items)
        )

    renderObject v = HH.div_ $ configSchema $ pure v.properties

    renderAny = case _ of
      SS.CseInteger v -> renderInteger v
      SS.CseString v -> renderString v
      SS.CseRegex v -> renderRegex v
      SS.CseArray v -> renderArray v
      SS.CseObject v -> renderObject v

  configSchema :: Maybe (Map String SS.ConfigSchemaEntry) -> Array (H.ComponentHTML Action slots m)
  configSchema = maybe [] (html <<< HH.dl_ <<< concatMap (uncurry configSchemaEntry) <<< Map.toUnfoldable)
    where
    html x =
      [ HH.dt_ [ HH.text "Configuration Schema" ]
      , HH.dd_ [ x ]
      ]

  product :: SS.Product -> H.ComponentHTML Action slots m
  product (SS.Product p) =
    HH.li_
      [ HH.dl_
          ( dataItem "SKU" p.sku
              <> opt (dataItem "Name") p.name
              <> opt (dataItem "Description") p.description
              <> productOptions p.options
              <> opt (dataItemRaw "Attributes" <<< configValues) p.attr
              <> configSchema p.configSchema
          )
      ]

  showSkuCode :: SS.Sku -> String
  showSkuCode = case _ of
    SS.SkuCode c -> c
    SS.Sku s -> s.code

  showProductRef :: SS.ProductRef -> String
  showProductRef (SS.ProductRef p) = showSkuCode p.sku <> (maybe "" (\s -> " [" <> show s <> "]") p.solutionURI)

  unitRef :: SS.UnitRef -> String
  unitRef (SS.UnitRef unit) =
    unit.unitID
      <> (maybe "" (\p -> " [" <> showProductRef p <> "]") unit.product)

  unitRefs :: Array SS.UnitRef -> H.ComponentHTML Action slots m
  unitRefs = HH.ul_ <<< map (\u -> HH.li_ [ HH.text $ unitRef u ])

  showSegmentPrice :: SS.SegmentPrice -> String
  showSegmentPrice (SS.SegmentPrice p) =
    show p.price
      <> " ["
      <> show p.minimum
      <> ","
      <> maybe "" show p.exclusiveMaximum
      <> ")"

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

  showSegment :: SS.Segment -> String
  showSegment (SS.Segment s) = "[" <> show s.minimum <> "," <> maybe "" show s.exclusiveMaximum <> ")"

  segmentation :: SS.PriceSegmentation -> H.ComponentHTML Action slots m
  segmentation (SS.PriceSegmentation p) =
    HH.dl_
      $ dataItem "Segment Unit" (unitRef p.segmentUnit)
      <> dataItem "Period" (show p.period)
      <> dataItem "Model" (show p.model)
      <> dataItemRaw "Segments" (HH.ul_ $ segment <$> p.segments)
    where
    segment s = HH.li_ [ HH.text $ showSegment s ]

  priceSegmentationByUnit :: SS.PriceSegmentationByUnit -> H.ComponentHTML Action slots m
  priceSegmentationByUnit (SS.PriceSegmentationByUnit p) =
    HH.dl_
      $ dataItem "Unit" (unitRef p.unit)
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
        [ HH.td_ [ HH.text $ unitRef p.unit ]
        , HH.td_ [ HH.text $ show p.price ]
        ]

  priceByUnit :: SS.PriceByUnit -> H.ComponentHTML Action slots m
  priceByUnit (SS.PriceByUnit p) =
    HH.dl_
      $ dataItem "Unit" (unitRef p.unit)
      <> dataItemRaw "Price" (segmentedPrice p.price)

  pricesByUnit :: Array SS.PriceByUnit -> H.ComponentHTML Action slots m
  pricesByUnit = blockList <<< map (HH.li_ <<< singleton <<< priceByUnit)

  pricesPerDim :: Array SS.PricesPerDimByUnit -> H.ComponentHTML Action slots m
  pricesPerDim = HH.ul_ <<< map mkItem
    where
    mkItem (SS.PricesPerDimByUnit p) =
      HH.dl_
        $ dataItemRaw "Dimension" (dimValue p.dim)
        <> dataItemRaw "Prices" (pricesByUnit p.prices)
        <> dataItem "Monthly Minimum" (show p.monthlyMinimum)

  rateCardCharge :: SS.RateCardCharge -> H.ComponentHTML Action slots m
  rateCardCharge = case _ of
    SS.RccSimple r ->
      HH.dl_ $ dataItem "Unit" (unitRef r.unit)
        <> dataItemRaw "Price" (simplePrice r.price)
        <> opt (dataItemRaw "Segmentation" <<< segmentation) r.segmentation
        <> opt (dataItem "Term of Price Change" <<< \n -> show n <> " days") r.termOfPriceChangeInDays
        <> opt (dataItem "Monthly Minimum" <<< show) r.monthlyMinimum
    SS.RccMixed r ->
      HH.dl_ $ dataItemRaw "Units" (unitRefs r.units)
        <> optArr (dataItemRaw "Price Segmentations" <<< priceSegmentationsByUnit) r.priceSegmentations
        <> optArr (dataItemRaw "Default Prices" <<< defaultPrices) r.defaultPrices
        <> dataItemRaw "Prices per Dimension" (pricesPerDim r.pricesPerDim)
        <> dataItem "Monthly Minimum" (show r.monthlyMinimum)
        <> dataItem "Term of Price Change" (show r.termOfPriceChangeInDays <> " days")
    SS.RccArray rs -> HH.ol_ <<< map (\r -> HH.li_ [ rateCardCharge r ]) $ rs

  rateCard :: SS.RateCard -> H.ComponentHTML Action slots m
  rateCard (SS.RateCard r) =
    HH.li_ $ dataItemRaw "SKU" (sku r.sku)
      <> opt (dataItem "Name") r.name
      <> opt (dataItem "Description") r.description
      <> dataItemRaw "Charge" (rateCardCharge r.charge)

  rateCards :: Array SS.RateCard -> H.ComponentHTML Action slots m
  rateCards = blockList <<< map rateCard

  currency :: SS.Currency -> String
  currency (SS.Currency c) =
    c.code
      <> maybe "" (\c' -> "\"" <> c' <> "\"") c.country

  price :: SS.PriceBook -> H.ComponentHTML Action slots m
  price (SS.PriceBook p) =
    HH.li_
      [ HH.dl_
          ( dataItem "ID" p.id
              <> dataItem "Plan" p.plan
              <> dataItem "Version" p.version
              <> opt (dataItem "Description") p.description
              <> dataItem "Currency" (currency p.currency)
              <> opt (dataItemRaw "Rate Cards" <<< rateCards) p.rateCards
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

  loading = [ HH.p_ [ HH.text "Loading …" ] ]

  defRender ::
    forall a.
    SubState a ->
    (a -> Array (H.ComponentHTML Action slots m)) ->
    Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> idle
    Loading -> loading
    Success dat -> rend dat
    Error err -> error err

  solution :: SS.Solution -> H.ComponentHTML Action slots m
  solution (SS.Solution sol) =
    HH.li_
      [ HH.dl_
          $ dataItem "ID" sol.id
          <> opt (dataItem "Name") sol.name
          <> opt (dataItem "Description") sol.description
          <> dataItemRaw "Price Books" (blockList (map price sol.priceBooks))
          <> dataItemRaw "Products" (blockList (map product sol.products))
      ]

  productCatalog (SS.ProductCatalog pc) =
    [ HH.h1_ [ HH.text (maybe "Unnamed Product Catalog" identity pc.name) ]
    , HH.h2_ [ HH.text "Description" ]
    , HH.p_ [ HH.text $ maybe "No description" identity pc.description ]
    , HH.h2_ [ HH.text "Solutions" ]
    , blockList <<< map solution <<< fromFoldable $ pc.solutions
    ]

  content = defRender state productCatalog

handleAction ::
  forall o m.
  MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  LoadProductCatalog url -> do
    H.modify_ \_ -> Loading
    res <- getJson url identity
    H.modify_ \_ -> res

getJson ::
  forall m a b.
  Bind m => MonadAff m => DecodeJson a => String -> (a -> b) -> m (SubState b)
getJson url handle = do
  res <- H.liftAff (AJX.get url)
  case res of
    Left error ->
      let
        errorStr =
          default "Generic error"
            # on _affjaxError printError
            # on _notFound (const "Not found")
            # on _parseError printJsonDecodeError
            $ error
      in
        pure $ Error errorStr
    Right content -> pure $ Success $ handle content
