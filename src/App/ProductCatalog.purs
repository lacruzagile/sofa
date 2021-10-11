module App.ProductCatalog (Slot, proxy, component) where

import Prelude
import App.Charge as Charge
import Css as Css
import Data.Array (concatMap, fromFoldable, singleton)
import Data.Loadable (Loadable(..), getJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.SmartSpec as SS
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

type Slots
  = ( charge :: Charge.Slot Unit )

type State
  = Loadable SS.ProductCatalog

data Action
  = NoOp
  | ClearState
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

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ content ]
  where
  opt :: forall a b. (a -> Array b) -> Maybe a -> Array b
  opt = maybe []

  blockList = HH.ul [ HP.class_ Css.blocklisthl ]

  dataItem label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ HH.text value ]
    ]

  dataItemRaw label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ value ]
    ]

  productOption :: SS.ProductOption -> H.ComponentHTML Action Slots m
  productOption (SS.ProdOptSkuCode s) = HH.li_ [ HH.text s ]

  productOption (SS.ProductOption po) =
    HH.li_
      [ HH.dl_
          ( dataItem "SKU" (show po.sku)
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

  productOptions :: Maybe (Array SS.ProductOption) -> Array (H.ComponentHTML Action Slots m)
  productOptions = maybe [] (html <<< blockList <<< map productOption)
    where
    html x =
      [ HH.dt_ [ HH.text "Product Options" ]
      , HH.dd_ [ x ]
      ]

  configSchemaEntry :: SS.ConfigSchemaEntry -> H.ComponentHTML Action Slots m
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

  configSchema :: Map String SS.ConfigSchemaEntry -> H.ComponentHTML Action Slots m
  configSchema = HH.dl_ <<< concatMap entry <<< Map.toUnfoldable
    where
    entry (Tuple k e) =
      [ HH.dt_ [ HH.text k, HH.text " (", HH.text $ showCseTypeName e, HH.text ")" ]
      , HH.dd_ [ configSchemaEntry e ]
      ]

  product :: SS.Product -> H.ComponentHTML Action Slots m
  product (SS.Product p) =
    HH.li_
      [ HH.dl_ $ dataItem "SKU" (show p.sku)
          <> dataItem "Product Category" (show p.productCategory)
          <> opt (dataItem "Platform" <<< show) p.platform
          <> opt (dataItem "Name") p.name
          <> opt (dataItem "Description") p.description
          <> opt (dataItemRaw "Attributes" <<< renderConfigValues) p.attr
          <> opt (dataItemRaw "Order Configuration Schema" <<< configSchema) p.orderConfigSchema
          <> opt (dataItemRaw "Asset Configuration Schema" <<< configSchemaEntry) p.assetConfigSchema
          <> productOptions p.options
          <> opt (dataItem "Features" <<< const "TODO") p.features
          <> opt (dataItem "Variables" <<< const "TODO") p.variables
          <> dataItemRaw "Units" (renderSpecUnits p.chargeUnits)
          <> opt (dataItem "Rules" <<< const "TODO") p.rules
      ]

  renderSpecUnit :: SS.ChargeUnit -> H.ComponentHTML Action Slots m
  renderSpecUnit (SS.ChargeUnit u) =
    HH.dl_
      $ dataItem "Id" u.id
      <> opt (dataItem "Name") u.name
      <> opt (dataItem "Description") u.description
      <> dataItem "Charge Type" (show u.chargeType)
      <> opt (dataItemRaw "Price Dimension Schema" <<< configSchemaEntry) u.priceDimSchema

  renderSpecUnits :: Array SS.ChargeUnit -> H.ComponentHTML Action Slots m
  renderSpecUnits = blockList <<< map (HH.li_ <<< singleton <<< renderSpecUnit)

  renderConfigValues :: Map String SS.ConfigValue -> H.ComponentHTML Action Slots m
  renderConfigValues = HH.dl_ <<< concatMap (uncurry entry) <<< Map.toUnfoldable
    where
    entry k v = dataItem k (show v)

  renderRateCardCharge :: SS.ChargeUnitMap -> SS.Charge -> H.ComponentHTML Action Slots m
  renderRateCardCharge unitMap charge =
    HH.slot Charge.proxy unit Charge.component
      { unitMap, charge, quantity: Map.empty }
      (\_ -> NoOp)

  renderRateCard :: SS.ChargeUnitMap -> SS.RateCard -> H.ComponentHTML Action Slots m
  renderRateCard unitMap (SS.RateCard r) =
    HH.li_ $ dataItem "SKU" (show r.sku)
      <> opt (dataItem "Name") r.name
      <> opt (dataItem "Description") r.description
      <> dataItemRaw "Charge" (renderRateCardCharge unitMap r.charge)

  renderRateCards :: Map SS.SkuCode SS.ChargeUnitMap -> Array SS.RateCard -> H.ComponentHTML Action Slots m
  renderRateCards prodMap = blockList <<< map (\rc@(SS.RateCard { sku }) -> renderRateCard (fromMaybe Map.empty $ Map.lookup sku prodMap) rc)

  renderPriceBookCurrency :: Map SS.SkuCode SS.ChargeUnitMap -> SS.PriceBookCurrency -> H.ComponentHTML Action Slots m
  renderPriceBookCurrency prodMap (SS.PriceBookCurrency p) =
    HH.li_
      [ HH.dl_
          ( dataItem "Currency" (show p.currency)
              <> opt (dataItemRaw "Rate Cards" <<< renderRateCards prodMap) p.rateCards
          )
      ]

  renderPriceBookVersion :: Map SS.SkuCode SS.ChargeUnitMap -> SS.PriceBookVersion -> H.ComponentHTML Action Slots m
  renderPriceBookVersion prodMap (SS.PriceBookVersion p) =
    HH.li_
      [ HH.dl_
          ( dataItem "Version" p.version
              <> (dataItemRaw "Currencies" $ HH.ul_ $ map (renderPriceBookCurrency prodMap) p.byCurrency)
          )
      ]

  renderPrice :: Map SS.SkuCode SS.ChargeUnitMap -> SS.PriceBook -> H.ComponentHTML Action Slots m
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
    (a -> Array (H.ComponentHTML Action Slots m)) ->
    Array (H.ComponentHTML Action Slots m)
  defRender s rend = case s of
    Idle -> idle
    ToLoad _ -> idle
    Loading -> loading
    Loaded dat -> rend dat
    Error err -> error err

  solution :: SS.Solution -> H.ComponentHTML Action Slots m
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
  MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  NoOp -> pure unit
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