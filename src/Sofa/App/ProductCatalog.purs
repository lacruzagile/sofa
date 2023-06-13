module Sofa.App.ProductCatalog (Slot, proxy, component) where

import Prelude
import Data.Array (concatMap, fromFoldable, singleton)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Sofa.App.Charge (Slot, component, proxy) as Charge
import Sofa.App.Requests (getProductCatalog)
import Sofa.Css as Css
import Sofa.Data.Charge (ChargeUnitMap, productChargeUnitMap) as Charge
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.SmartSpec as SS
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
  | LoadProductCatalog

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
initialize = Just LoadProductCatalog

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ content ]
  where
  opt :: forall a b. (a -> Array b) -> Maybe a -> Array b
  opt = maybe []

  blockList = HH.ul_

  dataItem label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ HH.text value ]
    ]

  dataItemRaw label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ value ]
    ]

  productOption :: SS.ProductOption -> H.ComponentHTML Action Slots m
  productOption (SS.ProdOptSkuCode s) = HH.li_ [ HH.text $ show s ]

  productOption (SS.ProductOption po) =
    HH.li_
      [ HH.dl_
          ( dataItem "SKU" (show po.sku)
              <> opt (dataItem "Title") po.title
              <> dataItem "Required" (show po.required)
              <> dataItem "Quote line visible" (show po.quoteLineVisible)
              <> dataItem "Quantity" (show po.quantity)
              <> dataItem "Min quantity" (show po.minQuantity)
              <> dataItem "Max quantity" (show po.maxQuantity)
              <> dataItem "Selected by default" (show po.selectedByDefault)
              <> dataItem "Type" (show po.type_)
          )
      ]

  productOptions :: Maybe (Array SS.ProductOption) -> Array (H.ComponentHTML Action Slots m)
  productOptions = maybe [] (html <<< blockList <<< map productOption)
    where
    html x =
      [ HH.dt_ [ HH.text "Product options" ]
      , HH.dd_ [ x ]
      ]

  configSchemaEntry :: SS.ConfigSchemaEntry -> H.ComponentHTML Action Slots m
  configSchemaEntry = case _ of
    SS.CseBoolean v -> renderBoolean v
    SS.CseInteger v -> renderInteger v
    SS.CseString v -> renderString v
    SS.CseDate v -> renderDate v
    SS.CseRegex v -> renderRegex v
    SS.CseConst v -> renderConst v
    SS.CseArray v -> renderArray v
    SS.CseObject v -> renderObject v
    SS.CseOneOf v -> renderOneOf v
    where
    renderBoolean v = HH.dl_ (opt (dataItem "Default" <<< show) v.default)

    renderInteger v =
      HH.dl_
        ( opt (dataItem "Minimum" <<< show) v.minimum
            <> opt (dataItem "Maximum" <<< show) v.maximum
            <> opt (dataItem "Default" <<< show) v.default
        )

    renderString v =
      HH.dl_
        ( opt (dataItem "Minimum length" <<< show) v.minLength
            <> opt (dataItem "Maximum length" <<< show) v.maxLength
            <> opt (dataItem "Default") v.default
        )
    
    renderDate v =
      HH.dl_
        ( 
          opt (dataItem "Default") v.default
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

  configSchema :: FO.Object SS.ConfigSchemaEntry -> H.ComponentHTML Action Slots m
  configSchema = HH.dl_ <<< concatMap entry <<< FO.toUnfoldable
    where
    entry (Tuple k e) =
      [ HH.dt_ [ HH.text k, HH.text " (", HH.text $ showCseTypeName e, HH.text ")" ]
      , HH.dd_ [ configSchemaEntry e ]
      ]

  product :: SS.Product -> H.ComponentHTML Action Slots m
  product (SS.Product p) =
    HH.li_
      [ HH.dl_ $ dataItem "SKU" (show p.sku)
          <> opt (dataItem "Title") p.title
          <> opt (dataItem "Description") p.description
          <> opt (dataItemRaw "Attributes" <<< renderConfigValues) p.attr
          <> opt (dataItemRaw "Order configuration schema" <<< configSchemaEntry) p.orderConfigSchema
          <> opt (dataItemRaw "Asset configuration schema" <<< configSchemaEntry) p.assetConfigSchema
          <> productOptions p.options
          <> opt (dataItem "Features" <<< const "TODO") p.features
          <> dataItemRaw "Units" (renderSpecUnits p.chargeUnits)
          <> opt (dataItem "Rules" <<< const "TODO") p.rules
      ]

  renderSpecUnit :: SS.ChargeUnit -> H.ComponentHTML Action Slots m
  renderSpecUnit (SS.ChargeUnit u) =
    HH.dl_
      $ dataItem "Id" (show u.id)
      <> opt (dataItem "Title") u.title
      <> opt (dataItem "Description") u.description
      <> dataItem "Kind" (show u.kind)
      <> opt (dataItemRaw "Price dimension schema" <<< configSchemaEntry) u.priceDimSchema

  renderSpecUnits :: Array SS.ChargeUnit -> H.ComponentHTML Action Slots m
  renderSpecUnits = blockList <<< map (HH.li_ <<< singleton <<< renderSpecUnit)

  renderConfigValues :: Map String SS.ConfigValue -> H.ComponentHTML Action Slots m
  renderConfigValues = HH.dl_ <<< concatMap (uncurry entry) <<< Map.toUnfoldable
    where
    entry k v = dataItem k (show v)

  renderRateCardCharges :: SS.ChargeCurrency -> Charge.ChargeUnitMap -> Array SS.Charge -> H.ComponentHTML Action Slots m
  renderRateCardCharges defaultCurrency unitMap charges =
    HH.slot Charge.proxy unit Charge.component
      { unitMap
      , defaultCurrency
      , charges
      , estimatedUsage: Map.empty
      , priceOnly: true
      , readOnly: true
      }
      (\_ -> NoOp)

  renderRateCard :: SS.ChargeCurrency -> Charge.ChargeUnitMap -> SS.RateCard -> H.ComponentHTML Action Slots m
  renderRateCard defaultCurrency unitMap (SS.RateCard r) =
    HH.li_ $ dataItem "SKU" (show r.sku)
      <> opt (dataItem "Title") r.title
      <> opt (dataItem "Description") r.description
      <> dataItemRaw "Charges" (renderRateCardCharges defaultCurrency unitMap r.charges)

  renderRateCards :: SS.ChargeCurrency -> Map SS.SkuCode Charge.ChargeUnitMap -> Array SS.RateCard -> H.ComponentHTML Action Slots m
  renderRateCards defaultCurrency prodMap = blockList <<< map (\rc@(SS.RateCard { sku }) -> renderRateCard defaultCurrency (fromMaybe Map.empty $ Map.lookup sku prodMap) rc)

  renderPriceBookCurrency :: Map SS.SkuCode Charge.ChargeUnitMap -> SS.PriceBookCurrency -> H.ComponentHTML Action Slots m
  renderPriceBookCurrency prodMap (SS.PriceBookCurrency p) =
    let
      defaultCurrency = SS.ChargeCurrency (unwrap p.currency)
    in
      HH.li_
        [ HH.dl_
            ( dataItem "Currency" (show p.currency)
                <> opt (dataItemRaw "Rate cards" <<< renderRateCards defaultCurrency prodMap) p.rateCards
            )
        ]

  renderPriceBookVersion :: Map SS.SkuCode Charge.ChargeUnitMap -> SS.PriceBookVersion -> H.ComponentHTML Action Slots m
  renderPriceBookVersion prodMap (SS.PriceBookVersion p) =
    HH.li_
      [ HH.dl_
          ( dataItem "Version" (SS.prettyDate p.version)
              <> (dataItemRaw "Currencies" $ HH.ul_ $ map (renderPriceBookCurrency prodMap) p.byCurrency)
          )
      ]

  renderPrice :: Map SS.SkuCode Charge.ChargeUnitMap -> SS.PriceBook -> H.ComponentHTML Action Slots m
  renderPrice prodMap (SS.PriceBook p) =
    HH.li_
      [ HH.dl_
          ( dataItem "ID" p.id
              <> opt (dataItem "Title") p.title
              <> opt (dataItem "Description") p.description
              <> (dataItemRaw "Versions" $ HH.ul_ $ map (renderPriceBookVersion prodMap) p.byVersion)
          )
      ]

  error err =
    [ HH.div
        [ Css.classes
            [ "p-5"
            , "bg-red-100"
            , "border"
            , "border-red-400"
            , "text-raspberry-500"
            ]
        ]
        [ HH.h3 [ Css.class_ "text-lg" ] [ HH.text "Error" ]
        , HH.p_ [ HH.text err ]
        ]
    ]

  idle = [ HH.p_ [ HH.text "Idle …" ] ]

  loading =
    [ HH.p
        [ Css.classes [ "animate-pulse", "text-2xl" ] ]
        [ HH.text "Loading product catalog …" ]
    ]

  defRender ::
    forall a.
    Loadable a ->
    (a -> Array (H.ComponentHTML Action Slots m)) ->
    Array (H.ComponentHTML Action Slots m)
  defRender s rend = case s of
    Idle -> idle
    Loading -> loading
    Loaded dat -> rend dat
    Error err -> error err

  solution :: SS.Solution -> H.ComponentHTML Action Slots m
  solution (SS.Solution sol) =
    HH.li_
      [ HH.dl_
          $ dataItem "ID" sol.id
          <> opt (dataItem "Title") sol.title
          <> opt (dataItem "Description") sol.description
          <> dataItemRaw "Price books" (blockList (map (renderPrice prodMap) sol.priceBooks))
          <> dataItemRaw "Products" (blockList (map product sol.products))
      ]
    where
    prodMap =
      Map.fromFoldable
        $ map (\p@(SS.Product { sku }) -> Tuple sku (Charge.productChargeUnitMap p))
        $ sol.products

  productCatalog (SS.ProductCatalog pc) =
    [ HH.h1_ [ HH.text (fromMaybe "Untitled product catalog" pc.title) ]
    , HH.h2_ [ HH.text "Description" ]
    , HH.p_ [ HH.text $ fromMaybe "No description" pc.description ]
    , HH.h2_ [ HH.text "Categories" ]
    , blockList <<< map solution <<< fromFoldable $ pc.solutions
    ]

  content = defRender state productCatalog

showCseTypeName :: SS.ConfigSchemaEntry -> String
showCseTypeName = case _ of
  SS.CseBoolean _ -> "boolean"
  SS.CseDate _ -> "date"
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
  LoadProductCatalog -> do
    H.modify_ \_ -> Loading
    res <- H.liftAff getProductCatalog
    H.modify_ \_ -> res
