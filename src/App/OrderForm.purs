module App.OrderForm (Slot, proxy, component) where

import Prelude
import App.Charge as Charge
import App.OrderForm.Customer as Customer
import Control.Alternative ((<|>))
import Css as Css
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Array (modifyAt, snoc)
import Data.Array as A
import Data.Int as Int
import Data.Loadable (Loadable(..), getJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Number.Format (toStringWith, fixed)
import Data.SmartSpec (skuCode, solutionProducts)
import Data.SmartSpec as SS
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Widgets as Widgets

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "orderForm"
proxy = Proxy

type Slots
  = ( charge :: Charge.Slot OrderLineIndex
    , customer :: Customer.Slot Unit
    )

type State
  = Loadable StateOrderForm

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
    , currency :: Maybe SS.Currency
    , priceBooks :: Map String (Array PriceBook)
    -- ^ Map from solution name to price books in the current currency.
    , orderForm :: OrderForm
    }

type SubTotalEntry
  = { listPrice :: Additive Number
    , salesPrice :: Additive Number
    }

newtype SubTotal
  = SubTotal
  { usage :: SubTotalEntry -- ^ Estimated usage price.
  , monthly :: SubTotalEntry -- ^ Monthly price.
  , onetime :: SubTotalEntry -- ^ Onetime price.
  }

instance semigroupSubTotal :: Semigroup SubTotal where
  append (SubTotal a) (SubTotal b) = SubTotal $ a <> b

instance monoidSubTotal :: Monoid SubTotal where
  mempty = SubTotal { usage: mempty, monthly: mempty, onetime: mempty }

-- Similar to SS.OrderForm but with a few optional fields.
type OrderForm
  = { id :: Maybe String
    , customer :: Maybe SS.Customer
    , status :: Maybe SS.OrderStatus
    , summary :: SubTotal
    , sections :: Array (Maybe OrderSection)
    }

type OrderSection
  = { solution :: SS.Solution
    , priceBook :: Maybe PriceBook
    , orderLines :: Array (Maybe OrderLine)
    -- ^ Order lines of the product options.
    , summary :: SubTotal
    }

type OrderLine
  = { product :: SS.Product
    , charge :: Maybe SS.Charge
    , quantity :: Int
    , configs :: Array (Map String SS.ConfigValue)
    }

type PriceBook
  = { id :: String
    , name :: String
    , version :: String
    , rateCards :: Maybe (Map String SS.RateCard) -- ^ Maybe a map from SKU to rate cards.
    }

type OrderLineIndex
  = { sectionIndex :: Int, orderLineIndex :: Int }

data Action
  = NoOp
  | ClearState
  | CheckToLoad
  | LoadProductCatalog String
  | SetCustomer SS.Customer
  | AddSection
  | SectionSetSolution { sectionIndex :: Int, solutionId :: String }
  | SectionSetPriceBook { sectionIndex :: Int, priceBook :: Maybe PriceBook }
  | RemoveSection { sectionIndex :: Int }
  | AddOrderLine { sectionIndex :: Int }
  | OrderLineSetProduct { sectionIndex :: Int, orderLineIndex :: Int, sku :: SS.Sku }
  | OrderLineSetQuantity
    { sectionIndex :: Int, orderLineIndex :: Int, quantity :: Int
    }
  | OrderLineAddConfig OrderLineIndex
  | OrderLineSetConfig
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , configIndex :: Int
    , field :: String
    , value :: SS.ConfigValue
    }
  | OrderLineRemoveConfig
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , configIndex :: Int
    }
  | OrderLineSetCharge { sectionIndex :: Int, orderLineIndex :: Int, charge :: SS.Charge }
  | RemoveOrderLine { sectionIndex :: Int, orderLineIndex :: Int }

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
render state = HH.section_ [ HH.article_ renderContent ]
  where
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

  renderCharge :: OrderLineIndex -> SS.ChargeUnitMap -> SS.Charge -> H.ComponentHTML Action Slots m
  renderCharge olIdx unitMap charge =
    HH.slot Charge.proxy olIdx Charge.component
      { unitMap, charge }
      ( \c ->
          OrderLineSetCharge
            { sectionIndex: olIdx.sectionIndex
            , orderLineIndex: olIdx.orderLineIndex
            , charge: c
            }
      )

  renderChargeModal ::
    OrderLineIndex ->
    SS.ChargeUnitMap ->
    Maybe SS.Charge ->
    Array (H.ComponentHTML Action Slots m)
  renderChargeModal olIdx unitMap = maybe noCharge withCharge
    where
    chargeText = HH.text "Charge"

    noMargin :: forall r t. HP.IProp ( style :: String | r ) t
    noMargin = HP.style "margin:0"

    noCharge =
      [ HH.br_
      , HH.button [ HP.disabled true, noMargin ] [ chargeText ]
      ]

    withCharge = case _ of
      SS.ChargeArray [] -> noCharge
      charge ->
        [ HH.br_
        , HH.label
            [ HP.for modalLabel, HP.class_ Css.button, noMargin ]
            [ chargeText ]
        , Widgets.modal modalLabel "Charge"
            [ renderCharge olIdx unitMap charge ]
            []
        ]

    modalLabel = "of-charge-" <> show olIdx.sectionIndex <> "-" <> show olIdx.orderLineIndex

  renderOrderLine ::
    SS.Solution ->
    OrderLineIndex ->
    Maybe OrderLine ->
    H.ComponentHTML Action Slots m
  renderOrderLine (SS.Solution sol) olIdx = case _ of
    Nothing ->
      body
        [ HH.label_
            [ HH.text "Product"
            , HH.select [ HE.onValueChange actionSetProduct ]
                $ [ HH.option
                      [ HP.value "", HP.disabled true, HP.selected true ]
                      [ HH.text "Please choose a product" ]
                  ]
                <> products
            ]
        ]
    Just ol ->
      let
        SS.Product product = ol.product
      in
        body
          $ [ HH.div [ HP.classes [ Css.flex, Css.five ] ]
                [ HH.label [ HP.classes [ Css.full, Css.threeFifth1000 ] ]
                    [ HH.text "Product"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.disabled true
                        , HP.value product.sku
                        ]
                    ]
                , HH.label [ HP.classes [ Css.threeFifth, Css.fifth1000 ] ]
                    [ HH.text "Quantity"
                    , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.min 1.0
                        , HP.value $ show ol.quantity
                        , HE.onValueChange \input ->
                            OrderLineSetQuantity
                              { sectionIndex: olIdx.sectionIndex
                              , orderLineIndex: olIdx.orderLineIndex
                              , quantity: fromMaybe 0 $ Int.fromString input
                              }
                        ]
                    ]
                , HH.div [ HP.classes [ Css.twoFifth, Css.fifth1000 ] ]
                    $ renderChargeModal olIdx (SS.productChargeUnits ol.product) ol.charge
                ]
            ]
          <> ( if isJust product.orderConfigSchema then
                renderProductConfigs product ol.configs <> renderAddProductConfig
              else
                []
            )
    where
    body subBody =
      HH.div [ HP.classes [ Css.orderLine ] ]
        $ [ HH.a
              [ HP.class_ Css.close
              , HE.onClick \_ -> RemoveOrderLine olIdx
              ]
              [ HH.text "×" ]
          ]
        <> subBody

    products = map (\(SS.Product p) -> HH.option [ HP.value p.sku ] [ HH.text p.sku ]) sol.products

    actionSetProduct sku =
      OrderLineSetProduct
        { sectionIndex: olIdx.sectionIndex
        , orderLineIndex: olIdx.orderLineIndex
        , sku: SS.SkuCode sku
        }

    renderProductConfigs product configs =
      let
        allowRemove = A.length configs > 1
      in
        A.concat $ A.mapWithIndex (renderProductConfig allowRemove product) configs

    renderProductConfig allowRemove product cfgIdx config =
      maybe []
        ( A.singleton
            <<< renderConfigSchema allowRemove olIdx cfgIdx
                ( \field value ->
                    OrderLineSetConfig
                      { sectionIndex: olIdx.sectionIndex
                      , orderLineIndex: olIdx.orderLineIndex
                      , configIndex: cfgIdx
                      , field
                      , value
                      }
                )
                config
        )
        product.orderConfigSchema

    renderAddProductConfig =
      [ HH.div [ HP.class_ Css.orderLineConfig ]
          [ HH.button
              [ HP.class_ Css.addOrderLineConfig, HE.onClick \_ -> OrderLineAddConfig olIdx ]
              [ HH.text "+" ]
          ]
      ]

  renderConfigSchema ::
    Boolean ->
    OrderLineIndex ->
    Int ->
    (String -> SS.ConfigValue -> Action) ->
    Map String SS.ConfigValue ->
    Map String SS.ConfigSchemaEntry ->
    H.ComponentHTML Action Slots m
  renderConfigSchema allowRemove olIdx cfgIdx onChange config = wrap <<< A.concatMap renderEntry <<< Map.toUnfoldable
    where
    wrap entries =
      HH.div [ HP.classes [ Css.orderLineConfig ] ]
        $ ( if allowRemove then
              [ HH.a
                  [ HP.class_ Css.close
                  , HE.onClick \_ ->
                      OrderLineRemoveConfig
                        { sectionIndex: olIdx.sectionIndex
                        , orderLineIndex: olIdx.orderLineIndex
                        , configIndex: cfgIdx
                        }
                  ]
                  [ HH.text "×" ]
              ]
            else
              []
          )
        <> entries

    mact = maybe NoOp

    opt :: forall a b. (a -> b) -> Maybe a -> Array b
    opt f = maybe [] (\x -> [ f x ])

    renderSchemaEntry ::
      (SS.ConfigValue -> Action) ->
      Maybe SS.ConfigValue ->
      SS.ConfigSchemaEntry ->
      H.ComponentHTML Action Slots m
    renderSchemaEntry act value = case _ of
      SS.CseInteger c ->
        HH.input
          $ [ HP.type_ HP.InputNumber
            , HE.onValueChange (mact (act <<< SS.CvInteger) <<< Int.fromString)
            ]
          <> opt (HP.value <<< show) value
          <> opt (HP.min <<< Int.toNumber) c.minimum
          <> opt (HP.max <<< Int.toNumber) c.maximum
      SS.CseString c
        | not (A.null c.enum) ->
          let
            props e =
              [ HP.selected
                  ( value == Just (SS.CvString e)
                      || (value == Nothing && Just e == c.default)
                  )
              ]

            onIndexChange i = case A.index c.enum (i - 1) of
              Nothing -> NoOp
              Just s -> act $ SS.CvString s
          in
            HH.select [ HE.onSelectedIndexChange onIndexChange ]
              $ [ HH.option [ HP.disabled true ] [ HH.text $ "Please choose an option" ] ]
              <> map (\e -> HH.option (props e) [ HH.text e ]) c.enum
      SS.CseString c ->
        let
          mi = maybe "0" show c.minLength

          ma = maybe "" show c.maxLength

          pat =
            if mi == "0" && ma == "" then
              []
            else
              [ HP.pattern $ ".{" <> mi <> "," <> ma <> "}" ]
        in
          HH.input $ [ HE.onValueChange (act <<< SS.CvString) ]
            <> opt HP.value (maybe c.default (Just <<< show) value)
            <> pat
      SS.CseRegex c ->
        HH.input
          $ [ HE.onValueChange (act <<< SS.CvString)
            ]
          <> opt HP.value (maybe c.default (Just <<< show) value)
      SS.CseConst _c -> HH.input [ HP.value "const", HP.disabled true ]
      SS.CseArray _c -> HH.input [ HP.value "Unsupported configuration type: array", HP.disabled true ]
      SS.CseObject _c -> HH.input [ HP.value "Unsupported configuration type: object", HP.disabled true ]
      SS.CseOneOf _c -> HH.input [ HP.value "Unsupported configuration type: oneOf", HP.disabled true ]

    renderEntry (Tuple key schemaEntry) =
      [ HH.label_
          [ withDescription $ HH.text $ fromMaybe key $ SS.configSchemaEntryTitle schemaEntry
          , renderSchemaEntry (onChange key) (Map.lookup key config) schemaEntry
          ]
      ]
      where
      tooltip label text =
        HH.span
          [ HP.attr (H.AttrName "data-tooltip") text, HP.class_ Css.tooltipTop ]
          [ label, HH.sup_ [ HH.a_ [ HH.text "?" ] ] ]

      withDescription label = maybe label (tooltip label) $ SS.configSchemaEntryDescription schemaEntry

  renderSection ::
    StateOrderForm ->
    Int ->
    Maybe OrderSection ->
    H.ComponentHTML Action Slots m
  renderSection sof secIdx = case _ of
    Nothing ->
      body
        [ HH.label_
            [ HH.text "Solution"
            , HH.select [ HE.onValueChange actionSetSolution ]
                $ [ HH.option
                      [ HP.value "", HP.disabled true, HP.selected true ]
                      [ HH.text "Please choose a solution" ]
                  ]
                <> solutionOptions
            ]
        ]
    Just sec ->
      let
        SS.Solution sol = sec.solution

        priceBooks = fromMaybe [] $ Map.lookup sol.id sof.priceBooks

        priceBookOpts = priceBookOptions sec.priceBook priceBooks

        priceBookSel = case sof.currency of
          Nothing -> "No price currency selected"
          Just (SS.Currency { code }) ->
            if A.null priceBookOpts then
              "No price books for " <> code
            else
              "Please choose a price book"
      in
        body
          $ [ HH.div [ HP.classes [ Css.flex, Css.two ] ]
                [ HH.label_
                    [ HH.text "Solution"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.disabled true
                        , HP.value $ solutionLabel sec.solution
                        ]
                    ]
                , HH.label_
                    [ HH.text "Price Book"
                    , HH.select [ HE.onSelectedIndexChange $ actionSetPriceBook priceBooks ]
                        $ [ HH.option
                              [ HP.disabled true, HP.selected (isNothing sec.priceBook) ]
                              [ HH.text priceBookSel ]
                          ]
                        <> priceBookOpts
                    ]
                ]
            ]
          <> sectionOrderLines sec.solution sec.orderLines
          <> [ HH.div [ HP.class_ Css.orderSection ]
                [ HH.button
                    [ HP.class_ Css.addOrderLine, HE.onClick \_ -> AddOrderLine { sectionIndex: secIdx } ]
                    [ HH.text "+" ]
                ]
            , renderOrderSectionSummary sof.currency sec.summary
            ]
    where
    SS.ProductCatalog pc = sof.productCatalog

    body subBody =
      HH.div [ HP.class_ Css.orderSection ]
        $ [ HH.a [ HP.class_ Css.close, HE.onClick \_ -> RemoveSection { sectionIndex: secIdx } ] [ HH.text "×" ]
          ]
        <> subBody

    solutionLabel (SS.Solution s) = fromMaybe s.id s.name

    actionSetSolution solId =
      SectionSetSolution
        { sectionIndex: secIdx
        , solutionId: solId
        }

    actionSetPriceBook priceBooks i =
      ( \pb ->
          SectionSetPriceBook
            { sectionIndex: secIdx
            , priceBook: pb
            }
      )
        $ A.index priceBooks (i - 1)

    solutionOptions =
      map
        (\(Tuple i s) -> HH.option [ HP.value i ] [ HH.text $ solutionLabel s ])
        (Map.toUnfoldable pc.solutions)

    priceBookOptions curPriceBook =
      map
        ( \pb ->
            HH.option
              [ HP.selected (Just pb.id == map _.id curPriceBook)
              ]
              [ HH.text $ pb.name <> " (" <> pb.version <> ")" ]
        )

    sectionOrderLines sol orderLines = [ HH.div_ $ A.mapWithIndex renderOrderLine' orderLines ]
      where
      renderOrderLine' olIdx = renderOrderLine sol { sectionIndex: secIdx, orderLineIndex: olIdx }

  renderSections ::
    StateOrderForm ->
    Array (Maybe OrderSection) ->
    H.ComponentHTML Action Slots m
  renderSections sof secs =
    HH.div_
      $ [ HH.h3_ [ HH.text "Sections" ]
        ]
      <> A.mapWithIndex (renderSection sof) secs
      <> [ HH.button [ HE.onClick \_ -> AddSection ] [ HH.text "Add Section" ] ]

  renderOrderSectionSummary :: Maybe SS.Currency -> SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSectionSummary currency (SubTotal summary) =
    let
      price = renderWithCurrency currency
    in
      HH.div [ HP.classes [ Css.flex, Css.four ] ]
        [ HH.div_ [ HH.strong_ [ HH.text "Sub-totals" ] ]
        , HH.div_ [ HH.text "Estimated usage: ", price summary.usage ]
        , HH.div_ [ HH.text "Monthly: ", price summary.monthly ]
        , HH.div_ [ HH.text "Onetime: ", price summary.onetime ]
        ]

  renderOrderSummary :: Maybe SS.Currency -> SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSummary currency (SubTotal summary) =
    let
      price = renderWithCurrency currency
    in
      HH.div [ HP.classes [ Css.flex, Css.four ] ]
        [ HH.div_ [ HH.strong_ [ HH.text "Totals" ] ]
        , HH.div_ [ HH.text "Estimated usage: ", price summary.usage ]
        , HH.div_ [ HH.text "Monthly: ", price summary.monthly ]
        , HH.div_ [ HH.text "Onetime: ", price summary.onetime ]
        ]

  renderWithCurrency :: Maybe SS.Currency -> SubTotalEntry -> H.ComponentHTML Action Slots m
  renderWithCurrency currency amount = case currency of
    Nothing -> HH.text "N/A"
    Just (SS.Currency { code: "" }) -> HH.text "N/A"
    Just (SS.Currency c) ->
      if amount.listPrice == amount.salesPrice then
        HH.text $ showMonetary amount.listPrice <> " " <> c.code
      else
        Widgets.withTooltip Widgets.Top ("Without discounts: " <> showMonetary amount.listPrice)
          $ HH.span_
              [ HH.span [ HP.style "color:red" ] [ HH.text $ showMonetary amount.salesPrice ]
              , HH.text " "
              , HH.text c.code
              ]

  renderCustomer :: H.ComponentHTML Action Slots m
  renderCustomer =
    HH.div_
      [ HH.h3_ [ HH.text "Customer" ]
      , HH.slot Customer.proxy unit Customer.component unit SetCustomer
      ]

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action Slots m)
  renderOrderForm sof =
    [ HH.h1_ [ HH.text "Order Form" ]
    , renderCustomer
    , renderSections sof sof.orderForm.sections
    , renderOrderSummary sof.currency sof.orderForm.summary
    , HH.hr_
    , HH.label [ HP.for "of-json", HP.class_ Css.button ] [ HH.text "Order Form JSON" ]
    , Widgets.modal "of-json" "Order Form JSON"
        [ HH.pre_
            [ HH.code_ [ HH.text $ fromMaybe errMsg $ toJson sof.orderForm ]
            ]
        ]
        []
    ]
    where
    errMsg =
      "Cannot produce JSON. You need to select a customer\n\
           \price currency and a price book for each order section."

  renderContent = defRender state renderOrderForm

showMonetary :: Additive Number -> String
showMonetary (Additive n) = toStringWith (fixed 2) n

toJson :: OrderForm -> Maybe String
toJson orderForm = do
  customer <- orderForm.customer
  sections <- traverse toOrderSection =<< sequence orderForm.sections
  pure $ stringifyWithIndent 2
    $ encodeJson
    $ SS.OrderForm
        { id: ""
        , status: SS.OsInDraft
        , customer
        , sections
        }
  where
  toOrderLine :: OrderLine -> Maybe SS.OrderLine
  toOrderLine ol = do
    ch <- ol.charge
    pure
      $ SS.OrderLine
          { sku: SS.SkuCode $ _.sku $ unwrap $ ol.product
          , charge: ch
          , quantity: ol.quantity
          , configs: ol.configs
          }

  toPriceBookRef pb =
    SS.PriceBookRef
      { priceBookID: pb.id
      , version: pb.version
      , solutionURI: Nothing
      }

  toOrderSection :: OrderSection -> Maybe SS.OrderSection
  toOrderSection os = do
    solutionURI <- _.uri $ unwrap $ os.solution
    basePriceBook <- toPriceBookRef <$> os.priceBook
    orderLines <- sequence $ map (toOrderLine =<< _) os.orderLines
    pure $ SS.OrderSection { solutionURI, basePriceBook, orderLines }

loadCatalog ::
  forall slots output m.
  MonadAff m =>
  String ->
  H.HalogenM State Action slots output m Unit
loadCatalog url = do
  H.modify_ \_ -> Loading
  productCatalog <- H.liftAff $ getJson url
  let
    res =
      ( \(pc :: SS.ProductCatalog) ->
          { productCatalog: pc
          , currency: Nothing
          , priceBooks: Map.empty
          , orderForm:
              { id: Nothing
              , customer: Nothing
              , status: Nothing
              , summary: mempty
              , sections: []
              }
          }
      )
        <$> productCatalog
  H.modify_ \_ -> res

mkDefaultConfig :: SS.Product -> Map String SS.ConfigValue
mkDefaultConfig (SS.Product p) = maybe Map.empty mkDefaults p.orderConfigSchema
  where
  mkDefaults = Map.mapMaybe mkDefault

  mkDefault = case _ of
    SS.CseInteger x -> SS.CvInteger <$> x.default
    SS.CseString x -> SS.CvString <$> (x.default <|> A.head x.enum)
    SS.CseRegex x -> SS.CvString <$> x.default
    SS.CseConst x -> Just x.const
    SS.CseArray _ -> Just $ SS.CvArray []
    SS.CseObject _ -> Nothing
    SS.CseOneOf _ -> Nothing

calcSubTotal :: OrderSection -> OrderSection
calcSubTotal os =
  os
    { orderLines = orderLines'
    , summary = sumOrderLines orderLines'
    }
  where
  orderLines' = map (map (updateOrderLineCharge os.priceBook)) os.orderLines

  -- | Sets the order line charge from the given price book. If the order line
  -- | already has a charge, then it is returned unchanged.
  updateOrderLineCharge :: Maybe PriceBook -> OrderLine -> OrderLine
  updateOrderLineCharge mpb ol
    | isJust ol.charge = ol
    | otherwise =
      fromMaybe ol
        $ (\pb -> ol { charge = lookupCharge ol.product pb })
        <$> mpb

  lookupCharge :: SS.Product -> PriceBook -> Maybe SS.Charge
  lookupCharge (SS.Product product) pb = do
    rateCards <- pb.rateCards
    SS.RateCard rateCard <- Map.lookup product.sku rateCards
    pure rateCard.charge

  sumOrderLines :: Array (Maybe OrderLine) -> SubTotal
  sumOrderLines = A.foldl (\a b -> a <> (conv b)) mempty

  conv :: Maybe OrderLine -> SubTotal
  conv Nothing = mempty

  conv (Just ol) =
    let
      (SubTotal result) = maybe mempty (calcCharge ol.product) ol.charge

      scale = map (Int.toNumber ol.quantity * _)
    in
      SubTotal
        { usage:
            { listPrice: scale result.usage.listPrice
            , salesPrice: scale result.usage.salesPrice
            }
        , monthly:
            { listPrice: scale result.monthly.listPrice
            , salesPrice: scale result.monthly.salesPrice
            }
        , onetime:
            { listPrice: scale result.onetime.listPrice
            , salesPrice: scale result.onetime.salesPrice
            }
        }

  calcCharge :: SS.Product -> SS.Charge -> SubTotal
  calcCharge product (SS.ChargeArray cs) = A.foldl (\a b -> a <> calcChargeElem b) mempty cs
    where
    priceToAmount (SS.PriceByUnit p) =
      maybe mempty (mkChargeSummary (priceInSegment 1 p.price))
        $ chargeType p.unit product

    priceByUnitToAmount (SS.PriceByUnitPerDim p) = A.foldl (\a b -> a <> priceToAmount b) mempty p.prices

    calcChargeElem (SS.ChargeElement c) = A.foldl (\a b -> a <> priceByUnitToAmount b) mempty c.priceByUnitByDim

  mkChargeSummary :: SubTotalEntry -> SS.ChargeType -> SubTotal
  mkChargeSummary c = case _ of
    SS.ChargeTypeOnetime -> SubTotal { usage: mempty, monthly: mempty, onetime: c }
    SS.ChargeTypeMonthly -> SubTotal { usage: mempty, monthly: c, onetime: mempty }
    SS.ChargeTypeUsage -> SubTotal { usage: c, monthly: mempty, onetime: mempty }

  chargeType (SS.ChargeUnitRef unit) (SS.Product { chargeUnits }) =
    map (\(SS.ChargeUnit u) -> u.chargeType)
      $ A.find (\(SS.ChargeUnit u) -> u.id == unit.unitID)
      $ chargeUnits

  priceInSegment :: Int -> SS.Price -> SubTotalEntry
  priceInSegment q (SS.Price segments) = maybe mempty getValue $ A.head $ A.filter isInSegment $ segments
    where
    getValue (SS.PricePerSegment p) =
      maybe { listPrice: Additive p.listPrice, salesPrice: Additive p.listPrice }
        (\sp -> { listPrice: Additive p.listPrice, salesPrice: Additive sp })
        p.salesPrice

    isInSegment (SS.PricePerSegment p) = p.minimum <= q && maybe true (q < _) p.exclusiveMaximum

calcTotal :: OrderForm -> OrderForm
calcTotal orderForm = orderForm { summary = SubTotal $ sumOrderSecs orderForm.sections }
  where
  sumOrderSecs = A.foldl (\a b -> a <> (conv b)) mempty

  conv = maybe mempty (\{ summary: SubTotal os } -> os)

-- | Helper function to modify an indexed order line.
modifyOrderLine :: Int -> Int -> StateOrderForm -> (OrderLine -> OrderLine) -> StateOrderForm
modifyOrderLine secIdx olIdx state updateOrderLine =
  state
    { orderForm = updateOrderForm state.orderForm
    }
  where
  updateOrderForm :: OrderForm -> OrderForm
  updateOrderForm orderForm =
    calcTotal
      $ orderForm { sections = updateSections orderForm.sections }

  updateSections :: Array (Maybe OrderSection) -> Array (Maybe OrderSection)
  updateSections sections =
    fromMaybe sections
      $ modifyAt secIdx (map updateOrderLines)
      $ sections

  updateOrderLines :: OrderSection -> OrderSection
  updateOrderLines section =
    calcSubTotal
      $ section
          { orderLines =
            fromMaybe section.orderLines
              $ modifyAt olIdx (map updateOrderLine) section.orderLines
          }

handleAction ::
  forall slots output m.
  MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ClearState -> H.put Idle
  CheckToLoad -> do
    state <- H.get
    case state of
      ToLoad url -> loadCatalog url
      _ -> pure unit
  LoadProductCatalog url -> loadCatalog url
  SetCustomer customer ->
    H.modify_
      $ map \st ->
          let
            currency = case customer of
              SS.NewCustomer cust -> case cust.commercial of
                SS.Commercial comm -> Just comm.priceCurrency
              _ -> Nothing

            SS.ProductCatalog pc = st.productCatalog

            rateCardMap = Map.fromFoldable <<< map (\rc@(SS.RateCard rc') -> Tuple (skuCode rc'.sku) rc)

            mkPriceBooks c = do
              SS.Solution sol <- A.fromFoldable $ Map.values pc.solutions
              SS.PriceBook pb <- sol.priceBooks
              SS.PriceBookVersion pbv <- pb.byVersion
              SS.PriceBookCurrency pbc <- pbv.byCurrency
              if pbc.currency == c then
                [ Tuple sol.id
                    [ { id: pb.id
                      , name: pb.name
                      , version: pbv.version
                      , rateCards: rateCardMap <$> pbc.rateCards
                      }
                    ]
                ]
              else
                []

            priceBooks = maybe Map.empty (Map.fromFoldableWith (<>) <<< mkPriceBooks) currency
          in
            st
              { currency = currency
              , priceBooks = priceBooks
              , orderForm = st.orderForm { customer = Just customer }
              }
  AddSection ->
    H.modify_
      $ map \st ->
          st { orderForm { sections = snoc st.orderForm.sections Nothing } }
  SectionSetSolution { sectionIndex, solutionId } ->
    H.modify_
      $ map \st ->
          let
            SS.ProductCatalog pc = st.productCatalog
          in
            st
              { orderForm
                { sections =
                  fromMaybe st.orderForm.sections
                    $ do
                        solution <- Map.lookup solutionId pc.solutions
                        modifyAt sectionIndex
                          ( \_ ->
                              Just
                                { solution: solution
                                , priceBook: Nothing
                                , orderLines: [ Nothing ]
                                , summary: mempty
                                }
                          )
                          st.orderForm.sections
                }
              }
  SectionSetPriceBook { sectionIndex, priceBook } ->
    H.modify_
      $ map \st ->
          let
            setPriceBook :: Maybe OrderSection -> Maybe OrderSection
            setPriceBook = map (\section -> calcSubTotal section { priceBook = priceBook })
          in
            st
              { orderForm =
                calcTotal
                  st.orderForm
                    { sections =
                      fromMaybe st.orderForm.sections
                        $ modifyAt sectionIndex setPriceBook st.orderForm.sections
                    }
              }
  RemoveSection { sectionIndex } ->
    H.modify_
      $ map \st ->
          st
            { orderForm
              { sections =
                fromMaybe st.orderForm.sections
                  $ A.deleteAt sectionIndex st.orderForm.sections
              }
            }
  AddOrderLine { sectionIndex } ->
    let
      addOrderLine :: Maybe OrderSection -> Maybe OrderSection
      addOrderLine = map (\section -> section { orderLines = snoc section.orderLines Nothing })
    in
      H.modify_
        $ map \st ->
            st
              { orderForm
                { sections =
                  fromMaybe st.orderForm.sections (modifyAt sectionIndex addOrderLine st.orderForm.sections)
                }
              }
  RemoveOrderLine { sectionIndex, orderLineIndex } ->
    let
      removeOrderLine :: Maybe OrderSection -> Maybe OrderSection
      removeOrderLine =
        map
          ( \section ->
              calcSubTotal
                section
                  { orderLines = fromMaybe section.orderLines $ A.deleteAt orderLineIndex section.orderLines
                  }
          )
    in
      H.modify_
        $ map \st ->
            st
              { orderForm =
                calcTotal
                  st.orderForm
                    { sections =
                      fromMaybe st.orderForm.sections (modifyAt sectionIndex removeOrderLine st.orderForm.sections)
                    }
              }
  OrderLineSetProduct { sectionIndex, orderLineIndex, sku } ->
    let
      mkOrderLine :: SS.Product -> OrderLine
      mkOrderLine product =
        { product
        , charge: Nothing
        , quantity: 1
        , configs: [ mkDefaultConfig product ]
        }

      updateOrderLine :: SS.Product -> Maybe OrderLine -> OrderLine
      updateOrderLine product = case _ of
        Nothing -> mkOrderLine product
        Just _ol -> mkOrderLine product

      -- | Build order lines for all required product options.
      requiredOptions :: SS.Product -> Map String SS.Product -> Array (Maybe OrderLine)
      requiredOptions (SS.Product p) solProds =
        let
          requiredSkuCode = case _ of
            SS.ProdOptSkuCode _ -> Nothing
            SS.ProductOption po -> if po.required then Just (skuCode po.sku) else Nothing

          requiredProds = A.mapMaybe (requiredSkuCode >=> (\o -> Map.lookup o solProds)) <$> p.options
        in
          maybe [] (map (Just <<< mkOrderLine)) requiredProds

      updateOrderSection :: OrderSection -> OrderSection
      updateOrderSection section =
        let
          solProds = solutionProducts section.solution
        in
          calcSubTotal
            section
              { orderLines =
                maybe
                  section.orderLines
                  (\(Tuple product ls) -> ls <> requiredOptions product solProds)
                  ( do
                      product <- Map.lookup (skuCode sku) solProds
                      ls <- modifyAt orderLineIndex (Just <<< updateOrderLine product) section.orderLines
                      pure $ Tuple product ls
                  )
              }

      updateSections :: Array (Maybe OrderSection) -> Array (Maybe OrderSection)
      updateSections sections =
        fromMaybe sections
          $ modifyAt sectionIndex (map updateOrderSection)
          $ sections
    in
      H.modify_
        $ map \st -> st { orderForm = calcTotal st.orderForm { sections = updateSections st.orderForm.sections } }
  OrderLineSetQuantity { sectionIndex, orderLineIndex, quantity } ->
    let
      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol = ol { quantity = quantity }
    in
      H.modify_ $ map \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineAddConfig { sectionIndex, orderLineIndex } ->
    let
      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol = ol { configs = ol.configs <> [ mkDefaultConfig ol.product ] }
    in
      H.modify_ $ map \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineRemoveConfig { sectionIndex, orderLineIndex, configIndex } ->
    let
      -- | Remove the configuration entry. If this is the last entry then we
      -- | ignore the request.
      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol =
        ol
          { configs =
            if A.length ol.configs == 1 then
              ol.configs
            else
              fromMaybe ol.configs $ A.deleteAt configIndex ol.configs
          }
    in
      H.modify_ $ map \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineSetConfig { sectionIndex, orderLineIndex, configIndex, field, value } ->
    let
      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol =
        ol
          { configs =
            fromMaybe [ Map.singleton field value ]
              $ A.modifyAt configIndex (Map.insert field value) ol.configs
          }
    in
      H.modify_
        $ map \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineSetCharge { sectionIndex, orderLineIndex, charge } ->
    H.modify_
      $ map \st -> modifyOrderLine sectionIndex orderLineIndex st (_ { charge = Just charge })
