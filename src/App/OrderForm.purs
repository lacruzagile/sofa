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
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec (skuCode, solutionProducts)
import Data.SmartSpec as SS
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Debug (spyWith)
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
  = ( charge :: Charge.Slot
    , customer :: Customer.Slot Unit )

type State
  = Loadable StateOrderForm

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
    , currency :: Maybe SS.Currency
    , priceBooks :: Map String (Array PriceBook)
    -- ^ Map from solution name to price books in the current currency.
    , orderForm :: OrderForm
    }

-- Similar to SS.OrderForm but with a few optional fields.
type OrderForm
  = { id :: Maybe String
    , customer :: Maybe SS.Customer
    , status :: Maybe SS.OrderStatus
    , summary :: SS.OrderSummary
    , sections :: Array (Maybe OrderSection)
    }

type OrderSection
  = { solution :: SS.Solution
    , priceBook :: Maybe PriceBook
    , orderLines :: Array (Maybe OrderLine)
    -- ^ Order lines of the product options.
    , summary :: SS.OrderSectionSummary
    }

type OrderLine
  = { product :: SS.Product
    , charge :: Maybe SS.Charge
    , quantity :: Int
    , configs :: Array (Map String SS.ConfigValue)
    }

-- type Charge
--   = { chargeType :: SS.ChargeType
--     , charge :: SS.Charge
--     }
type PriceBook
  = { id :: String
    , name :: String
    , version :: String
    , rateCards :: Maybe (Map String SS.RateCard) -- ^ Maybe a map from SKU to rate cards.
    }

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
  | OrderLineSetConfig
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , configIndex :: Int
    , field :: String
    , value :: SS.ConfigValue
    }
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

  renderCharge :: String -> SS.ChargeUnitMap -> SS.Charge -> H.ComponentHTML Action Slots m
  renderCharge label unitMap charge =
    HH.slot Charge.proxy label Charge.component {unitMap, charge} (\_ -> NoOp)

  renderChargeModal :: Int -> Int -> SS.ChargeUnitMap -> SS.Charge -> H.ComponentHTML Action Slots m
  renderChargeModal secIdx olIdx unitMap charge =
    HH.div_
      [ HH.label [ HP.for label, HP.class_ Css.button ] [ HH.text "Charge" ]
      , Widgets.modal label "Charge" [ renderCharge label unitMap charge ] []
      ]
    where
    label = "of-charge-" <> show secIdx <> "-" <> show olIdx

  renderOrderLine ::
    SS.Solution ->
    Int ->
    Int ->
    Maybe OrderLine ->
    H.ComponentHTML Action Slots m
  renderOrderLine (SS.Solution sol) secIdx olIdx = case _ of
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
                , HH.label [ HP.classes [ Css.full, Css.fifth1000 ] ]
                    [ HH.text "Quantity"
                    , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.min 1.0
                        , HP.value $ show ol.quantity
                        , HE.onValueChange \input ->
                            OrderLineSetQuantity
                              { sectionIndex: secIdx
                              , orderLineIndex: olIdx
                              , quantity: fromMaybe 0 $ Int.fromString input
                              }
                        ]
                    ]
                , HH.div [ HP.classes [ Css.full, Css.fifth1000 ] ]
                    [ maybe (HH.text "No charge") (renderChargeModal secIdx olIdx (SS.productChargeUnits ol.product)) ol.charge
                    ]
                ]
            ]
          <> renderProductConfigs product ol.configs
    where
    body subBody =
      HH.div [ HP.classes [ Css.orderLine ] ]
        $ [ HH.a
              [ HP.class_ Css.close
              , HE.onClick \_ -> RemoveOrderLine { sectionIndex: secIdx, orderLineIndex: olIdx }
              ]
              [ HH.text "×" ]
          ]
        <> subBody

    products = map (\(SS.Product p) -> HH.option [ HP.value p.sku ] [ HH.text p.sku ]) sol.products

    actionSetProduct sku =
      OrderLineSetProduct
        { sectionIndex: secIdx
        , orderLineIndex: olIdx
        , sku: SS.SkuCode sku
        }

    renderProductConfigs product = A.concat <<< A.mapWithIndex (renderProductConfig product)

    renderProductConfig product cfgIdx config =
      maybe []
        ( renderConfigSchema cfgIdx
            ( \field value ->
                OrderLineSetConfig
                  { sectionIndex: secIdx
                  , orderLineIndex: olIdx
                  , configIndex: cfgIdx
                  , field
                  , value
                  }
            )
            config
        )
        product.configSchema

  renderConfigSchema ::
    Int ->
    (String -> SS.ConfigValue -> Action) ->
    Map String SS.ConfigValue ->
    Map String SS.ConfigSchemaEntry ->
    Array (H.ComponentHTML Action Slots m)
  renderConfigSchema idx onChange config = wrap <<< A.concatMap renderEntry <<< Map.toUnfoldable
    where
    wrap entries =
      [ HH.fieldset_
          $ [ HH.legend_ [ HH.text $ "Instance " <> show (idx + 1) ] ]
          <> entries
      ]

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
            props e = [ HP.selected (Just e == c.default) ]

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
              [ HH.text $ pb.id <> " (" <> pb.version <> ")" ]
        )

    sectionOrderLines sol orderLines =
      [ HH.div_ (A.mapWithIndex (renderOrderLine sol secIdx) orderLines)
      ]

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

  renderOrderSectionSummary :: Maybe SS.Currency -> SS.OrderSectionSummary -> H.ComponentHTML Action Slots m
  renderOrderSectionSummary currency (SS.OrderSectionSummary summary) =
    let
      price = HH.text <<< showWithCurrency currency
    in
      HH.div [ HP.classes [ Css.flex, Css.four ] ]
        [ HH.div_ [ HH.strong_ [ HH.text "Sub-totals" ] ]
        , HH.div_ [ HH.text "Estimated usage: ", price summary.estimatedUsageSubTotal ]
        , HH.div_ [ HH.text "Monthly: ", price summary.monthlySubTotal ]
        , HH.div_ [ HH.text "Onetime: ", price summary.onetimeSubTotal ]
        ]

  renderOrderSummary :: Maybe SS.Currency -> SS.OrderSummary -> H.ComponentHTML Action Slots m
  renderOrderSummary currency (SS.OrderSummary summary) =
    let
      price = HH.text <<< showWithCurrency currency
    in
      HH.div [ HP.classes [ Css.flex, Css.four ] ]
        [ HH.div_ [ HH.strong_ [ HH.text "Totals" ] ]
        , HH.div_ [ HH.text "Estimated usage: ", price summary.estimatedUsageTotal ]
        , HH.div_ [ HH.text "Monthly: ", price summary.monthlyTotal ]
        , HH.div_ [ HH.text "Onetime: ", price summary.onetimeTotal ]
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

showWithCurrency :: Maybe SS.Currency -> Number -> String
showWithCurrency currency amount = case currency of
  Nothing -> "N/A"
  Just (SS.Currency { code: "" }) -> "N/A"
  Just (SS.Currency c) -> show amount <> " " <> c.code

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
              , summary:
                  SS.OrderSummary
                    { estimatedUsageTotal: 0.0
                    , monthlyTotal: 0.0
                    , onetimeTotal: 0.0
                    }
              , sections: []
              }
          }
      )
        <$> productCatalog
  H.modify_ \_ -> res

mkDefaultConfig :: SS.Product -> Map String SS.ConfigValue
mkDefaultConfig (SS.Product p) = maybe Map.empty mkDefaults p.configSchema
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

  updateOrderLineCharge :: Maybe PriceBook -> OrderLine -> OrderLine
  updateOrderLineCharge mpb ol =
    fromMaybe ol
      $ (\pb -> ol { charge = lookupCharge ol.product pb })
      <$> mpb

  lookupCharge :: SS.Product -> PriceBook -> Maybe SS.Charge
  lookupCharge (SS.Product product) pb = do
    rateCards <- pb.rateCards
    SS.RateCard rateCard <- Map.lookup product.sku rateCards
    pure rateCard.charge

  sumOrderLines :: Array (Maybe OrderLine) -> SS.OrderSectionSummary
  sumOrderLines = toOrderSectionSummary <<< A.foldl (\a b -> plus a (conv b)) nil

  conv :: Maybe OrderLine -> { u :: Number, m :: Number, o :: Number }
  conv Nothing = nil

  conv (Just ol) =
    let
      result = maybe nil (calcCharge ol.product) ol.charge
    in
      { u: Int.toNumber ol.quantity * result.u
      , m: Int.toNumber ol.quantity * result.m
      , o: Int.toNumber ol.quantity * result.o
      }

  plus a b = { u: a.u + b.u, m: a.m + b.m, o: a.o + b.o }

  nil = { u: 0.0, m: 0.0, o: 0.0 }

  calcCharge :: SS.Product -> SS.Charge -> { u :: Number, m :: Number, o :: Number }
  calcCharge product = case _ of
    SS.ChargeSimple c -> maybe nil (mkChargeSummary (simplePriceToAmount 1 c)) $ chargeType c.unit product
    SS.ChargeMixed c -> spyWith "Mixed charges not supported at the moment" (\_ -> c) nil
    SS.ChargeArray cs -> A.foldl (\a b -> plus a (calcCharge product b)) nil cs

  toOrderSectionSummary :: { u :: Number, m :: Number, o :: Number } -> SS.OrderSectionSummary
  toOrderSectionSummary { u, m, o } =
    SS.OrderSectionSummary
      { estimatedUsageSubTotal: u
      , monthlySubTotal: m
      , onetimeSubTotal: o
      }

  mkChargeSummary :: Number -> SS.ChargeType -> { u :: Number, m :: Number, o :: Number }
  mkChargeSummary c = case _ of
    SS.ChargeTypeOnetime -> { u: 0.0, m: 0.0, o: c }
    SS.ChargeTypeMonthly -> { u: 0.0, m: c, o: 0.0 }
    SS.ChargeTypeUsage -> { u: c, m: 0.0, o: 0.0 }

  chargeType (SS.ChargeUnitRef unit) (SS.Product { chargeUnits }) =
    map (\(SS.ChargeUnit u) -> u.chargeType)
      $ A.find (\(SS.ChargeUnit u) -> u.id == unit.unitID)
      $ chargeUnits

  priceInSegment :: Int -> Array SS.PricePerSegment -> Number
  priceInSegment q = maybe 0.0 (\(SS.PricePerSegment p) -> p.listPrice) <<< A.head <<< A.filter isInSegment
    where
    isInSegment (SS.PricePerSegment p) = p.minimum <= q && maybe true (q < _) p.exclusiveMaximum

  simplePriceToAmount q c = case c.price of
    SS.SimplePriceSegmented (SS.Price ps) -> priceInSegment q ps
    SS.SimplePriceByDim _ -> 0.0

calcTotal :: OrderForm -> OrderForm
calcTotal orderForm = orderForm { summary = SS.OrderSummary $ sumOrderSecs orderForm.sections }
  where
  sumOrderSecs = A.foldl (\a b -> plus a (conv b)) nil

  conv Nothing = nil

  conv (Just { summary: SS.OrderSectionSummary os }) =
    { estimatedUsageTotal: os.estimatedUsageSubTotal
    , monthlyTotal: os.monthlySubTotal
    , onetimeTotal: os.onetimeSubTotal
    }

  plus a b =
    { estimatedUsageTotal: a.estimatedUsageTotal + b.estimatedUsageTotal
    , monthlyTotal: a.monthlyTotal + b.monthlyTotal
    , onetimeTotal: a.onetimeTotal + b.onetimeTotal
    }

  nil =
    { estimatedUsageTotal: 0.0
    , monthlyTotal: 0.0
    , onetimeTotal: 0.0
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
                                , summary:
                                    SS.OrderSectionSummary
                                      { estimatedUsageSubTotal: 0.0
                                      , monthlySubTotal: 0.0
                                      , onetimeSubTotal: 0.0
                                      }
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
      -- Creates a new array of product configurations. The list is a truncation
      -- or extension of the old one such that its length matches the quantity
      -- variable. If the array needs to be extended then it is extended by
      -- empty configurations.
      mkConfigs product oldConfigs =
        A.take quantity oldConfigs
          <> A.replicate (quantity - A.length oldConfigs) (mkDefaultConfig product)

      updateQuantity :: OrderLine -> OrderLine
      updateQuantity ol =
        ol
          { quantity = quantity
          , configs = mkConfigs ol.product ol.configs
          }

      updateOrderLine :: OrderSection -> OrderSection
      updateOrderLine section =
        calcSubTotal
          section
            { orderLines =
              fromMaybe
                section.orderLines
                (modifyAt orderLineIndex (map updateQuantity) section.orderLines)
            }

      updateSections :: Array (Maybe OrderSection) -> Array (Maybe OrderSection)
      updateSections sections =
        fromMaybe sections
          $ modifyAt sectionIndex (map updateOrderLine)
          $ sections
    in
      H.modify_
        $ map \st -> st { orderForm = calcTotal st.orderForm { sections = updateSections st.orderForm.sections } }
  OrderLineSetConfig { sectionIndex, orderLineIndex, configIndex, field, value } ->
    let
      updateValue :: OrderLine -> OrderLine
      updateValue ol =
        ol
          { configs =
            fromMaybe [ Map.singleton field value ]
              $ A.modifyAt configIndex (Map.insert field value) ol.configs
          }

      updateOrderLine :: OrderSection -> OrderSection
      updateOrderLine section =
        section
          { orderLines =
            fromMaybe
              section.orderLines
              (modifyAt orderLineIndex (map updateValue) section.orderLines)
          }

      updateSections :: Array (Maybe OrderSection) -> Array (Maybe OrderSection)
      updateSections sections =
        fromMaybe sections
          $ modifyAt sectionIndex (map updateOrderLine)
          $ sections
    in
      H.modify_
        $ map \st -> st { orderForm { sections = updateSections st.orderForm.sections } }
