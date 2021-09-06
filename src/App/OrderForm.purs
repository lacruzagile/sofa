module App.OrderForm (Slot, proxy, component) where

import Prelude
import App.OrderForm.Customer as Customer
import Css as Css
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Array (deleteAt, mapWithIndex, modifyAt, snoc)
import Data.Array as A
import Data.Int as Int
import Data.Loadable (Loadable(..), getJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Route as Route
import Data.SmartSpec (ConfigSchemaEntry, ProductOption(..), skuCode, solutionProducts)
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

-- Takes as input a product catalog URL.
type Input
  = Maybe String

type Slots
  = ( customer :: Customer.Slot Unit )

type State
  = Loadable StateOrderForm

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
    , orderForm :: OrderForm
    }

-- Similar to SS.OrderForm but with a few optional fields.
type OrderForm
  = { id :: Maybe String
    , currency :: Maybe SS.Currency
    , customer :: Maybe SS.Customer
    , status :: Maybe SS.OrderStatus
    , summary :: SS.OrderSummary
    , sections :: Array (Maybe OrderSection)
    }

type OrderSection
  = { solution :: SS.Solution
    , priceBook :: Maybe SS.PriceBook
    , orderLines :: Array (Maybe OrderLine)
    -- ^ Order lines of the product options.
    , summary :: SS.OrderSectionSummary
    }

type OrderLine
  = { product :: SS.Product
    , charge :: SS.Charge
    , quantity :: Int
    , configs :: Array (Map String SS.ConfigValue)
    }

data Action
  = NoOp
  | ClearState
  | CheckToLoad
  | LoadProductCatalog String
  | SetCustomer SS.Customer
  | AddSection
  | SectionSetSolution { sectionIndex :: Int, solutionId :: String }
  | SectionSetPriceBook { sectionIndex :: Int, priceBook :: Maybe SS.PriceBook }
  | RemoveSection { sectionIndex :: Int }
  | AddOrderLine { sectionIndex :: Int }
  | OrderLineSetProduct { sectionIndex :: Int, orderLineIndex :: Int, sku :: SS.Sku }
  | OrderLineSetQuantity
    { sectionIndex :: Int, orderLineIndex :: Int, quantity :: Int
    }
  | OrderLineSetConfig
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , field :: String
    , value :: SS.ConfigValue
    }
  | RemoveOrderLine { sectionIndex :: Int, orderLineIndex :: Int }

component ::
  forall query output m.
  MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = initialize
            , receive = receive
            }
    }

initialState :: Input -> State
initialState = maybe Idle ToLoad

initialize :: Maybe Action
initialize = Just CheckToLoad

receive :: Input -> Maybe Action
receive = Just <<< maybe ClearState LoadProductCatalog

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.section [ HP.classes [ Css.flex, Css.five ] ]
    [ HH.aside [ HP.classes [ Css.full, Css.fifth1000, Css.sideMenu ] ]
        [ HH.h2_ [ HH.text "Catalogs" ]
        , HH.div
            [ HP.classes [ Css.flex, Css.two, Css.three500, Css.five800, Css.one1000 ] ]
            [ orderFormLink "Sinch Cloud" "v1alpha1/examples/product-catalog.cloud.json"
            , orderFormLink "Normalized Example" "v1alpha1/examples/product-catalog.cloud.normalized.json"
            ]
        ]
    , HH.article [ HP.classes [ Css.full, Css.fourFifth1000 ] ] content
    ]
  where
  orderFormLink txt uri =
    HH.a
      [ HP.classes [ Css.button, Css.success ]
      , Route.href $ Route.OrderForm { catalogUri: Just uri }
      ]
      [ HH.text txt ]

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

  idle = [ HH.p [ HP.class_ Css.landing ] [ HH.text "Select a product catalog to open its order form…" ] ]

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

  orderLine ::
    SS.Solution ->
    Int ->
    Int ->
    Maybe OrderLine ->
    H.ComponentHTML Action Slots m
  orderLine (SS.Solution sol) secIdx olIdx = case _ of
    Nothing ->
      body
        [ HH.label_
            [ HH.text "Product"
            , HH.select [ HE.onValueChange actionSetProduct ]
                $ [ HH.option [ HP.value "", HP.selected true ] [ HH.text "Please choose a product" ] ]
                <> products
            ]
        ]
    Just ol ->
      let
        SS.Product product = ol.product
      in
        body
          $ [ HH.div [ HP.classes [ Css.flex, Css.four ] ]
                [ HH.label [ HP.classes [ Css.full, Css.threeFourth1000 ] ]
                    [ HH.text "Product"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.disabled true
                        , HP.value product.sku
                        ]
                    ]
                , HH.label [ HP.classes [ Css.full, Css.fourth1000 ] ]
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
                ]
            ]
          <> prodConfig product
    where
    body subBody =
      HH.div [ HP.classes [ Css.orderSection ] ]
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

    prodConfig product =
      maybe []
        ( renderConfigSchema
            ( \field value ->
                OrderLineSetConfig
                  { sectionIndex: secIdx
                  , orderLineIndex: olIdx
                  , field
                  , value
                  }
            )
        )
        product.configSchema

  renderConfigSchema ::
    (String -> SS.ConfigValue -> Action) ->
    Map String ConfigSchemaEntry ->
    Array (H.ComponentHTML Action Slots m)
  renderConfigSchema onChange = A.concatMap renderEntry <<< Map.toUnfoldable
    where
    mact = maybe NoOp

    opt :: forall a b. (a -> b) -> Maybe a -> Array b
    opt f = maybe [] (\x -> [ f x ])

    renderSchemaEntry act = case _ of
      SS.CseInteger c ->
        HH.input
          $ [ HP.type_ HP.InputNumber
            , HE.onValueChange (mact (act <<< SS.CvInteger) <<< Int.fromString)
            ]
          <> opt HP.min c.minimum
          <> opt HP.max c.maximum
      SS.CseString c
        | not (A.null c.enum) ->
          let
            props e =
              [ HP.selected (Just e == c.default)
              , HE.onClick \_ -> act (SS.CvString e)
              ]
          in
            HH.select_
              $ [ HH.option_ [ HH.text $ "Please choose an option" ] ]
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
            <> opt HP.value c.default
            <> pat
      SS.CseRegex _c -> HH.input [ HE.onValueChange (act <<< SS.CvString) ]
      SS.CseConst _c -> HH.input [ HP.value "const", HP.disabled true ]
      SS.CseArray _c -> HH.input [ HP.value "Unsupported configuration type: array", HP.disabled true ]
      SS.CseObject _c -> HH.input [ HP.value "Unsupported configuration type: object", HP.disabled true ]
      SS.CseOneOf _c -> HH.input [ HP.value "Unsupported configuration type: oneOf", HP.disabled true ]

    renderEntry (Tuple key schemaEntry) =
      [ HH.label_
          [ HH.text key
          , renderSchemaEntry (onChange key) schemaEntry
          ]
      ]

  renderSection :: SS.ProductCatalog -> Maybe SS.Currency -> Int -> Maybe OrderSection -> H.ComponentHTML Action Slots m
  renderSection (SS.ProductCatalog pc) currency secIdx = case _ of
    Nothing ->
      body
        [ HH.label_
            [ HH.text "Solution"
            , HH.select [ HE.onValueChange actionSetSolution ]
                $ [ HH.option [ HP.value "", HP.selected true ] [ HH.text "Please choose a solution" ] ]
                <> solutionOptions
            ]
        ]
    Just sec ->
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
                  , HH.select_
                      $ [ HH.option [ HP.selected (isNothing sec.priceBook), HE.onClick \_ -> actionSetPriceBook Nothing ] [ HH.text "Please choose a price book" ] ]
                      <> priceBookOptions sec.priceBook sec.solution
                  ]
              ]
          ]
        <> sectionOrderLines sec.solution sec.orderLines
        <> [ HH.div [ HP.class_ Css.orderSection ]
              [ HH.button
                  [ HP.class_ Css.addOrderLine, HE.onClick \_ -> AddOrderLine { sectionIndex: secIdx } ]
                  [ HH.text "+" ]
              ]
          , renderSummary sec.summary
          ]
    where
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

    actionSetPriceBook pb =
      SectionSetPriceBook
        { sectionIndex: secIdx
        , priceBook: pb
        }

    solutionOptions =
      map
        (\(Tuple i s) -> HH.option [ HP.value i ] [ HH.text $ solutionLabel s ])
        (Map.toUnfoldable pc.solutions)

    priceBookOptions curPriceBook (SS.Solution { priceBooks }) =
      map
        ( \pb@(SS.PriceBook pb') ->
            HH.option
              [ HP.selected (Just pb'.id == map (\(SS.PriceBook x) -> x.id) curPriceBook)
              , HE.onClick \_ -> SectionSetPriceBook { sectionIndex: secIdx, priceBook: Just pb }
              ]
              [ HH.text $ pb'.id <> " (" <> pb'.version <> ")" ]
        )
        $ A.filter (\(SS.PriceBook pb) -> Just pb.currency == currency)
        $ priceBooks

    sectionOrderLines sol orderLines =
      [ HH.div_ (mapWithIndex (orderLine sol secIdx) orderLines)
      ]

    renderSummary (SS.OrderSectionSummary summary) =
      let
        price = HH.text <<< showWithCurrency currency
      in
        HH.div [ HP.classes [ Css.flex, Css.four ] ]
          [ HH.div_ [ HH.strong_ [ HH.text "Sub-totals" ] ]
          , HH.div_ [ HH.text "Estimated usage: ", price summary.estimatedUsageSubTotal ]
          , HH.div_ [ HH.text "Monthly: ", price summary.monthlySubTotal ]
          , HH.div_ [ HH.text "Onetime: ", price summary.onetimeSubTotal ]
          ]

  renderSections ::
    SS.ProductCatalog ->
    Maybe SS.Currency ->
    Array (Maybe OrderSection) ->
    H.ComponentHTML Action Slots m
  renderSections pc currency secs =
    HH.div_
      $ [ HH.h3_ [ HH.text "Sections" ]
        ]
      <> mapWithIndex (renderSection pc currency) secs
      <> [ HH.button [ HE.onClick \_ -> AddSection ] [ HH.text "Add Section" ] ]

  renderCustomer :: H.ComponentHTML Action Slots m
  renderCustomer =
    HH.div_
      [ HH.h3_ [ HH.text "Customer" ]
      , HH.slot Customer.proxy unit Customer.component unit SetCustomer
      ]

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action Slots m)
  renderOrderForm { productCatalog, orderForm } =
    [ HH.h1_ [ HH.text "Order Form" ]
    , renderCustomer
    , renderSections productCatalog orderForm.currency orderForm.sections
    , renderSummary orderForm.currency
    , HH.hr_
    , HH.label [ HP.for "of-json", HP.class_ Css.button ] [ HH.text "Order Form JSON" ]
    , Widgets.modal "of-json" "Order Form JSON"
        [ HH.pre_
            [ HH.code_ [ HH.text $ fromMaybe "Cannot produce JSON" $ toJson orderForm ]
            ]
        ]
        []
    ]
    where
    SS.OrderSummary summary = orderForm.summary

    renderSummary currency =
      let
        price = HH.text <<< showWithCurrency currency
      in
        HH.div [ HP.classes [ Css.flex, Css.four ] ]
          [ HH.div_ [ HH.strong_ [ HH.text "Totals" ] ]
          , HH.div_ [ HH.text "Estimated usage: ", price summary.estimatedUsageTotal ]
          , HH.div_ [ HH.text "Monthly: ", price summary.monthlyTotal ]
          , HH.div_ [ HH.text "Onetime: ", price summary.onetimeTotal ]
          ]

  content = defRender state renderOrderForm

showWithCurrency :: Maybe SS.Currency -> Number -> String
showWithCurrency currency amount = case currency of
  Nothing -> "N/A"
  Just (SS.Currency c) -> show amount <> " " <> c.code

toJson :: OrderForm -> Maybe String
toJson orderForm = do
  customer <- orderForm.customer
  sections <- traverse toOrderSection =<< sequence orderForm.sections
  pure $ stringifyWithIndent 2
    $ encodeJson
    $ SS.OrderForm
        { id: "ORDER-1"
        , customer
        , status: SS.OsSalesOrderNew
        , sections
        }
  where
  toOrderLine ol =
    SS.OrderLine
      { sku: SS.SkuCode $ _.sku $ unwrap $ ol.product
      , charge: ol.charge
      , quantity: ol.quantity
      , configs: ol.configs
      }

  toPriceBookRef (SS.PriceBook pb) =
    SS.PriceBookRef
      { priceBookID: pb.id
      , version: pb.version
      , currency: pb.currency
      , solutionURI: Nothing
      }

  toOrderSection :: OrderSection -> Maybe SS.OrderSection
  toOrderSection os = do
    solutionURI <- _.uri $ unwrap $ os.solution
    basePriceBook <- toPriceBookRef <$> os.priceBook
    orderLines <- sequence $ map (map toOrderLine) os.orderLines
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
          , orderForm:
              { id: Nothing
              , currency: Nothing
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
          st
            { orderForm
              { customer = Just customer
              , currency =
                case customer of
                  SS.NewCustomer cust -> case cust.commercial of
                    SS.Commercial comm -> Just comm.priceCurrency
                  _ -> Nothing
              }
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
            setPriceBook = map (\section -> section { priceBook = priceBook })
          in
            st
              { orderForm
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
                fromMaybe st.orderForm.sections (deleteAt sectionIndex st.orderForm.sections)
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
              section
                { orderLines =
                  fromMaybe section.orderLines (deleteAt orderLineIndex section.orderLines)
                }
          )
    in
      H.modify_
        $ map \st ->
            st
              { orderForm
                { sections =
                  fromMaybe st.orderForm.sections (modifyAt sectionIndex removeOrderLine st.orderForm.sections)
                }
              }
  OrderLineSetProduct { sectionIndex, orderLineIndex, sku } ->
    let
      mkOrderLine product =
        { product: product
        , charge:
            SS.ChargeSimple
              { unit: SS.UnitRef { unitID: "UID", product: Nothing }
              , price:
                  SS.SimplePriceSegmented
                    $ SS.Price
                        [ SS.SegmentPrice
                            { minimum: 0
                            , exclusiveMaximum: Nothing
                            , listPrice: 0.0
                            , salesPrice: 0.0
                            , discount: Nothing
                            }
                        ]
              , segmentation: Nothing
              , termOfPriceChangeInDays: 0
              , monthlyMinimum: 0.0
              }
        , quantity: 1
        , configs: []
        }

      updateOrderLine :: SS.Product -> Map String SS.Product -> Maybe OrderLine -> OrderLine
      updateOrderLine prod _solProds = case _ of
        Nothing -> mkOrderLine prod
        Just _ol -> mkOrderLine prod

      -- | Build order lines for all required product options.
      requiredOptions :: SS.Product -> Map String SS.Product -> Array (Maybe OrderLine)
      requiredOptions (SS.Product p) solProds =
        let
          requiredSkuCode = case _ of
            ProdOptSkuCode _ -> Nothing
            ProductOption po -> if po.required then Just (skuCode po.sku) else Nothing

          requiredProds = A.mapMaybe (requiredSkuCode >=> (\o -> Map.lookup o solProds)) <$> p.options
        in
          maybe [] (map Just <<< map mkOrderLine) requiredProds

      updateOrderSection :: OrderSection -> OrderSection
      updateOrderSection section =
        let
          solProds = solutionProducts section.solution
        in
          section
            { orderLines =
              maybe
                section.orderLines
                (\(Tuple product ls) -> ls <> requiredOptions product solProds)
                ( do
                    product <- Map.lookup (skuCode sku) solProds
                    ls <- modifyAt orderLineIndex (Just <<< updateOrderLine product solProds) section.orderLines
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
        $ map \st -> st { orderForm { sections = updateSections st.orderForm.sections } }
  OrderLineSetQuantity { sectionIndex, orderLineIndex, quantity } ->
    let
      updateQuantity :: Maybe OrderLine -> Maybe OrderLine
      updateQuantity Nothing = Nothing

      updateQuantity (Just ol) = Just $ ol { quantity = quantity }

      updateOrderLine :: OrderSection -> OrderSection
      updateOrderLine section =
        section
          { orderLines =
            fromMaybe
              section.orderLines
              (modifyAt orderLineIndex updateQuantity section.orderLines)
          }

      updateSections :: Array (Maybe OrderSection) -> Array (Maybe OrderSection)
      updateSections sections =
        fromMaybe sections
          $ modifyAt sectionIndex (map updateOrderLine)
          $ sections
    in
      H.modify_
        $ map \st -> st { orderForm { sections = updateSections st.orderForm.sections } }
  OrderLineSetConfig { sectionIndex, orderLineIndex, field, value } ->
    let
      updateValue :: OrderLine -> OrderLine
      updateValue ol = ol { configs = [ Map.singleton field value ] }

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
