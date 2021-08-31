module App.OrderForm (Slot, proxy, component) where

import Prelude
import Css as Css
import Data.Array (deleteAt, mapWithIndex, modifyAt, snoc)
import Data.Array as A
import Data.Int as Int
import Data.Loadable (Loadable(..), getJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Route as Route
import Data.SmartSpec (ConfigSchemaEntry, ProductOption(..), skuCode, solutionProducts)
import Data.SmartSpec as SS
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

type State
  = Loadable StateOrderForm

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
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
    , orderLines :: Array (Maybe SS.OrderLine)
    -- ^ Order lines of the product options.
    , summary :: SS.OrderSectionSummary
    }

data Action
  = ClearState
  | CheckToLoad
  | LoadProductCatalog String
  | AddSection
  | SectionSetSolution { sectionIndex :: Int, solutionId :: String }
  | RemoveSection { sectionIndex :: Int }
  | AddOrderLine { sectionIndex :: Int }
  | OrderLineSetProduct { sectionIndex :: Int, orderLineIndex :: Int, sku :: SS.Sku }
  | OrderLineSetQuantity
    { sectionIndex :: Int, orderLineIndex :: Int, quantity :: Int
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

render :: forall slots m. State -> H.ComponentHTML Action slots m
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
    (a -> Array (H.ComponentHTML Action slots m)) ->
    Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> idle
    ToLoad _ -> idle
    Loading -> loading
    Loaded dat -> rend dat
    Error err -> error err

  currency legend =
    HH.fieldset_
      [ HH.legend_ [ HH.text legend ]
      , HH.div [ HP.class_ Css.two ]
          [ HH.input [ HP.type_ HP.InputText, HP.name "currency-code", HP.placeholder "Currency (e.g. EUR)" ]
          , HH.input [ HP.type_ HP.InputText, HP.name "currency-country", HP.placeholder "Country (e.g. DE)" ]
          ]
      ]

  commercial =
    [ HH.label [ HP.for "of-commercial", HP.class_ Css.button ] [ HH.text "Commercial" ]
    , Widgets.modal "of-commercial" "Commercial"
        [ HH.fieldset_
            [ HH.legend_ [ HH.text "Billing Option" ]
            , HH.label_
                [ HH.input [ HP.type_ HP.InputRadio, HP.name "billing-option", HP.id "billing-option-prepay" ]
                , HH.span [ HP.class_ Css.checkable ] [ HH.text "Prepay" ]
                ]
            , HH.label_
                [ HH.input [ HP.type_ HP.InputRadio, HP.name "billing-option", HP.id "billing-option-postpay" ]
                , HH.span [ HP.class_ Css.checkable ] [ HH.text "Postpay" ]
                ]
            ]
        , HH.fieldset_
            [ HH.legend_ [ HH.text "Contract Term" ]
            , HH.label_
                [ HH.input [ HP.type_ HP.InputRadio, HP.name "contract-term", HP.id "contract-term-ongoing" ]
                , HH.span [ HP.class_ Css.checkable ] [ HH.text "Ongoing" ]
                ]
            , HH.label_
                [ HH.input [ HP.type_ HP.InputRadio, HP.name "contract-term", HP.id "contract-term-fixed" ]
                , HH.span [ HP.class_ Css.checkable ] [ HH.text "Fixed" ]
                ]
            ]
        , currency "Payment Currency"
        , currency "Price Currency"
        ]
        [ HH.label
            [ HP.for "of-commercial", HP.classes [ Css.button ] ]
            [ HH.text "Save" ]
        , HH.label
            [ HP.for "of-commercial", HP.classes [ Css.button, Css.dangerous ] ]
            [ HH.text "Cancel" ]
        ]
    ]

  address = [ HH.fieldset_ [ HH.legend_ [ HH.text "Address" ], HH.textarea [ HP.placeholder "Address" ] ] ]

  contact name label =
    [ HH.fieldset_
        [ HH.legend_ [ HH.text label ]
        , HH.div [ HP.class_ Css.three ]
            [ HH.input [ HP.type_ HP.InputText, HP.name (name <> "-name"), HP.placeholder "Name" ]
            , HH.input [ HP.type_ HP.InputEmail, HP.name (name <> "-email"), HP.placeholder "Email" ]
            , HH.input [ HP.type_ HP.InputTel, HP.name (name <> "-phone"), HP.placeholder "Phone No" ]
            ]
        ]
    ]

  purchaser =
    [ HH.label [ HP.for "of-purchaser", HP.class_ Css.button ] [ HH.text "Purchaser" ]
    , Widgets.modal "of-purchaser" "Purchaser"
        ( [ HH.label_
              [ HH.text "Corporate Name"
              , HH.input [ HP.type_ HP.InputText, HP.name "purchaser-corp-name" ]
              ]
          , HH.label_
              [ HH.text "Country"
              , HH.input [ HP.type_ HP.InputText, HP.name "purchaser-country", HP.placeholder "DE" ]
              ]
          , HH.label_
              [ HH.text "Registration Number"
              , HH.input [ HP.type_ HP.InputText, HP.name "purchaser-regno", HP.placeholder "012345" ]
              ]
          , HH.label_
              [ HH.text "Tax ID"
              , HH.input [ HP.type_ HP.InputText, HP.name "purchaser-taxid", HP.placeholder "012345" ]
              ]
          , HH.label_
              [ HH.text "Website"
              , HH.input [ HP.type_ HP.InputText, HP.name "purchaser-website", HP.placeholder "https://example.org/" ]
              ]
          ]
            <> contact "purchaser" "Primary Contact"
            <> contact "purchaser" "Finance Contact"
            <> address
        )
        [ HH.label
            [ HP.for "of-purchaser", HP.classes [ Css.button ] ]
            [ HH.text "Save" ]
        , HH.label
            [ HP.for "of-purchaser", HP.classes [ Css.button, Css.dangerous ] ]
            [ HH.text "Cancel" ]
        ]
    ]

  seller =
    [ HH.label [ HP.for "of-seller", HP.class_ Css.button ] [ HH.text "Seller" ]
    , Widgets.modal "of-seller" "Seller"
        ( [ HH.label_
              [ HH.text "Legal Entity Name"
              , HH.input [ HP.type_ HP.InputText, HP.name "seller-name" ]
              ]
          , HH.label_
              [ HH.text "Legal Entity Country"
              , HH.input [ HP.type_ HP.InputText, HP.name "seller-country", HP.placeholder "DE" ]
              ]
          ]
            <> address
            <> contact "seller" "Primary Contract"
            <> contact "seller" "Finance Contract"
            <> contact "seller" "Support Contract"
        )
        [ HH.label
            [ HP.for "of-seller", HP.classes [ Css.button ] ]
            [ HH.text "Save" ]
        , HH.label
            [ HP.for "of-seller", HP.classes [ Css.button, Css.dangerous ] ]
            [ HH.text "Cancel" ]
        ]
    ]

  newCustomer = HH.div [ HP.class_ Css.tabBody ] (commercial <> purchaser <> seller)

  billingAccountRef =
    [ HH.label [ HP.for "of-billing-account", HP.class_ Css.button ] [ HH.text "Billing Account" ]
    , Widgets.modal "of-billing-account" "Billing Account"
        [ HH.label_
            [ HH.text "Identifier"
            , HH.input [ HP.type_ HP.InputText, HP.name "billing-account-id" ]
            ]
        ]
        [ HH.label
            [ HP.for "of-billing-account", HP.classes [ Css.button ] ]
            [ HH.text "Save" ]
        , HH.label
            [ HP.for "of-billing-account", HP.classes [ Css.button, Css.dangerous ] ]
            [ HH.text "Cancel" ]
        ]
    ]

  returnCustomer =
    HH.div [ HP.class_ Css.tabBody ]
      ( commercial <> billingAccountRef
      )

  orderLine ::
    SS.Solution ->
    Int ->
    Int ->
    Maybe SS.OrderLine ->
    H.ComponentHTML Action slots m
  orderLine solution@(SS.Solution sol) secIdx olIdx = case _ of
    Nothing ->
      body
        [ HH.label_
            [ HH.text "Product"
            , HH.select [ HE.onValueChange actionSetProduct ]
                $ [ HH.option [ HP.value "", HP.selected true ] [ HH.text "Please choose a product" ] ]
                <> products
            ]
        ]
    Just (SS.OrderLine ol) ->
      body
        $ [ HH.label_
              [ HH.text "Product"
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.disabled true
                  , HP.value (skuCode ol.sku)
                  ]
              ]
          , HH.label_
              [ HH.text "Quantity"
              , HH.input
                  [ HP.type_ HP.InputNumber
                  , HP.value $ show ol.quantity
                  , HE.onValueChange \input ->
                      OrderLineSetQuantity
                        { sectionIndex: secIdx
                        , orderLineIndex: olIdx
                        , quantity: maybe 0 identity $ Int.fromString input
                        }
                  ]
              ]
          ]
        <> prodConfig (skuCode ol.sku)
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

    prodConfig sku =
      let
        configSchema = do
          SS.Product prod <- Map.lookup sku $ solutionProducts solution
          prod.configSchema
      in
        maybe [] renderConfigSchema configSchema

  renderConfigSchema :: Map String ConfigSchemaEntry -> Array (H.ComponentHTML Action slots m)
  renderConfigSchema =
    let
      renderSchemaEntry = case _ of
        SS.CseInteger _c -> HH.input [ HP.type_ HP.InputNumber ]
        SS.CseString c
          | not (A.null c.enum) -> HH.select_ $ map (\e -> HH.option_ [ HH.text e ]) c.enum
        SS.CseString _ -> HH.input []
        SS.CseRegex _c -> HH.input []
        SS.CseConst _c -> HH.input [ HP.value "const", HP.disabled true ]
        SS.CseArray _c -> HH.input [ HP.value "Unsupported configuration type: array", HP.disabled true ]
        SS.CseObject _c -> HH.input [ HP.value "Unsupported configuration type: object", HP.disabled true ]
        SS.CseOneOf _c -> HH.input [ HP.value "Unsupported configuration type: oneOf", HP.disabled true ]

      renderEntry (Tuple key opt) =
        [ HH.label_
            [ HH.text key
            , renderSchemaEntry opt
            ]
        ]
    in
      A.concatMap renderEntry <<< Map.toUnfoldable

  section :: SS.ProductCatalog -> Int -> Maybe OrderSection -> H.ComponentHTML Action slots m
  section (SS.ProductCatalog pc) secIdx = case _ of
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
        $ [ HH.label_
              [ HH.text "Solution"
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.disabled true
                  , HP.value $ solutionLabel sec.solution
                  ]
              ]
          ]
        <> sectionOrderLines sec.solution sec.orderLines
        <> [ HH.button [ HE.onClick \_ -> AddOrderLine { sectionIndex: secIdx } ] [ HH.text "Add Order Line" ]
          , renderSummary sec.summary
          ]
    where
    body subBody =
      HH.div [ HP.classes [ Css.orderSection ] ]
        $ [ HH.a [ HP.class_ Css.close, HE.onClick \_ -> RemoveSection { sectionIndex: secIdx } ] [ HH.text "×" ]
          ]
        <> subBody

    solutionLabel (SS.Solution s) = maybe s.id identity s.name

    actionSetSolution solId =
      SectionSetSolution
        { sectionIndex: secIdx
        , solutionId: solId
        }

    solutionOptions =
      map
        (\(Tuple i s) -> HH.option [ HP.value i ] [ HH.text $ solutionLabel s ])
        (Map.toUnfoldable pc.solutions)

    sectionOrderLines sol orderLines =
      [ HH.div_ (mapWithIndex (orderLine sol secIdx) orderLines)
      ]

    renderSummary (SS.OrderSectionSummary summary) =
      HH.div [ HP.classes [ Css.flex, Css.four ] ]
        [ HH.div_ [ HH.strong_ [ HH.text "Sub-totals" ] ]
        , HH.div_ [ HH.text "Estimated usage: ", HH.text (show summary.estimatedUsageSubTotal) ]
        , HH.div_ [ HH.text "Monthly: ", HH.text (show summary.monthlySubTotal) ]
        , HH.div_ [ HH.text "Onetime: ", HH.text (show summary.onetimeSubTotal) ]
        ]

  sections :: SS.ProductCatalog -> Array (Maybe OrderSection) -> H.ComponentHTML Action slots m
  sections pc secs =
    HH.div_
      $ [ HH.h3_ [ HH.text "Sections" ]
        ]
      <> mapWithIndex (section pc) secs
      <> [ HH.button [ HE.onClick \_ -> AddSection ] [ HH.text "Add Section" ] ]

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action slots m)
  renderOrderForm { productCatalog, orderForm } =
    [ HH.h1_ [ HH.text "Order Form" ]
    , HH.div_
        [ Widgets.tabbed2 "customer"
            { label: HH.text "New Customer", content: newCustomer }
            { label: HH.text "Return Customer", content: returnCustomer }
        , sections productCatalog orderForm.sections
        , HH.div [ HP.classes [ Css.flex, Css.four ] ]
            [ HH.div_ [ HH.strong_ [ HH.text "Totals" ] ]
            , HH.div_ [ HH.text "Estimated usage: ", HH.text (show summary.estimatedUsageTotal) ]
            , HH.div_ [ HH.text "Monthly: ", HH.text (show summary.monthlyTotal) ]
            , HH.div_ [ HH.text "Onetime: ", HH.text (show summary.onetimeTotal) ]
            ]
        ]
    ]
    where
    SS.OrderSummary summary = orderForm.summary

  content = defRender state renderOrderForm

loadCatalog :: forall o m. MonadAff m => String -> H.HalogenM State Action () o m Unit
loadCatalog url = do
  H.modify_ \_ -> Loading
  productCatalog <- H.liftAff $ getJson url
  let
    res =
      ( \(pc :: SS.ProductCatalog) ->
          { productCatalog: pc
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
                  maybe st.orderForm.sections identity
                    $ do
                        solution <- Map.lookup solutionId pc.solutions
                        modifyAt sectionIndex
                          ( \s ->
                              Just
                                { solution: solution
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
  RemoveSection { sectionIndex } ->
    H.modify_
      $ map \st ->
          st
            { orderForm
              { sections =
                maybe st.orderForm.sections identity (deleteAt sectionIndex st.orderForm.sections)
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
                  maybe st.orderForm.sections identity (modifyAt sectionIndex addOrderLine st.orderForm.sections)
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
                  maybe section.orderLines identity (deleteAt orderLineIndex section.orderLines)
                }
          )
    in
      H.modify_
        $ map \st ->
            st
              { orderForm
                { sections =
                  maybe st.orderForm.sections identity (modifyAt sectionIndex removeOrderLine st.orderForm.sections)
                }
              }
  OrderLineSetProduct { sectionIndex, orderLineIndex, sku } ->
    let
      mkOrderLine prodSku =
        SS.OrderLine
          { basePriceBookRef: SS.PriceBookRef { priceBookID: "PBID", version: "", solutionURI: Nothing }
          , sku: prodSku
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
          }

      updateSku :: Maybe SS.OrderLine -> SS.OrderLine
      updateSku = case _ of
        Nothing -> mkOrderLine sku
        Just (SS.OrderLine ol) -> SS.OrderLine $ ol { sku = sku }

      -- | Add any required product options.
      requiredOptions :: OrderSection -> Array (Maybe SS.OrderLine)
      requiredOptions section =
        let
          prod = do
            SS.Product p <- Map.lookup (skuCode sku) $ solutionProducts section.solution
            p.options

          code = case _ of
            ProdOptSkuCode _ -> Nothing
            ProductOption po -> if po.required then Just po.sku else Nothing
        in
          maybe [] (map Just <<< A.mapMaybe (map mkOrderLine <<< code)) prod

      updateOrderLine :: OrderSection -> OrderSection
      updateOrderLine section =
        section
          { orderLines =
            maybe
              section.orderLines
              (\ls -> ls <> requiredOptions section)
              (modifyAt orderLineIndex (Just <<< updateSku) section.orderLines)
          }

      updateSections :: Array (Maybe OrderSection) -> Array (Maybe OrderSection)
      updateSections sections =
        maybe sections identity
          $ modifyAt sectionIndex (map updateOrderLine)
          $ sections
    in
      H.modify_
        $ map \st -> st { orderForm { sections = updateSections st.orderForm.sections } }
  OrderLineSetQuantity { sectionIndex, orderLineIndex, quantity } ->
    let
      updateQuantity :: Maybe SS.OrderLine -> Maybe SS.OrderLine
      updateQuantity Nothing = Nothing

      updateQuantity (Just (SS.OrderLine ol)) = Just $ SS.OrderLine $ ol { quantity = quantity }

      updateOrderLine :: OrderSection -> OrderSection
      updateOrderLine section =
        section
          { orderLines =
            maybe
              section.orderLines
              identity
              (modifyAt orderLineIndex updateQuantity section.orderLines)
          }

      updateSections :: Array (Maybe OrderSection) -> Array (Maybe OrderSection)
      updateSections sections =
        maybe sections identity
          $ modifyAt sectionIndex (map updateOrderLine)
          $ sections
    in
      H.modify_
        $ map \st -> st { orderForm { sections = updateSections st.orderForm.sections } }
