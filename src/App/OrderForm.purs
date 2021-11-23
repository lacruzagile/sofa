module App.OrderForm (Slot, proxy, component) where

import Prelude
import App.Charge (Slot, component, proxy) as Charge
import App.OrderForm.Customer as Customer
import App.Requests (getProductCatalog, postOrder)
import Control.Alternative ((<|>))
import Css as Css
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Array (foldl, head, modifyAt, snoc)
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Charge (ChargeUnitMap, dims, productChargeUnitMap, unitIds) as Charge
import Data.Either (Either(..))
import Data.Int as Int
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Loadable (Loadable(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Quantity (QuantityMap, Quantity, fromSmartSpecQuantity, toSmartSpecQuantity)
import Data.SmartSpec as SS
import Data.SubTotal (SubTotal)
import Data.SubTotal as SubTotal
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

type Input
  = Maybe SS.OrderForm

data State
  = Initializing SS.OrderForm
  | Initialized (Loadable StateOrderForm)

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
    , currency :: Maybe SS.PricingCurrency
    , priceBooks :: Map String (Array PriceBook)
    -- ^ Map from solution title to price books in the current currency.
    , orderForm :: OrderForm
    }

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
  = { orderLineId :: Maybe String
    , status :: SS.OrderLineStatus
    , product :: SS.Product
    , charges :: Maybe (Array SS.Charge)
    , unitMap :: Charge.ChargeUnitMap
    , configs :: Array SS.OrderLineConfig
    , estimatedUsage :: QuantityMap
    }

type PriceBook
  = { id :: String
    , title :: String
    , version :: String
    , currency :: SS.ChargeCurrency -- ^ The default charge currency.
    , rateCards :: Maybe (Map SS.SkuCode SS.RateCard) -- ^ Maybe a map from SKU to rate cards.
    }

type OrderLineIndex
  = { sectionIndex :: Int, orderLineIndex :: Int }

data Action
  = NoOp
  | Initialize
  | SetCustomer SS.Customer
  | AddSection
  | SectionSetSolution { sectionIndex :: Int, solutionId :: String }
  | SectionSetPriceBook { sectionIndex :: Int, priceBook :: Maybe PriceBook }
  | RemoveSection { sectionIndex :: Int }
  | AddOrderLine { sectionIndex :: Int }
  | OrderLineSetProduct { sectionIndex :: Int, orderLineIndex :: Int, sku :: SS.SkuCode }
  | OrderLineSetQuantity
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , configIndex :: Int
    , quantity :: Int
    }
  | OrderLineAddConfig OrderLineIndex
  | OrderLineSetConfig
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , configIndex :: Int
    , alter :: Maybe SS.ConfigValue -> SS.ConfigValue
    }
  | OrderLineRemoveConfig
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , configIndex :: Int
    }
  | OrderLineSetCharges
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , charges :: Array SS.Charge
    , estimatedUsage :: QuantityMap
    }
  | RemoveOrderLine { sectionIndex :: Int, orderLineIndex :: Int }
  | PostOrder

component ::
  forall query output m.
  MonadAff m =>
  CredentialStore m =>
  H.Component query Input output m
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

initialState :: Input -> State
initialState = maybe (Initialized Idle) Initializing

initialize :: Maybe Action
initialize = Just Initialize

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ renderContent ]
  where
  renderCharges ::
    OrderLineIndex ->
    Charge.ChargeUnitMap ->
    SS.ChargeCurrency ->
    QuantityMap ->
    Array SS.Charge ->
    H.ComponentHTML Action Slots m
  renderCharges olIdx unitMap defaultCurrency estimatedUsage charges =
    HH.slot Charge.proxy olIdx Charge.component
      { unitMap, defaultCurrency, charges, estimatedUsage }
      ( \result ->
          OrderLineSetCharges
            { sectionIndex: olIdx.sectionIndex
            , orderLineIndex: olIdx.orderLineIndex
            , charges: result.charges
            , estimatedUsage: result.estimatedUsage
            }
      )

  renderChargeModal ::
    OrderLineIndex ->
    Charge.ChargeUnitMap ->
    SS.ChargeCurrency ->
    QuantityMap ->
    Maybe (Array SS.Charge) ->
    Array (H.ComponentHTML Action Slots m)
  renderChargeModal olIdx unitMap defaultCurrency estimatedUsage = maybe noCharges withCharges
    where
    chargeText = HH.text "Charge"

    noMargin :: forall r t. HP.IProp ( style :: String | r ) t
    noMargin = HP.style "margin:0"

    noCharges =
      [ HH.br_
      , HH.button [ HP.disabled true, noMargin ] [ chargeText ]
      ]

    withCharges = case _ of
      [] -> noCharges
      charges ->
        [ HH.br_
        , HH.label
            [ HP.for modalLabel, HP.class_ Css.button, noMargin ]
            [ chargeText ]
        , Widgets.modal modalLabel "Charge"
            [ renderCharges olIdx unitMap defaultCurrency estimatedUsage charges ]
            []
        ]

    modalLabel = "of-charge-" <> show olIdx.sectionIndex <> "-" <> show olIdx.orderLineIndex

  renderOrderLine ::
    SS.Solution ->
    SS.ChargeCurrency ->
    OrderLineIndex ->
    Maybe OrderLine ->
    H.ComponentHTML Action Slots m
  renderOrderLine (SS.Solution sol) defaultCurrency olIdx = case _ of
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
                [ HH.label [ HP.classes [ Css.full, Css.fourFifth1000 ] ]
                    [ HH.text "Product"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.disabled true
                        , HP.value (show product.sku)
                        ]
                    ]
                , HH.div [ HP.classes [ Css.full, Css.fifth1000 ] ]
                    $ renderChargeModal olIdx ol.unitMap defaultCurrency ol.estimatedUsage ol.charges
                ]
            ]
          <> ( if isJust product.orderConfigSchema then
                renderProductConfigs product ol.configs <> renderAddProductConfig
              else
                renderQuantity 0 $ fromMaybe (SS.OrderLineConfig { quantity: 0, config: Nothing }) $ head ol.configs
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

    products =
      map
        ( \(SS.Product p) ->
            let
              sku = show p.sku
            in
              HH.option [ HP.value sku ] [ HH.text sku ]
        )
        sol.products

    actionSetProduct sku =
      OrderLineSetProduct
        { sectionIndex: olIdx.sectionIndex
        , orderLineIndex: olIdx.orderLineIndex
        , sku: SS.SkuCode sku
        }

    renderQuantity cfgIdx (SS.OrderLineConfig olc) =
      [ HH.div_
          [ HH.label_ [ HH.text "Quantity" ]
          , HH.input
              [ HP.type_ HP.InputNumber
              , HP.min 1.0
              , HP.value $ show olc.quantity
              , HE.onValueChange
                  $ \v ->
                      OrderLineSetQuantity
                        { sectionIndex: olIdx.sectionIndex
                        , orderLineIndex: olIdx.orderLineIndex
                        , configIndex: cfgIdx
                        , quantity: fromMaybe olc.quantity $ Int.fromString v
                        }
              ]
          ]
      ]

    renderProductConfigs product configs =
      let
        allowRemove = A.length configs > 1
      in
        A.concat (A.mapWithIndex (renderProductConfig allowRemove product) configs)

    renderProductConfig allowRemove product cfgIdx olc@(SS.OrderLineConfig { config }) =
      [ HH.div [ HP.classes [ Css.orderLineConfig ] ]
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
          <> renderQuantity cfgIdx olc
          <> case config of
              Nothing -> []
              Just c ->
                [ HH.fieldset_ $ [ HH.legend_ [ HH.text "Configuration" ] ]
                    <> maybe []
                        ( A.singleton
                            <<< renderConfigSchema
                                ( \alter ->
                                    OrderLineSetConfig
                                      { sectionIndex: olIdx.sectionIndex
                                      , orderLineIndex: olIdx.orderLineIndex
                                      , configIndex: cfgIdx
                                      , alter
                                      }
                                )
                                c
                        )
                        product.orderConfigSchema
                ]
      ]

    renderAddProductConfig =
      [ HH.div [ HP.class_ Css.orderLineConfig ]
          [ HH.button
              [ HP.class_ Css.addOrderLineConfig, HE.onClick \_ -> OrderLineAddConfig olIdx ]
              [ HH.text "+" ]
          ]
      ]

  renderConfigSchema ::
    ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
    SS.ConfigValue ->
    SS.ConfigSchemaEntry ->
    H.ComponentHTML Action Slots m
  renderConfigSchema onChange config = renderEntry onChange "Configuration" (Just config)
    where
    mact :: forall a. (a -> Action) -> Maybe a -> Action
    mact = maybe NoOp

    opt :: forall a b. (a -> b) -> Maybe a -> Array b
    opt f = maybe [] (\x -> [ f x ])

    renderEntry ::
      ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
      String ->
      Maybe SS.ConfigValue ->
      SS.ConfigSchemaEntry ->
      H.ComponentHTML Action Slots m
    renderEntry act fallbackTitle value schemaEntry = case schemaEntry of
      SS.CseInteger c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input
          $ [ HP.type_ HP.InputNumber
            , HE.onValueChange (mact (act <<< const <<< SS.CvInteger) <<< Int.fromString)
            ]
          <> opt (HP.value <<< show) value
          <> opt (HP.min <<< Int.toNumber) c.minimum
          <> opt (HP.max <<< Int.toNumber) c.maximum
      SS.CseString c
        | not (A.null c.enum) ->
          renderEntry' fallbackTitle schemaEntry
            $ let
                props e =
                  [ HP.selected
                      ( value == Just (SS.CvString e)
                          || (value == Nothing && Just e == c.default)
                      )
                  ]

                onIndexChange i = mact (act <<< const <<< SS.CvString) $ A.index c.enum (i - 1)
              in
                HH.select [ HE.onSelectedIndexChange onIndexChange ]
                  $ [ HH.option [ HP.disabled true ] [ HH.text $ "Please choose an option" ] ]
                  <> map (\e -> HH.option (props e) [ HH.text e ]) c.enum
      SS.CseString c ->
        renderEntry' fallbackTitle schemaEntry
          $ let
              mi = maybe "0" show c.minLength

              ma = maybe "" show c.maxLength

              pat =
                if mi == "0" && ma == "" then
                  []
                else
                  [ HP.pattern $ ".{" <> mi <> "," <> ma <> "}" ]
            in
              HH.input $ [ HE.onValueChange (act <<< const <<< SS.CvString) ]
                <> opt HP.value (maybe c.default (Just <<< show) value)
                <> pat
      SS.CseRegex c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input
          $ [ HE.onValueChange (act <<< const <<< SS.CvString)
            ]
          <> opt HP.value (maybe c.default (Just <<< show) value)
      SS.CseConst _c -> renderEntry' fallbackTitle schemaEntry $ HH.input [ HP.value "const", HP.disabled true ]
      SS.CseArray _c -> HH.input [ HP.value "Unsupported configuration type: array", HP.disabled true ]
      SS.CseObject c ->
        let
          findVal k = Map.lookup k $ toVal value

          toVal = case _ of
            Just (SS.CvObject m) -> m
            _ -> Map.empty

          act' k = \f -> act (SS.CvObject <<< Map.alter (Just <<< f) k <<< toVal)
        in
          HH.span_
            $ map (\(Tuple k schema) -> renderEntry (act' k) k (findVal k) schema)
            $ Map.toUnfoldable c.properties
      SS.CseOneOf _c -> HH.input [ HP.value "Unsupported configuration type: oneOf", HP.disabled true ]

    renderEntry' fallbackTitle schemaEntry inner =
      HH.label_
        [ withDescription $ HH.text $ fromMaybe fallbackTitle $ SS.configSchemaEntryTitle schemaEntry
        , inner
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
          Just priceCurrency ->
            if A.null priceBookOpts then
              "No price books for " <> show priceCurrency
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
          <> [ HH.div [ HP.class_ Css.orderLine ]
                [ HH.button
                    [ HP.class_ Css.addOrderLine, HE.onClick \_ -> AddOrderLine { sectionIndex: secIdx } ]
                    [ HH.text "+" ]
                ]
            , renderOrderSectionSummary sec.summary
            ]
    where
    SS.ProductCatalog pc = sof.productCatalog

    defaultCurrency = maybe (SS.ChargeCurrency (SS.Currency "FIX")) (SS.ChargeCurrency <<< unwrap) sof.currency

    body subBody =
      HH.div [ HP.class_ Css.orderSection ]
        $ [ HH.a [ HP.class_ Css.close, HE.onClick \_ -> RemoveSection { sectionIndex: secIdx } ] [ HH.text "×" ]
          ]
        <> subBody

    solutionLabel (SS.Solution s) = fromMaybe s.id s.title

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
              [ HH.text $ pb.title <> " (" <> pb.version <> ")" ]
        )

    sectionOrderLines sol orderLines = [ HH.div_ $ A.mapWithIndex renderOrderLine' orderLines ]
      where
      renderOrderLine' olIdx = renderOrderLine sol defaultCurrency { sectionIndex: secIdx, orderLineIndex: olIdx }

  renderSections ::
    StateOrderForm ->
    Array (Maybe OrderSection) ->
    H.ComponentHTML Action Slots m
  renderSections sof secs =
    HH.div_
      $ [ HH.h3_ [ HH.text "Sections" ]
        ]
      <> A.mapWithIndex (renderSection sof) secs
      <> [ HH.div [ HP.class_ Css.orderSection ]
            [ HH.button
                [ HP.class_ Css.addSection, HE.onClick \_ -> AddSection ]
                [ HH.text "+" ]
            ]
        ]

  renderOrderSectionSummary :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSectionSummary = SubTotal.renderSubTotalTable "Sub-totals"

  renderOrderSummary :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSummary = SubTotal.renderSubTotalTable "Totals"

  renderCustomer :: Maybe SS.Customer -> H.ComponentHTML Action Slots m
  renderCustomer customer =
    HH.div_
      [ HH.h3_ [ HH.text "Header" ]
      , HH.slot Customer.proxy unit Customer.component customer SetCustomer
      ]

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action Slots m)
  renderOrderForm sof =
    [ renderCustomer sof.orderForm.customer
    , renderSections sof sof.orderForm.sections
    , renderOrderSummary sof.orderForm.summary
    , HH.hr_
    , HH.label [ HP.for "of-json", HP.class_ Css.button ] [ HH.text "Order Form JSON" ]
    , Widgets.modal "of-json" "Order Form JSON"
        [ HH.pre_
            [ HH.code_ [ HH.text $ fromMaybe errMsg $ toJsonStr sof.orderForm ]
            ]
        ]
        []
    , HH.button [ HE.onClick $ \_ -> PostOrder ] [ HH.text "Create Order" ]
    ]
    where
    errMsg =
      "Cannot produce JSON. You need to select a customer\n\
           \price currency and a price book for each order section."

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
    Loading -> loading
    Loaded dat -> rend dat
    Error err -> error err

  renderContent :: Array (H.ComponentHTML Action Slots m)
  renderContent =
    [ HH.h1_ [ HH.text "Order Form" ] ]
      <> case state of
          Initializing _ -> []
          Initialized state' -> defRender state' renderOrderForm

toJson :: OrderForm -> Maybe SS.OrderForm
toJson orderForm = do
  customer <- orderForm.customer
  sections <- traverse toOrderSection =<< sequence orderForm.sections
  pure
    $ SS.OrderForm
        { id: ""
        , status: SS.OsInDraft
        , customer
        , sections
        }
  where
  toOrderLine :: OrderLine -> SS.OrderLine
  toOrderLine ol =
    SS.OrderLine
      { orderLineId: ol.orderLineId
      , status: ol.status
      , sku: _.sku $ unwrap $ ol.product
      , charges: fromMaybe [] ol.charges
      , configs: ol.configs
      , estimatedUsage: toSmartSpecQuantity ol.estimatedUsage
      }

  toPriceBookRef solutionUri pb =
    SS.PriceBookRef
      { priceBookId: pb.id
      , version: pb.version
      , solutionUri: Just solutionUri
      }

  toOrderSection :: OrderSection -> Maybe SS.OrderSection
  toOrderSection os = do
    solutionUri <- _.uri $ unwrap $ os.solution
    basePriceBook <- toPriceBookRef solutionUri <$> os.priceBook
    orderLines <- sequence $ map (toOrderLine <$> _) os.orderLines
    pure $ SS.OrderSection { basePriceBook, orderLines }

toJsonStr :: OrderForm -> Maybe String
toJsonStr = map (stringifyWithIndent 2 <<< encodeJson) <<< toJson

loadCatalog ::
  forall slots output m.
  MonadAff m =>
  H.HalogenM State Action slots output m Unit
loadCatalog = do
  H.put $ Initialized Loading
  productCatalog <- H.liftAff getProductCatalog
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
  H.put $ Initialized res

mkDefaultConfigs :: SS.Product -> Array SS.OrderLineConfig
mkDefaultConfigs (SS.Product p) =
  fromMaybe [ SS.OrderLineConfig { quantity: 1, config: Nothing } ]
    $ do
        schema <- p.orderConfigSchema
        default_ <- mkDefault schema
        pure [ SS.OrderLineConfig { quantity: 1, config: Just default_ } ]
  where
  mkDefault = case _ of
    SS.CseInteger x -> SS.CvInteger <$> x.default
    SS.CseString x -> SS.CvString <$> (x.default <|> A.head x.enum)
    SS.CseRegex x -> SS.CvString <$> x.default
    SS.CseConst x -> Just x.const
    SS.CseArray _ -> Just $ SS.CvArray []
    SS.CseObject x ->
      let
        defaults :: Maybe (Map String SS.ConfigValue)
        defaults =
          map Map.fromFoldable
            $ sequence
            $ map (\(Tuple k v) -> (\v' -> Tuple k v') <$> mkDefault v)
            $ (Map.toUnfoldable x.properties :: List _)
      in
        SS.CvObject <$> defaults
    SS.CseOneOf _ -> Nothing

calcSubTotal :: OrderSection -> OrderSection
calcSubTotal os =
  os
    { orderLines = orderLines'
    , summary = sumOrderLines orderLines'
    }
  where
  defaultCurrency = maybe (SS.ChargeCurrency (SS.Currency "FIX")) _.currency os.priceBook

  orderLines' = map (map (updateOrderLineCharges os.priceBook)) os.orderLines

  -- | Sets the order line charge and default quantity from the given price
  -- | book. If the order line already has a charge, then it is returned
  -- | unchanged.
  updateOrderLineCharges :: Maybe PriceBook -> OrderLine -> OrderLine
  updateOrderLineCharges mpb ol
    | isJust ol.charges = ol
    | otherwise =
      fromMaybe ol
        $ ( \pb ->
              let
                charges = lookupCharges ol.product pb
              in
                ol
                  { charges = charges
                  , estimatedUsage = mkDefaultEstimatedUsage ol.unitMap charges
                  }
          )
        <$> mpb

  lookupCharges :: SS.Product -> PriceBook -> Maybe (Array SS.Charge)
  lookupCharges (SS.Product product) pb = do
    rateCards <- pb.rateCards
    SS.RateCard rateCard <- Map.lookup product.sku rateCards
    pure rateCard.charges

  mkDefaultEstimatedUsage :: Charge.ChargeUnitMap -> Maybe (Array SS.Charge) -> QuantityMap
  mkDefaultEstimatedUsage _ Nothing = Map.empty

  mkDefaultEstimatedUsage unitMap (Just ces) =
    Map.fromFoldable
      $ List.concatMap mk
      $ List.fromFoldable ces
    where
    opt = maybe List.nil List.singleton

    mk charge = do
      unitId <- List.fromFoldable $ Charge.unitIds charge
      SS.ChargeUnit { kind } <- opt $ Map.lookup unitId unitMap
      if kind /= SS.CkUsage then
        List.nil
      else
        let
          dims :: List SS.DimValue
          dims = List.fromFoldable $ Charge.dims charge
        in
          pure
            $ Tuple unitId
            $ if List.null dims then
                Left 1
              else
                Right (Map.fromFoldable $ (\d -> Tuple d 1) <$> dims)

  sumOrderLines :: Array (Maybe OrderLine) -> SubTotal
  sumOrderLines = A.foldl (\a b -> a <> conv b) mempty

  conv :: Maybe OrderLine -> SubTotal
  conv mol =
    fromMaybe mempty
      $ do
          ol <- mol
          charges <- ol.charges
          pure $ calcCharges (orderLineQuantity ol) ol.estimatedUsage ol.unitMap charges

  calcCharges :: Quantity -> QuantityMap -> Charge.ChargeUnitMap -> Array SS.Charge -> SubTotal
  calcCharges quantity estimatedUsageMap unitMap =
    List.foldl (\a charge -> a <> calcCharge charge) mempty
      <<< List.fromFoldable
    where
    calcCharge :: SS.Charge -> SubTotal
    calcCharge = SubTotal.calcSubTotal quantity estimatedUsageMap unitMap defaultCurrency

calcTotal :: OrderForm -> OrderForm
calcTotal orderForm = orderForm { summary = sumOrderSecs orderForm.sections }
  where
  sumOrderSecs = A.foldl (\a b -> a <> conv b) mempty

  conv = maybe mempty (\{ summary } -> summary)

orderLineQuantity :: OrderLine -> Quantity
orderLineQuantity ol = foldl (\a (SS.OrderLineConfig b) -> a + b.quantity) 0 ol.configs

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

loadExisting ::
  forall slots output m.
  MonadAff m =>
  SS.OrderForm ->
  H.HalogenM State Action slots output m Unit
loadExisting (SS.OrderForm orderForm) = do
  H.put $ Initialized Loading
  productCatalog <- H.liftAff getProductCatalog
  let
    result = convertOrderForm <$> productCatalog
  H.put $ Initialized result
  where
  convertOrderForm :: SS.ProductCatalog -> StateOrderForm
  convertOrderForm productCatalog =
    let
      currency = toPricingCurrency orderForm.customer

      priceBooks = mkPriceBooks productCatalog currency
    in
      { productCatalog
      , currency
      , priceBooks
      , orderForm:
          calcTotal
            { id: Just orderForm.id
            , customer: Just orderForm.customer
            , status: Just orderForm.status
            , summary: mempty
            , sections: map (convertOrderSection productCatalog priceBooks) orderForm.sections
            }
      }

  convertOrderSection :: SS.ProductCatalog -> Map String (Array PriceBook) -> SS.OrderSection -> Maybe OrderSection
  convertOrderSection (SS.ProductCatalog { solutions }) pbs (SS.OrderSection s) = do
    let
      SS.PriceBookRef pbRef = s.basePriceBook
    solution <- List.find (\(SS.Solution { uri }) -> pbRef.solutionUri == uri) $ Map.values solutions
    let
      SS.Solution sol = solution

      priceBook = A.find (\pb -> pb.id == pbRef.priceBookId) =<< Map.lookup sol.id pbs
    pure
      $ calcSubTotal
          { solution
          , priceBook
          , orderLines: map (convertOrderLine solution) s.orderLines
          , summary: mempty
          }

  convertOrderLine (SS.Solution solution) (SS.OrderLine l) = do
    product <- List.find (\(SS.Product { sku }) -> l.sku == sku) $ solution.products
    pure
      { orderLineId: l.orderLineId
      , status: l.status
      , product
      , charges: Just l.charges
      , unitMap: Charge.productChargeUnitMap product
      , configs: l.configs
      , estimatedUsage: fromSmartSpecQuantity l.estimatedUsage
      }

modifyInitialized ::
  forall slots output m.
  MonadAff m =>
  (StateOrderForm -> StateOrderForm) ->
  H.HalogenM State Action slots output m Unit
modifyInitialized f =
  H.modify_
    $ case _ of
        Initialized st -> Initialized (f <$> st)
        initializing -> initializing

toPricingCurrency :: SS.Customer -> Maybe SS.PricingCurrency
toPricingCurrency = case _ of
  SS.NewCustomer cust -> case cust.commercial of
    SS.Commercial { billingCurrency } -> Just billingCurrency
  _ -> Nothing

-- | Assemble a map from solution ID to its associated price books. The price
-- | books are limited to the given pricing currency.
mkPriceBooks :: SS.ProductCatalog -> Maybe SS.PricingCurrency -> Map String (Array PriceBook)
mkPriceBooks (SS.ProductCatalog pc) = maybe Map.empty (Map.fromFoldableWith (<>) <<< mkPriceBookPairs)
  where
  rateCardMap = Map.fromFoldable <<< map (\rc@(SS.RateCard rc') -> Tuple rc'.sku rc)

  mkPriceBookPairs c = do
    SS.Solution sol <- A.fromFoldable $ Map.values pc.solutions
    SS.PriceBook pb <- sol.priceBooks
    SS.PriceBookVersion pbv <- pb.byVersion
    SS.PriceBookCurrency pbc <- pbv.byCurrency
    if pbc.currency == c then
      [ Tuple sol.id
          [ { id: pb.id
            , title: pb.title
            , version: pbv.version
            , currency: SS.ChargeCurrency (unwrap c)
            , rateCards: rateCardMap <$> pbc.rateCards
            }
          ]
      ]
    else
      []

handleAction ::
  forall slots output m.
  MonadAff m =>
  CredentialStore m => Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  Initialize -> do
    st <- H.get
    case st of
      Initializing orderForm -> loadExisting orderForm
      Initialized Idle -> loadCatalog
      _ -> pure unit
  SetCustomer customer ->
    modifyInitialized
      $ \st ->
          let
            currency = toPricingCurrency customer

            priceBooks = mkPriceBooks st.productCatalog currency
          in
            st
              { currency = currency
              , priceBooks = priceBooks
              , orderForm =
                st.orderForm
                  { customer = Just customer
                  --  If the currency changed then we can't use the same price
                  --  book so the summary and all sections need to be updated.
                  , summary = if st.currency == currency then st.orderForm.summary else mempty
                  , sections =
                    if st.currency == currency then
                      st.orderForm.sections
                    else
                      map (map (_ { priceBook = Nothing, summary = mempty :: SubTotal })) st.orderForm.sections
                  }
              }
  AddSection ->
    modifyInitialized
      $ \st ->
          st { orderForm { sections = snoc st.orderForm.sections Nothing } }
  SectionSetSolution { sectionIndex, solutionId } ->
    modifyInitialized
      $ \st ->
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
    modifyInitialized
      $ \st ->
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
    modifyInitialized
      $ \st ->
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
      modifyInitialized
        $ \st ->
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
      modifyInitialized
        $ \st ->
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
        { orderLineId: Nothing
        , status: SS.OlsNew
        , product
        , charges: Nothing
        , unitMap: Charge.productChargeUnitMap product
        , configs: mkDefaultConfigs product
        , estimatedUsage: Map.empty
        }

      updateOrderLine :: SS.Product -> Maybe OrderLine -> OrderLine
      updateOrderLine product = case _ of
        Nothing -> mkOrderLine product
        Just _ol -> mkOrderLine product

      -- | Build order lines for all required product options.
      requiredOptions :: SS.Product -> Map SS.SkuCode SS.Product -> Array (Maybe OrderLine)
      requiredOptions (SS.Product p) solProds =
        let
          requiredSkuCode = case _ of
            SS.ProdOptSkuCode _ -> Nothing
            SS.ProductOption po -> if po.required then Just po.sku else Nothing

          requiredProds = A.mapMaybe (requiredSkuCode >=> (\o -> Map.lookup o solProds)) <$> p.options
        in
          maybe [] (map (Just <<< mkOrderLine)) requiredProds

      updateOrderSection :: OrderSection -> OrderSection
      updateOrderSection section =
        let
          solProds = SS.solutionProducts section.solution
        in
          calcSubTotal
            section
              { orderLines =
                maybe
                  section.orderLines
                  (\(Tuple product ls) -> ls <> requiredOptions product solProds)
                  ( do
                      product <- Map.lookup sku solProds
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
      modifyInitialized
        $ \st -> st { orderForm = calcTotal st.orderForm { sections = updateSections st.orderForm.sections } }
  OrderLineSetQuantity { sectionIndex, orderLineIndex, configIndex, quantity } ->
    let
      updateOrderConfig :: SS.OrderLineConfig -> SS.OrderLineConfig
      updateOrderConfig (SS.OrderLineConfig olc) = SS.OrderLineConfig $ olc { quantity = quantity }

      -- | Remove the configuration entry. If this is the last entry then we
      -- | ignore the request.
      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol =
        ol
          { configs =
            if A.null ol.configs then
              [ SS.OrderLineConfig { quantity, config: Nothing } ]
            else
              fromMaybe ol.configs $ A.modifyAt configIndex updateOrderConfig ol.configs
          }
    in
      modifyInitialized $ \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineAddConfig { sectionIndex, orderLineIndex } ->
    let
      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol = ol { configs = ol.configs <> mkDefaultConfigs ol.product }
    in
      modifyInitialized $ \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
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
      modifyInitialized $ \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineSetConfig { sectionIndex, orderLineIndex, configIndex, alter } ->
    let
      alterConfig (SS.OrderLineConfig olc) = SS.OrderLineConfig $ olc { config = Just $ alter olc.config }

      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol =
        ol
          { configs =
            fromMaybe [ SS.OrderLineConfig { quantity: 1, config: Just $ alter Nothing } ]
              $ A.modifyAt configIndex alterConfig ol.configs
          }
    in
      modifyInitialized
        $ \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineSetCharges { sectionIndex, orderLineIndex, charges, estimatedUsage } ->
    modifyInitialized
      $ \st ->
          modifyOrderLine sectionIndex orderLineIndex st
            _
              { charges = Just charges
              , estimatedUsage = estimatedUsage
              }
  PostOrder -> do
    st <- H.get
    let
      ld o = case o of
        Loaded o' -> loadExisting o'
        _ -> pure unit
    case st of
      Initialized (Loaded st') -> case toJson st'.orderForm of
        Nothing -> pure unit
        Just json -> ld =<< H.lift (postOrder json)
      _ -> pure unit
