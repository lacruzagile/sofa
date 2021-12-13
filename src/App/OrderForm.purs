module App.OrderForm (Slot, Input(..), proxy, component) where

import Prelude
import App.Charge (Slot, component, proxy) as Charge
import App.OrderForm.OrderHeader as OrderHeader
import App.Requests (getOrder, getProductCatalog, patchOrder, postOrder, postOrderFulfillment)
import Control.Alternative ((<|>))
import Css as Css
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Array (foldl, head, modifyAt, snoc)
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Charge (ChargeUnitMap, dims, productChargeUnitMap, unitIds) as Charge
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Enum.Generic (genericFromEnum, genericToEnum)
import Data.Int as Int
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Loadable (Loadable(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe, maybe')
import Data.Newtype (unwrap)
import Data.Quantity (QuantityMap, Quantity, fromSmartSpecQuantity, toSmartSpecQuantity)
import Data.SmartSpec as SS
import Data.String as S
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
    , orderHeader :: OrderHeader.Slot Unit
    )

data Input
  = NewOrder
  | ExistingOrder SS.OrderForm
  | ExistingOrderId SS.OrderId

data State
  = Initializing Input
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
  = { original :: Maybe SS.OrderForm -- ^ The original order form, if one exists.
    , orderHeader :: Maybe OrderHeader.OrderHeader
    , displayName :: Maybe String
    , status :: SS.OrderStatus
    , notes :: Array SS.OrderNote
    , summary :: SubTotal
    , sections :: Array (Maybe OrderSection)
    }

type OrderSection
  = { orderSectionId :: Maybe SS.OrderSectionId
    , solution :: SS.Solution
    , priceBook :: Maybe PriceBook
    , orderLines :: Array (Maybe OrderLine)
    -- ^ Order lines of the product options.
    , summary :: SubTotal
    }

type OrderLine
  = { orderLineId :: Maybe SS.OrderLineId
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
  | SetOrderDisplayName String
  | SetOrderHeader (Maybe OrderHeader.OrderHeader)
  | SetOrderStatus SS.OrderStatus
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
  | CreateUpdateOrder -- ^ Create or update the current order.
  | FulfillOrder -- ^ Trigger order fulfillment.

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
initialState = Initializing

initialize :: Maybe Action
initialize = Just Initialize

render :: forall m. MonadAff m => CredentialStore m => State -> H.ComponentHTML Action Slots m
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
                [ HH.label [ HP.classes [ Css.full, Css.threeFifth1000 ] ]
                    [ HH.text "Product"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.disabled true
                        , HP.value (show product.sku)
                        ]
                    ]
                , HH.label [ HP.classes [ Css.full, Css.fifth1000 ] ]
                    [ HH.text "Status"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.disabled true
                        , HP.value (SS.prettyOrderLineStatus ol.status)
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
                maybe []
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
      SS.CseBoolean _ ->
        let
          checked = case value of
            Just (SS.CvBoolean b) -> b
            _ -> false
        in
          renderCheckbox fallbackTitle schemaEntry
            $ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.class_ Css.checkable
                , HP.checked checked
                , HE.onChecked (act <<< const <<< SS.CvBoolean)
                ]
      SS.CseInteger c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input
          $ [ HP.type_ HP.InputNumber
            , HP.placeholder "Integer"
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

              placeholder = case Tuple mi ma of
                Tuple "0" "" -> "String"
                Tuple "0" ma' -> "String of max " <> ma' <> " characters"
                Tuple mi' ma'
                  | mi' == ma' -> "String of " <> mi' <> " characters"
                  | otherwise -> "String between " <> mi' <> " and " <> ma' <> " characters"
            in
              HH.input
                $ [ HP.placeholder placeholder
                  , HE.onValueChange (act <<< const <<< SS.CvString)
                  ]
                <> opt HP.value (maybe c.default (Just <<< show) value)
                <> pat
      SS.CseRegex c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input
          $ [ HP.placeholder $ "String matching " <> c.pattern
            , HP.pattern c.pattern
            , HE.onValueChange (act <<< const <<< SS.CvString)
            ]
          <> opt HP.value (maybe c.default (Just <<< show) value)
      SS.CseConst _c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input [ HP.value "const", HP.disabled true ]
      SS.CseArray c ->
        let
          entries = case value of
            Just (SS.CvArray vals) -> vals
            _ -> []

          toVal = case _ of
            Just (SS.CvArray arr) -> arr
            _ -> []

          act' idx = \f -> act (SS.CvArray <<< fromMaybe [] <<< A.modifyAt idx (f <<< Just) <<< toVal)
        in
          renderEntry' fallbackTitle schemaEntry
            $ HH.span_
            $ A.mapWithIndex (\i -> renderListEntry (act' i) c.items) entries
            <> [ renderAddListEntry c.items act ]
      SS.CseObject c ->
        let
          findVal k = Map.lookup k $ toVal value

          toVal = case _ of
            Just (SS.CvObject m) -> m
            _ -> Map.empty

          act' k = \f -> act (SS.CvObject <<< Map.alter (Just <<< f) k <<< toVal)

          renderFields =
            map (\(Tuple k schema) -> renderEntry (act' k) k (findVal k) schema)
              $ Map.toUnfoldable c.properties
        in
          if S.null fallbackTitle then
            HH.span_ renderFields
          else
            HH.fieldset_
              ( [ HH.legend_ [ withDescription [] fallbackTitle schemaEntry ] ]
                  <> renderFields
              )
      SS.CseOneOf _c -> HH.input [ HP.value "Unsupported configuration type: oneOf", HP.disabled true ]

    renderCheckbox fallbackTitle schemaEntry inner =
      HH.div_
        [ HH.label_
            [ inner
            , withDescription [ Css.checkable ] fallbackTitle schemaEntry
            ]
        ]

    renderEntry' fallbackTitle schemaEntry inner =
      HH.label_
        [ withDescription [] fallbackTitle schemaEntry
        , inner
        ]

    withDescription classes fallbackTitle schemaEntry =
      Widgets.withMaybeTooltip
        classes
        Widgets.Top
        (SS.configSchemaEntryDescription schemaEntry)
        (HH.text $ fromMaybe fallbackTitle $ SS.configSchemaEntryTitle schemaEntry)

    renderListEntry ::
      ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
      SS.ConfigSchemaEntry ->
      SS.ConfigValue ->
      H.ComponentHTML Action Slots m
    renderListEntry act entry value =
      HH.div [ HP.class_ Css.orderLineConfig ]
        [ renderEntry act "" (Just value) entry ]

    renderAddListEntry schemaEntry act =
      HH.div [ HP.class_ Css.orderLineConfig ]
        [ HH.button
            [ HP.class_ Css.addOrderLineConfig
            , HE.onClick \_ ->
                let
                  toVal = case _ of
                    Just (SS.CvArray vals) -> vals
                    _ -> []

                  defValue = maybe [ SS.CvNull ] A.singleton (mkDefaultConfig schemaEntry)

                  addEntry v = v <> defValue
                in
                  act (SS.CvArray <<< addEntry <<< toVal)
            ]
            [ HH.text "+" ]
        ]

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

  renderOrderInfo :: OrderForm -> H.ComponentHTML Action Slots m
  renderOrderInfo orderForm =
    HH.div [ HP.classes [ Css.flex, Css.two ] ]
      $ withOriginal
          ( \o ->
              HH.div_
                [ HH.strong_ [ HH.text "Order ID" ]
                , HH.text ": "
                , HH.text $ maybe "Not Available" show o.id
                ]
          )
      <> [ HH.div_
            [ HH.strong_ [ HH.text "Name" ]
            , HH.text ": "
            , renderOrderDisplayName orderForm.displayName
            ]
        ]
      <> withOriginal
          ( \o ->
              HH.div_
                [ HH.strong_ [ HH.text "Created" ]
                , HH.text ": "
                , HH.text $ maybe "Not Available" unwrap o.createTime
                ]
          )
      <> [ HH.div_
            [ HH.strong_ [ HH.text "Status" ]
            , HH.text ": "
            , renderOrderStatus orderForm.status
            ]
        ]
      <> withOriginal
          ( \o ->
              HH.div_
                [ HH.strong_ [ HH.text "Approval" ]
                , HH.text ": "
                , HH.text $ SS.prettyOrderApprovalStatus o.approvalStatus
                ]
          )
      <> [ HH.div_
            [ HH.strong_ [ HH.text "Notes" ]
            , HH.text ": "
            , renderOrderNotes orderForm.notes
            ]
        ]
    where
    withOriginal renderEntry = case orderForm.original of
      Nothing -> []
      Just (SS.OrderForm o) -> [ renderEntry o ]

  renderOrderDisplayName :: Maybe String -> H.ComponentHTML Action Slots m
  renderOrderDisplayName name =
    HH.input
      [ HP.value $ fromMaybe "" name
      , HP.style "width:auto"
      , HE.onValueChange SetOrderDisplayName
      ]

  renderOrderStatus :: SS.OrderStatus -> H.ComponentHTML Action Slots m
  renderOrderStatus selected =
    HH.select [ HP.style "width:auto", HE.onSelectedIndexChange actSetOrderStatus ]
      $ map renderOption orderStatuses
    where
    orderStatuses = A.mapMaybe genericToEnum $ enumFromTo bottom top

    bottom = genericFromEnum (genericBottom :: SS.OrderStatus)

    top = genericFromEnum (genericTop :: SS.OrderStatus)

    actSetOrderStatus = maybe NoOp SetOrderStatus <<< A.index orderStatuses

    renderOption s =
      HH.option
        [ HP.selected $ s == selected ]
        [ HH.text $ SS.prettyOrderStatus s ]

  renderOrderNotes :: Array SS.OrderNote -> H.ComponentHTML Action Slots m
  renderOrderNotes [] = HH.text "No order notes"

  renderOrderNotes notes = HH.text $ (show $ A.length notes) <> " notes"

  renderOrderHeader :: Maybe OrderHeader.OrderHeader -> H.ComponentHTML Action Slots m
  renderOrderHeader orderHeader =
    HH.div_
      [ HH.h3_ [ HH.text "Header" ]
      , HH.slot OrderHeader.proxy unit OrderHeader.component orderHeader SetOrderHeader
      ]

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action Slots m)
  renderOrderForm sof =
    [ renderOrderInfo sof.orderForm
    , renderOrderHeader sof.orderForm.orderHeader
    , renderSections sof sof.orderForm.sections
    , renderOrderSummary sof.orderForm.summary
    , HH.hr_
    , HH.div_
        [ HH.label
            [ HP.for "of-json", HP.class_ Css.button ]
            [ HH.text "Order Form JSON" ]
        , Widgets.modal "of-json" "Order Form JSON"
            [ HH.pre_
                [ HH.code_ [ HH.text $ fromMaybe errMsg $ toJsonStr sof.orderForm ]
                ]
            ]
            []
        ]
    , HH.button
        [ HP.disabled $ maybe true (const false) sof.orderForm.orderHeader
        , HE.onClick $ \_ -> CreateUpdateOrder
        ]
        [ HH.text $ maybe "Create Order" (const "Update Order") (getOrderId sof) ]
    , HH.text " "
    , HH.button
        [ HP.disabled $ maybe true (SS.OsInFulfillment /= _) (getOriginalOrderStatus sof)
        , HE.onClick $ \_ -> FulfillOrder
        ]
        [ HH.text "Fulfill Order" ]
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
  orderHeader <- orderForm.orderHeader
  sections <- traverse toOrderSection =<< sequence orderForm.sections
  pure
    $ SS.OrderForm
        { id: (\(SS.OrderForm { id }) -> id) =<< orderForm.original
        , status: orderForm.status
        , approvalStatus: SS.OasUndecided
        , displayName: orderForm.displayName
        , commercial: orderHeader.commercial
        , buyer: orderHeader.buyer
        , seller: orderHeader.seller
        , orderNotes: []
        , sections
        , createTime: Nothing
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
      , solutionUri: Just "NOVA" -- FIXME: Should be solutionUri.
      }

  toOrderSection :: OrderSection -> Maybe SS.OrderSection
  toOrderSection os = do
    solutionUri <- _.uri $ unwrap $ os.solution
    basePriceBook <- toPriceBookRef solutionUri <$> os.priceBook
    orderLines <- sequence $ map (toOrderLine <$> _) os.orderLines
    pure
      $ SS.OrderSection
          { orderSectionId: os.orderSectionId
          , basePriceBook
          , orderLines
          }

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
              { original: Nothing
              , displayName: Nothing
              , orderHeader: Nothing
              , status: SS.OsInDraft
              , notes: []
              , summary: mempty
              , sections: []
              }
          }
      )
        <$> productCatalog
  H.put $ Initialized res

mkDefaultConfig :: SS.ConfigSchemaEntry -> Maybe SS.ConfigValue
mkDefaultConfig = case _ of
  SS.CseBoolean x -> SS.CvBoolean <$> x.default
  SS.CseInteger x -> SS.CvInteger <$> x.default
  SS.CseString x -> SS.CvString <$> (x.default <|> A.head x.enum)
  SS.CseRegex x -> SS.CvString <$> x.default
  SS.CseConst x -> Just x.const
  SS.CseArray _ -> Just $ SS.CvArray []
  SS.CseObject x ->
    let
      defaults :: Map String SS.ConfigValue
      defaults =
        Map.fromFoldable
          $ List.mapMaybe (\(Tuple k v) -> (\v' -> Tuple k v') <$> mkDefaultConfig v)
          $ Map.toUnfoldable x.properties
    in
      Just $ SS.CvObject defaults
  SS.CseOneOf _ -> Nothing

mkDefaultConfigs :: SS.Product -> Array SS.OrderLineConfig
mkDefaultConfigs (SS.Product p) =
  fromMaybe [ SS.OrderLineConfig { quantity: 1, config: Nothing } ]
    $ do
        schema <- p.orderConfigSchema
        default_ <- mkDefaultConfig schema
        pure [ SS.OrderLineConfig { quantity: 1, config: Just default_ } ]

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
loadExisting original@(SS.OrderForm orderForm) = do
  H.put $ Initialized Loading
  productCatalog <- H.liftAff getProductCatalog
  let
    result = convertOrderForm <$> productCatalog
  H.put $ Initialized result
  where
  convertOrderForm :: SS.ProductCatalog -> StateOrderForm
  convertOrderForm productCatalog =
    let
      currency = toPricingCurrency orderForm.commercial

      priceBooks = mkPriceBooks productCatalog currency
    in
      { productCatalog
      , currency
      , priceBooks
      , orderForm:
          calcTotal
            { original: Just original
            , displayName: orderForm.displayName
            , orderHeader:
                Just
                  { commercial: orderForm.commercial
                  , buyer: orderForm.buyer
                  , seller: orderForm.seller
                  }
            , status: orderForm.status
            , notes: orderForm.orderNotes
            , summary: mempty
            , sections: map (convertOrderSection productCatalog priceBooks) orderForm.sections
            }
      }

  convertOrderSection :: SS.ProductCatalog -> Map String (Array PriceBook) -> SS.OrderSection -> Maybe OrderSection
  convertOrderSection (SS.ProductCatalog { solutions }) pbs (SS.OrderSection s) = do
    let
      SS.PriceBookRef pbRef = s.basePriceBook
    solution <-
      List.find
        ( \(SS.Solution { uri }) ->
            pbRef.solutionUri == uri
              || ( pbRef.solutionUri == Just "NOVA"
                    && uri
                    == Just "https://ea.pages.sinch.com/smart-spec/v1alpha1/examples/phase1/solution.phase1.sms-prod.json"
                )
        )
        $ Map.values solutions
    let
      SS.Solution sol = solution

      priceBook = A.find (\pb -> pb.id == pbRef.priceBookId) =<< Map.lookup sol.id pbs
    pure
      $ calcSubTotal
          { orderSectionId: s.orderSectionId
          , solution
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

toPricingCurrency :: SS.Commercial -> Maybe SS.PricingCurrency
toPricingCurrency (SS.Commercial { billingCurrency }) = Just billingCurrency

getOrderId :: StateOrderForm -> Maybe SS.OrderId
getOrderId = (\(SS.OrderForm o) -> o.id) <=< _.orderForm.original

getOriginalOrderStatus :: StateOrderForm -> Maybe SS.OrderStatus
getOriginalOrderStatus = map (\(SS.OrderForm o) -> o.status) <<< _.orderForm.original

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
            , title: fromMaybe pb.id pb.title
            , version: pbv.version
            , currency: SS.ChargeCurrency (unwrap c)
            , rateCards: rateCardMap <$> pbc.rateCards
            }
          ]
      ]
    else
      []

handleAction ::
  forall output m.
  MonadAff m =>
  CredentialStore m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  Initialize -> do
    st <- H.get
    case st of
      Initializing NewOrder -> loadCatalog
      Initializing (ExistingOrder orderForm) -> loadExisting orderForm
      Initializing (ExistingOrderId id) -> do
        H.put $ Initialized Loading
        orderForm <- H.lift $ getOrder id
        case orderForm of
          Error err -> H.put $ Initialized (Error err)
          Idle -> H.put $ Initialized Idle
          Loaded order -> loadExisting order
          Loading -> H.put $ Initialized Loading
      _ -> pure unit
  SetOrderDisplayName name ->
    modifyInitialized
      $ \st ->
          st
            { orderForm =
              st.orderForm
                { displayName =
                  let
                    sname = S.trim name
                  in
                    if sname == "" then Nothing else Just sname
                , summary = mempty
                , sections = []
                }
            }
  SetOrderHeader Nothing ->
    modifyInitialized
      $ \st ->
          st
            { currency = Nothing
            , priceBooks = Map.empty
            , orderForm =
              st.orderForm
                { orderHeader = Nothing
                , summary = mempty
                , sections = []
                }
            }
  SetOrderHeader (Just orderHeader) ->
    modifyInitialized
      $ \st ->
          let
            currency = toPricingCurrency orderHeader.commercial

            priceBooks = mkPriceBooks st.productCatalog currency
          in
            st
              { currency = currency
              , priceBooks = priceBooks
              , orderForm =
                st.orderForm
                  { orderHeader = Just orderHeader
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
  SetOrderStatus status ->
    modifyInitialized
      $ \st -> st { orderForm = st.orderForm { status = status } }
  AddSection ->
    modifyInitialized
      $ \st -> st { orderForm { sections = snoc st.orderForm.sections Nothing } }
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
                          ( \sec ->
                              Just
                                { orderSectionId: _.orderSectionId =<< sec
                                , solution: solution
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
  CreateUpdateOrder -> do
    st <- H.get
    let
      -- Updates the current state to match the response order object.
      ld o = case o of
        Loaded o' -> loadExisting o'
        _ -> pure unit

      run json =
        maybe'
          (\_ -> postOrder json)
          (\id -> patchOrder id json)
    case st of
      Initialized (Loaded st') -> case toJson st'.orderForm of
        Nothing -> pure unit
        Just json -> ld =<< H.lift (run json (getOrderId st'))
      _ -> pure unit
  FulfillOrder -> do
    st <- H.get
    let
      -- Updates the current state to match the response order object.
      ld o = case o of
        Loaded o' -> loadExisting o'
        _ -> pure unit
    case st of
      Initialized
        ( Loaded
          { orderForm: { original: Just (SS.OrderForm { id: Just id }) }
        }
      ) -> ld =<< H.lift (postOrderFulfillment id)
      _ -> pure unit
