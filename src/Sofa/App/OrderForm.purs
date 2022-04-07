module Sofa.App.OrderForm (Slot, Input(..), proxy, component) where

import Prelude
import Control.Alternative (guard, (<|>))
import Control.Parallel (parallel, sequential)
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Array (foldl, head, modifyAt, snoc)
import Data.Array as A
import Data.Date (Date, Month(..), canonicalDate)
import Data.Either (Either(..), either, note)
import Data.Enum (toEnum)
import Data.Foldable (sum)
import Data.Int as Int
import Data.List as SList
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe, maybe')
import Data.Newtype (unwrap)
import Data.String as S
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.App.Charge (Slot, component, proxy) as Charge
import Sofa.App.NavbarItemUser as NavbarItemUser
import Sofa.App.OrderForm.Buyer as Buyer
import Sofa.App.OrderForm.Commercial as Commercial
import Sofa.App.OrderForm.Notes as Notes
import Sofa.App.OrderForm.Observers as Observers
import Sofa.App.OrderForm.SelectOrderStatus as SelectOrderStatus
import Sofa.App.OrderForm.SelectProduct as SelectProduct
import Sofa.App.OrderForm.Seller as Seller
import Sofa.App.OrderForm.Widget.AssetConfigLink as WAssetConfigLink
import Sofa.App.OrderForm.Widget.Checkbox as WCheckbox
import Sofa.App.OrderForm.Widget.Dropdown as WDropdown
import Sofa.App.OrderForm.Widget.FileAttachment as WFileAttachment
import Sofa.App.OrderForm.Widget.Radio as WRadio
import Sofa.App.OrderForm.Widget.Textarea as WTextarea
import Sofa.App.OrderForm.Widget.Typeahead as WTypeahead
import Sofa.App.Requests (deleteFile, deleteOrder, getOrder, getProductCatalog, patchOrder, postOrder, postOrderFulfillment)
import Sofa.App.SchemaDataSource (DataSourceEnumResult, getDataSourceEnum)
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.EditableInput as EditableInput
import Sofa.Component.Icon as Icon
import Sofa.Component.Select as Select
import Sofa.Component.Tabs as Tabs
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Charge (ChargeUnitMap, dims, productChargeUnitMap, unitIds) as Charge
import Sofa.Data.Currency (mkCurrency, unsafeMkCurrency)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Quantity (QuantityMap, Quantity, fromSmartSpecQuantity, toSmartSpecQuantity)
import Sofa.Data.Schema (isValidValue)
import Sofa.Data.Schema as Schema
import Sofa.Data.SmartSpec as SS
import Sofa.Data.SubTotal (SubTotal)
import Sofa.Data.SubTotal as SubTotal
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "orderForm"
proxy = Proxy

type Slots
  = ( seller :: Seller.Slot Unit
    , buyer :: Buyer.Slot Unit
    , commercial :: Commercial.Slot Unit
    , notes :: Notes.Slot Unit
    , observers :: Observers.Slot Unit
    , selectSolution :: Select.Slot Int String -- ID is section ID, output is solution ID.
    , selectPriceBook :: Select.Slot Int Int -- ID is section index, output is price book index.
    , selectOrderStatus :: SelectOrderStatus.Slot Unit
    , selectProduct :: SelectProduct.Slot OrderLineIndex
    , charge :: Charge.Slot OrderLineIndex
    , selectEnum :: Select.Slot ConfigEntryIndex Int -- Output is selected value index.
    , widgetAssetConfigLink :: WAssetConfigLink.Slot ConfigEntryIndex
    , widgetCheckbox :: WCheckbox.Slot ConfigEntryIndex
    , widgetDropdown :: WDropdown.Slot ConfigEntryIndex
    , widgetFileAttachment :: WFileAttachment.Slot ConfigEntryIndex
    , widgetRadio :: WRadio.Slot ConfigEntryIndex
    , widgetTextarea :: WTextarea.Slot ConfigEntryIndex
    , widgetTypeahead :: WTypeahead.Slot ConfigEntryIndex
    , nectaryTabs :: Tabs.Slot ConfigEntryIndex
    , orderName :: EditableInput.Slot Unit
    , navbarItemUser :: NavbarItemUser.Slot Unit
    -- ^ Temporary until we have working Salesforce login.
    )

data Input
  = NewOrder
  | ExistingOrder SS.OrderForm
  | ExistingOrderId SS.OrderId
  | ExistingCrmQuoteId SS.CrmQuoteId

data State
  = Initializing Input
  | Initialized (Loadable StateOrderForm)

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
    , currency :: Maybe SS.PricingCurrency
    , priceBooks :: Map String (Array PriceBook)
    -- ^ Map from solution title to price books in the current currency.
    , orderForm :: OrderForm
    , configTabs :: Map ConfigEntryIndex Int
    -- ^ The currently selected tab of a `oneOf` configuration entry.
    , orderUpdateInFlight :: Boolean -- ^ Whether a current order update request is in flight.
    , orderFulfillInFlight :: Boolean -- ^ Whether a current order fulfillment request is in flight.
    }

-- Similar to SS.OrderForm but with a few optional fields.
type OrderForm
  = { original :: Maybe SS.OrderForm -- ^ The original order form, if one exists.
    , changed :: Boolean -- ^ Whether this order has unsaved changes.
    , crmQuoteId :: Maybe SS.CrmQuoteId
    , commercial :: Maybe SS.Commercial
    , buyer :: Maybe SS.Buyer
    , seller :: Maybe SS.Seller
    , displayName :: Maybe String
    , status :: SS.OrderStatus
    , observers :: Array SS.OrderObserver
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
    , statusReason :: String
    , product :: SS.Product
    , charges :: Maybe (Array SS.Charge)
    , unitMap :: Charge.ChargeUnitMap
    , configs :: Array SS.OrderLineConfig
    , estimatedUsage :: QuantityMap
    }

type PriceBook
  = { id :: String
    , title :: String
    , version :: Date
    , currency :: SS.ChargeCurrency -- ^ The default charge currency.
    , rateCards :: Maybe (Map SS.SkuCode SS.RateCard) -- ^ Maybe a map from SKU to rate cards.
    }

type OrderLineIndex
  = { sectionIndex :: Int, orderLineIndex :: Int }

type ConfigIndex
  = { sectionIndex :: Int
    , orderLineIndex :: Int
    , configIndex :: Int
    }

type ConfigEntryIndex
  = { configIndex :: ConfigIndex
    , entryIndex :: SList.List Int -- ^ Index within array or object.
    }

data Action
  = NoOp
  | Initialize
  | SetOrderDisplayName String
  | SetSeller SS.Seller
  | SetBuyer SS.Buyer
  | SetCommercial SS.Commercial
  | SetObservers (Array SS.OrderObserver)
  | SetNotes (Array SS.OrderNote)
  | SetOrderStatus SS.OrderStatus
  | AddSection
  | SectionSetSolution { sectionIndex :: Int, solutionId :: String }
  | SectionSetPriceBook { sectionIndex :: Int, priceBook :: Maybe PriceBook }
  | RemoveSection { sectionIndex :: Int }
  | AddOrderLine { sectionIndex :: Int }
  | OrderLineSetProduct
    { sectionIndex :: Int, orderLineIndex :: Int, product :: SS.Product
    }
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
  | OrderLineSetConfigTab ConfigEntryIndex Int
  | OrderLineSetCharges
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , charges :: Array SS.Charge
    , estimatedUsage :: QuantityMap
    }
  | RemoveOrderLine { sectionIndex :: Int, orderLineIndex :: Int }
  | DiscardOrder -- ^ Discard the currently loaded order.
  | CreateUpdateOrder -- ^ Create or update the current order.
  | FulfillOrder -- ^ Trigger order fulfillment.

component ::
  forall query output m.
  MonadAff m =>
  CredentialStore m =>
  MonadAlert m =>
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

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  MonadAlert m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.section (if inQuoteContext then [ HP.class_ (Css.c "mx-5") ] else [])
    [ if inQuoteContext then
        -- Temporarily show login button if running in a quote context.
        HH.header
          [ HP.classes [ Css.c "float-right" ] ]
          [ HH.slot_ NavbarItemUser.proxy unit NavbarItemUser.component absurd ]
      else
        HH.text ""
    , HH.article_ renderContent
    ]
  where
  -- An order is in a quote context when it has a CRM quote ID. In practice this
  -- means that SOFA is running as an app inside a Salesforce quote.
  inQuoteContext :: Boolean
  inQuoteContext = case state of
    Initialized (Loaded { orderForm: { crmQuoteId: Just _ } }) -> true
    _ -> false

  renderSmallTitle t = HH.div [ HP.class_ (Css.c "sofa-small-title") ] [ HH.text t ]

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

  renderChargeDetails ::
    OrderLineIndex ->
    Charge.ChargeUnitMap ->
    SS.ChargeCurrency ->
    QuantityMap ->
    Maybe (Array SS.Charge) ->
    Array (H.ComponentHTML Action Slots m)
  renderChargeDetails olIdx unitMap defaultCurrency estimatedUsage = maybe [] withCharges
    where
    withCharges = case _ of
      [] -> []
      charges ->
        [ HH.details [ HP.class_ (Css.c "mt-5") ]
            [ HH.summary
                [ HP.classes [ Css.c "text-lg", Css.c "cursor-pointer" ] ]
                [ HH.text "Charges" ]
            , renderCharges olIdx unitMap defaultCurrency estimatedUsage charges
            ]
        ]

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
            [ renderSmallTitle "Product"
            , HH.slot SelectProduct.proxy olIdx SelectProduct.component
                sol.products
                ( \product ->
                    OrderLineSetProduct
                      { sectionIndex: olIdx.sectionIndex
                      , orderLineIndex: olIdx.orderLineIndex
                      , product
                      }
                )
            ]
        ]
    Just ol ->
      let
        SS.Product product = ol.product
      in
        body
          $ [ HH.div [ HP.classes [ Css.c "flex" ] ]
                $ [ HH.div [ HP.class_ (Css.c "w-3/5") ]
                      [ renderSmallTitle "Product"
                      , HH.span
                          [ HP.classes [ Css.c "text-lg", Css.c "font-semibold" ] ]
                          [ HH.text $ show product.sku ]
                      ]
                  , HH.div [ HP.class_ (Css.c "w-1/5") ]
                      [ renderSmallTitle "Status"
                      , let
                          wrap content
                            | ol.statusReason == "" = HH.text content
                            | otherwise =
                              Tooltip.render
                                (Tooltip.defaultInput { text = ol.statusReason })
                                (Icon.textWithTooltip content)
                        in
                          HH.span
                            [ HP.class_ (Css.c "font-semibold") ]
                            [ wrap (SS.prettyOrderLineStatus ol.status) ]
                      ]
                  ]
                <> ( if isJust product.orderConfigSchema then
                      [ HH.div [ HP.class_ (Css.c "w-1/5") ]
                          [ renderSmallTitle "Total Quantity"
                          , HH.span
                              [ HP.class_ (Css.c "font-semibold") ]
                              [ HH.text
                                  $ show
                                  $ sum
                                  $ (\(SS.OrderLineConfig { quantity }) -> quantity)
                                  <$> ol.configs
                              ]
                          ]
                      ]
                    else
                      [ HH.label [ HP.class_ (Css.c "w-1/5") ]
                          [ renderSmallTitle "Quantity"
                          , renderQuantityInput 0
                              $ fromMaybe
                                  ( SS.OrderLineConfig
                                      { id: Nothing
                                      , quantity: 0
                                      , config: Nothing
                                      }
                                  )
                              $ head ol.configs
                          ]
                      ]
                  )
            ]
          <> renderChargeDetails olIdx ol.unitMap defaultCurrency ol.estimatedUsage ol.charges
          <> ( if isNothing product.orderConfigSchema then
                []
              else
                [ HH.details [ HP.class_ (Css.c "mt-5") ]
                    $ [ HH.summary
                          [ HP.classes [ Css.c "text-lg", Css.c "cursor-pointer" ] ]
                          [ HH.text "Configurations" ]
                      ]
                    <> renderProductConfigs product ol.orderLineId ol.configs
                    <> renderAddProductConfig
                ]
            )
    where
    removeBtn
      | not isInDraft = []
      | otherwise =
        [ HH.button
            [ HP.classes
                [ Css.c "relative"
                , Css.c "float-right"
                , Css.c "p-2"
                , Css.c "cursor-pointer"
                ]
            , HE.onClick \_ -> RemoveOrderLine olIdx
            ]
            [ Icon.close3 [ Icon.ariaLabel "Remove order line" ] ]
        ]

    body subBody =
      HH.div
        [ HP.classes [ Css.c "m-5", Css.c "border-t" ] ]
        (removeBtn <> subBody)

    renderQuantityInput cfgIdx (SS.OrderLineConfig olc) =
      HH.input
        [ HP.classes
            [ Css.c "nectary-input"
            , Css.c "nectary-input-number"
            , Css.c "w-full"
            , Css.c "max-w-96"
            ]
        , HP.type_ HP.InputNumber
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

    renderProductConfigs product orderLineId configs =
      let
        allowRemove = A.length configs > 1 && isInDraft
      in
        A.concat
          $ A.mapWithIndex (renderProductConfig allowRemove product orderLineId) configs

    renderProductConfig allowRemove product orderLineId cfgIdx olc@(SS.OrderLineConfig { id: configId, config }) =
      [ HH.div [ HP.classes [ Css.c "my-5", Css.c "p-5", Css.c "border-l-8", Css.c "border-gray-100" ] ]
          [ HH.label_
              [ HH.div [ HP.classes [ Css.c "sofa-small-title", Css.c "mr-5" ] ] [ HH.text "Quantity" ]
              , renderQuantityInput cfgIdx olc
              ]
          , if allowRemove then
              HH.button
                [ HP.classes
                    [ Css.c "relative"
                    , Css.c "float-right"
                    , Css.c "sofa-btn-destructive"
                    , Css.c "h-auto"
                    , Css.c "ml-2"
                    , Css.c "py-0"
                    ]
                , HE.onClick \_ ->
                    OrderLineRemoveConfig
                      { sectionIndex: olIdx.sectionIndex
                      , orderLineIndex: olIdx.orderLineIndex
                      , configIndex: cfgIdx
                      }
                ]
                [ HH.text "Remove Configuration" ]
            else
              HH.text ""
          , case configId of
              Nothing -> HH.text ""
              Just id ->
                -- Show the order line configuration ID to help use of asset
                -- configuration links.
                HH.div
                  [ HP.classes
                      [ Css.c "relative"
                      , Css.c "float-right"
                      , Css.c "ml-2"
                      , Css.c "text-stormy-300"
                      ]
                  ]
                  [ Tooltip.render
                      ( Tooltip.defaultInput
                          { text = "Configuration ID: " <> id
                          , orientation = Tooltip.Left
                          , width = Just "20rem"
                          }
                      )
                      (HH.text "#")
                  ]
          , HH.hr [ HP.class_ (Css.c "my-2") ]
          , case config of
              Nothing -> HH.text ""
              Just c ->
                maybe (HH.text "")
                  ( renderConfigSchema
                      orderLineId
                      { sectionIndex: olIdx.sectionIndex
                      , orderLineIndex: olIdx.orderLineIndex
                      , configIndex: cfgIdx
                      }
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

    renderAddProductConfig
      | not isInDraft = []
      | otherwise =
        [ HH.div [ HP.classes [ Css.c "flex", Css.c "w-full" ] ]
            [ HH.div [ HP.class_ (Css.c "grow") ] []
            , HH.button
                [ HP.classes [ Css.c "sofa-btn-secondary", Css.c "h-8" ]
                , HE.onClick \_ -> OrderLineAddConfig olIdx
                ]
                [ HH.text "Add configuration" ]
            ]
        ]

  renderConfigSchema ::
    Maybe SS.OrderLineId ->
    ConfigIndex ->
    ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
    SS.ConfigValue ->
    SS.ConfigSchemaEntry ->
    H.ComponentHTML Action Slots m
  renderConfigSchema orderLineId configIdx onChange config =
    renderEntry
      rootEntryIdx
      onChange
      ""
      (Just config)
    where
    rootEntryIdx = { configIndex: configIdx, entryIndex: SList.Nil }

    mact :: forall a. (a -> Action) -> Maybe a -> Action
    mact = maybe NoOp

    opt :: forall a b. (a -> b) -> Maybe a -> Array b
    opt f = maybe [] (\x -> [ f x ])

    renderEntry ::
      ConfigEntryIndex ->
      ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
      String ->
      Maybe SS.ConfigValue ->
      SS.ConfigSchemaEntry ->
      H.ComponentHTML Action Slots m
    renderEntry entryIdx act fallbackTitle value schemaEntry = case schemaEntry of
      SS.CseBoolean _ ->
        let
          checked = case value of
            Just (SS.CvBoolean b) -> b
            _ -> false
        in
          renderCheckbox fallbackTitle schemaEntry
            $ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.classes [ Css.c "mr-5" ]
                , HP.checked checked
                , HE.onChecked (act <<< const <<< SS.CvBoolean)
                ]
      SS.CseInteger { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
      SS.CseInteger c
        | not (A.null c.enum) ->
          renderEnumEntry
            entryIdx
            act
            fallbackTitle
            value
            schemaEntry
            c
            SS.CvInteger
            show
      SS.CseInteger c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input
          $ [ HP.classes [ Css.c "nectary-input", Css.c "nectary-input-number", Css.c "w-96" ]
            , HP.type_ HP.InputNumber
            , HP.placeholder "Integer"
            , HE.onValueChange (mact (act <<< const <<< SS.CvInteger) <<< Int.fromString)
            ]
          <> opt (HP.value <<< show) value
          <> opt (HP.min <<< Int.toNumber) c.minimum
          <> opt (HP.max <<< Int.toNumber) c.maximum
      SS.CseString { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
      SS.CseString c
        | not (A.null c.enum) ->
          renderEnumEntry
            entryIdx
            act
            fallbackTitle
            value
            schemaEntry
            c
            SS.CvString
            identity
      SS.CseString c ->
        renderEntry' fallbackTitle schemaEntry
          $ let
              mi = maybe "0" show c.minLength

              ma = maybe "" show c.maxLength

              pat = case c.pattern of
                Just pattern -> [ HP.pattern pattern ]
                Nothing ->
                  if mi == "0" && ma == "" then
                    []
                  else
                    [ HP.pattern $ ".{" <> mi <> "," <> ma <> "}" ]

              placeholder = case c.pattern of
                Just pattern -> "String matching " <> pattern
                Nothing -> case Tuple mi ma of
                  Tuple "0" "" -> "String"
                  Tuple "0" ma' -> "String of max " <> ma' <> " characters"
                  Tuple mi' ma'
                    | mi' == ma' -> "String of " <> mi' <> " characters"
                    | otherwise -> "String between " <> mi' <> " and " <> ma' <> " characters"
            in
              HH.input
                $ [ HP.type_ HP.InputText
                  , HP.classes [ Css.c "nectary-input", Css.c "w-96" ]
                  , HP.placeholder placeholder
                  , HE.onValueChange (act <<< const <<< SS.CvString)
                  ]
                <> opt HP.value (maybe c.default (Just <<< show) value)
                <> pat
      SS.CseRegex { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
      SS.CseRegex c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input
          $ [ HP.type_ HP.InputText
            , HP.classes [ Css.c "nectary-input", Css.c "w-96" ]
            , HP.placeholder $ "String matching " <> c.pattern
            , HP.pattern c.pattern
            , HE.onValueChange (act <<< const <<< SS.CvString)
            ]
          <> opt HP.value (maybe c.default (Just <<< show) value)
      SS.CseConst _c ->
        renderEntry' fallbackTitle schemaEntry
          $ HH.input [ HP.type_ HP.InputText, HP.value "const", HP.disabled true ]
      SS.CseArray { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
      SS.CseArray c ->
        let
          entries = case value of
            Just (SS.CvArray vals) -> vals
            _ -> []

          toVal = case _ of
            Just (SS.CvArray arr) -> arr
            _ -> []

          act' idx = \f -> act (SS.CvArray <<< fromMaybe [] <<< A.modifyAt idx (f <<< Just) <<< toVal)

          removeAct idx = \_ -> act (SS.CvArray <<< fromMaybe [] <<< A.deleteAt idx <<< toVal)

          mkElement content =
            if S.null fallbackTitle then
              HH.div
                [ HP.classes [ Css.c "flex", Css.c "flex-col", Css.c "space-y-4" ] ]
                (content <> [ renderAddListEntry c.items act ])
            else
              HH.fieldset [ HP.classes [ Css.c "my-2", Css.c "flex", Css.c "flex-col", Css.c "border" ] ]
                ( [ HH.legend
                      [ HP.classes [ Css.c "ml-2", Css.c "px-3" ] ]
                      [ withDescription fallbackTitle schemaEntry ]
                  ]
                    <> content
                    <> [ renderAddListEntry c.items act ]
                )
        in
          mkElement
            $ A.mapWithIndex
                (\i -> renderListEntry (pushEntryIndex entryIdx i) (act' i) (removeAct i) c.items)
                entries
      SS.CseObject { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
      SS.CseObject c ->
        let
          findVal k = Map.lookup k $ toVal value

          toVal = case _ of
            Just (SS.CvObject m) -> m
            _ -> Map.empty

          act' k = \f -> act (SS.CvObject <<< Map.alter (Just <<< f) k <<< toVal)

          renderFields =
            A.mapWithIndex
              ( \i (Tuple k schema) ->
                  renderEntry (pushEntryIndex entryIdx i) (act' k) k (findVal k) schema
              )
              $ FO.toUnfoldable c.properties
        in
          if S.null fallbackTitle then
            HH.div
              [ HP.classes [ Css.c "flex", Css.c "flex-col", Css.c "space-y-4" ] ]
              renderFields
          else
            HH.fieldset
              [ HP.classes
                  [ Css.c "my-2"
                  , Css.c "p-3"
                  , Css.c "flex"
                  , Css.c "flex-col"
                  , Css.c "border"
                  ]
              ]
              ( [ HH.legend_ [ withDescription fallbackTitle schemaEntry ] ]
                  <> renderFields
              )
      SS.CseOneOf c ->
        let
          withTabs content =
            HH.div_
              [ HH.slot Tabs.proxy entryIdx Tabs.component
                  { selected:
                      fromMaybe 0 do
                        v <- value
                        A.findIndex (\schema -> isValidValue schema v) c.oneOf
                  , tabs:
                      A.mapWithIndex
                        ( \i schema ->
                            { disabled: false
                            , content: HH.text $ fromMaybe (show i) (Schema.getTitle schema)
                            }
                        )
                        c.oneOf
                  }
                  (OrderLineSetConfigTab entryIdx)
              , content
              ]

          -- The user has explicitly selected a tab so we try hard to show that
          -- tab, even ignoring the current configuration value if necessary.
          selectedTab = case state of
            Initialized (Loaded { configTabs }) -> do
              idx <- Map.lookup entryIdx configTabs
              schema <- A.index c.oneOf idx
              let
                value' = do
                  v <- value
                  if isValidValue schema v then value else Nothing
              pure $ renderEntry entryIdx act "" value' schema
            _ -> Nothing

          -- The user has not selected a tab but we have a value so show the
          -- first matching tab.
          matchingValue = do
            v <- value
            schema <- A.find (\schema -> isValidValue schema v) c.oneOf
            pure $ renderEntry entryIdx act "" value schema

          -- Just show the first tab.
          firstValue = renderEntry entryIdx act "" value <$> A.head c.oneOf
        in
          renderEntry' fallbackTitle schemaEntry
            $ maybe (HH.text $ "No oneOf schema matches the value: " <> show value) withTabs
            $ selectedTab
            <|> matchingValue
            <|> firstValue

    pushEntryIndex :: ConfigEntryIndex -> Int -> ConfigEntryIndex
    pushEntryIndex oldIdx idx = oldIdx { entryIndex = idx SList.: oldIdx.entryIndex }

    renderCheckbox fallbackTitle schemaEntry inner =
      HH.label [ HP.classes [ Css.c "flex" ] ]
        [ inner
        , withDescription fallbackTitle schemaEntry
        ]

    renderWidget entryIdx fallbackTitle value schemaEntry act widget =
      renderEntry' fallbackTitle schemaEntry
        $ case widget of
            SS.SwTextarea ->
              HH.slot
                WTextarea.proxy
                entryIdx
                WTextarea.component
                { value:
                    case value of
                      Just (SS.CvString string) -> Just string
                      _ -> Nothing
                }
                (mact (act <<< const <<< SS.CvString))
            SS.SwCheckbox { dataSource } ->
              maybe
                insufficientDataError
                ( \getEnumData ->
                    HH.slot
                      WCheckbox.proxy
                      entryIdx
                      WCheckbox.component
                      { value:
                          case value of
                            Just (SS.CvArray vs) -> vs
                            _ -> []
                      , getEnumData: getEnumData
                      }
                      (mact (act <<< const <<< SS.CvArray) <<< Just)
                )
                (mkGetEnumData <$> dataSourceWithFallback dataSource)
            SS.SwDropdown { dataSource } ->
              maybe
                insufficientDataError
                ( \getEnumData ->
                    HH.slot
                      WDropdown.proxy
                      entryIdx
                      WDropdown.component
                      { value, getEnumData: getEnumData }
                      (mact (act <<< const))
                )
                (mkGetEnumData <$> dataSourceWithFallback dataSource)
            SS.SwRadio { dataSource } ->
              maybe
                insufficientDataError
                ( \getEnumData ->
                    HH.slot
                      WRadio.proxy
                      entryIdx
                      WRadio.component
                      { value, getEnumData: getEnumData }
                      (mact (act <<< const))
                )
                (mkGetEnumData <$> dataSourceWithFallback dataSource)
            SS.SwTypeahead { minInputLength, debounceMs, dataSource } ->
              maybe
                insufficientDataError
                ( \getEnumData ->
                    HH.slot
                      WTypeahead.proxy
                      entryIdx
                      WTypeahead.component
                      { value
                      , minInputLength
                      , debounceMs
                      , getEnumData: getEnumData
                      }
                      (mact (act <<< const))
                )
                (mkGetEnumData <$> dataSourceWithFallback dataSource)
            SS.SwAssetConfigLink { sku } ->
              HH.slot
                WAssetConfigLink.proxy
                entryIdx
                WAssetConfigLink.component
                { value: maybe' (\_ -> mkDefaultConfig schemaEntry) Just value
                , skuPattern: sku
                , configs:
                    case state of
                      Initialized (Loaded { orderForm: { sections } }) -> do
                        section <-
                          maybe [] A.singleton
                            $ join
                            $ A.index sections entryIdx.configIndex.sectionIndex
                        mOrderLine <- section.orderLines
                        case mOrderLine of
                          Nothing -> []
                          Just orderLine ->
                            let
                              SS.Product { sku } = orderLine.product
                            in
                              [ Tuple sku orderLine.configs ]
                      _ -> []
                }
                (mact (act <<< const))
            SS.SwFileAttachment { maxSize, mediaTypes } -> case orderLineId of
              Nothing -> HH.text "Please save the order first."
              Just olid ->
                HH.slot
                  WFileAttachment.proxy
                  entryIdx
                  WFileAttachment.component
                  { orderLineId: olid
                  , value: maybe' (\_ -> mkDefaultConfig schemaEntry) Just value
                  , maxSize
                  , mediaTypes
                  }
                  (mact (act <<< const))
      where
      insufficientDataError =
        HH.span
          [ HP.class_ (Css.c "text-raspberry-500") ]
          [ HH.text "Insufficient data…" ]

      mkGetEnumData :: SS.SchemaDataSourceEnum -> Maybe String -> m DataSourceEnumResult
      mkGetEnumData dataSource =
        let
          getCommercial _unit = case state of
            Initialized (Loaded { orderForm: { commercial } }) -> commercial
            _ -> Nothing
        in
          getDataSourceEnum { getCommercial } dataSource

      -- Endow a data source with fallback to schema entry enum values. Takes as
      -- input a maybe data source, which is preferred, otherwise uses the enum
      -- values of the current schema entry, and if no enum is available then
      -- nothing is returned.
      dataSourceWithFallback :: Maybe SS.SchemaDataSourceEnum -> Maybe SS.SchemaDataSourceEnum
      dataSourceWithFallback = case _ of
        Just ds -> Just ds
        Nothing -> case schemaEntry of
          SS.CseInteger { enum }
            | not (A.null enum) ->
              Just
                $ SS.SdsEnumMap
                    { entries:
                        Map.fromFoldable
                          $ map (\i -> Tuple (show i) (SS.CvInteger i)) enum
                    }
          SS.CseString { enum }
            | not (A.null enum) ->
              Just
                $ SS.SdsEnumMap
                    { entries:
                        Map.fromFoldable
                          $ map (\s -> Tuple s (SS.CvString s)) enum
                    }
          _ -> Nothing

    renderEntry' fallbackTitle schemaEntry inner =
      if S.null fallbackTitle then
        inner
      else
        HH.label [ HP.classes [ Css.c "flex", Css.c "flex-col" ] ]
          [ withDescription fallbackTitle schemaEntry
          , inner
          ]

    renderEnumEntry ::
      forall a r.
      Eq a =>
      ConfigEntryIndex ->
      ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
      String ->
      Maybe SS.ConfigValue ->
      SS.ConfigSchemaEntry ->
      { default :: Maybe a, enum :: Array a | r } ->
      (a -> SS.ConfigValue) ->
      (a -> String) ->
      H.ComponentHTML Action Slots m
    renderEnumEntry entryIdx act fallbackTitle value schemaEntry c mkValue showValue =
      renderEntry' fallbackTitle schemaEntry
        $ let
            onIndexChange i = mact (act <<< const <<< mkValue) $ A.index c.enum i
          in
            HH.slot
              (Proxy :: Proxy "selectEnum")
              entryIdx
              Select.component
              ( Select.defaultInput
                  { selected =
                    do
                      selVal <- value <|> (mkValue <$> c.default)
                      A.findIndex (\v -> mkValue v == selVal) c.enum
                  , values = A.mapWithIndex (\i e -> Tuple (HH.text $ showValue e) i) c.enum
                  , wrapperClasses = [ Css.c "inline-block", Css.c "w-96" ]
                  }
              )
              onIndexChange

    withDescription fallbackTitle schemaEntry = case SS.configSchemaEntryDescription schemaEntry of
      Nothing -> body false
      Just description ->
        Tooltip.render
          (Tooltip.defaultInput { text = description, width = Just "20rem" })
          (body true)
      where
      body tt =
        HH.div
          [ HP.classes [ Css.c "sofa-small-title", Css.c "mr-5" ] ]
          [ HH.text $ fromMaybe fallbackTitle $ SS.configSchemaEntryTitle schemaEntry
          , if tt then Icon.tooltip else HH.text ""
          ]

    renderListEntry ::
      ConfigEntryIndex ->
      ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
      (Unit -> Action) ->
      SS.ConfigSchemaEntry ->
      SS.ConfigValue ->
      H.ComponentHTML Action Slots m
    renderListEntry entryIdx act removeAct entry value =
      HH.div [ HP.classes [ Css.c "p-3", Css.c "border-b", Css.c "group" ] ]
        [ renderRemoveListEntry removeAct
        , renderEntry entryIdx act "" (Just value) entry
        ]

    renderRemoveListEntry :: (Unit -> Action) -> H.ComponentHTML Action Slots m
    renderRemoveListEntry removeAct
      | not isInDraft = HH.text ""
      | otherwise =
        HH.button
          [ HP.classes
              [ Css.c "sofa-btn-destructive"
              , Css.c "h-auto"
              , Css.c "relative"
              , Css.c "float-right"
              , Css.c "py-0"
              , Css.c "invisible"
              , Css.c "group-hover:visible"
              ]
          , HE.onClick \_ -> removeAct unit
          ]
          [ HH.text "- Remove" ]

    renderAddListEntry schemaEntry act
      | not isInDraft = HH.text ""
      | otherwise =
        HH.div_
          [ HH.button
              [ HP.classes
                  [ Css.c "sofa-btn-secondary"
                  , Css.c "h-8"
                  , Css.c "m-5"
                  , Css.c "py-0"
                  ]
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
              [ HH.text "Add New" ]
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
            [ renderSmallTitle "Solution"
            , HH.slot
                (Proxy :: Proxy "selectSolution")
                secIdx
                Select.component
                ( Select.defaultInput
                    { values =
                      let
                        notSameSolutionId id' =
                          maybe
                            true
                            (\{ solution: SS.Solution { id } } -> id' /= id)

                        isSolutionAvailable id = A.all (notSameSolutionId id) sof.orderForm.sections

                        -- Filters out solutions that are already used for other
                        -- order sections.
                        filterAvailableSolutions = Map.filterKeys isSolutionAvailable
                      in
                        map
                          (\(Tuple i s) -> Tuple (HH.text $ solutionLabel s) i)
                          $ Map.toUnfoldable
                          $ filterAvailableSolutions pc.solutions
                    , noSelectionText = "Please choose a solution"
                    , wrapperClasses = [ Css.c "w-96" ]
                    }
                )
                actionSetSolution
            ]
        ]
    Just sec ->
      let
        SS.Solution sol = sec.solution

        priceBooks = fromMaybe [] $ Map.lookup sol.id sof.priceBooks

        priceBookOpts =
          let
            mkPriceBookOption i pb =
              Tuple
                (HH.text $ pb.title <> " (" <> SS.prettyDate pb.version <> ")")
                i
          in
            A.mapWithIndex mkPriceBookOption priceBooks

        -- We're in the process of adding an order line if there is a order line
        -- with value `Nothing`.
        isAddingOrderLine = A.any isNothing sec.orderLines
      in
        body
          $ [ HH.div [ HP.classes [ Css.c "flex" ] ]
                [ HH.div [ HP.class_ (Css.c "w-1/2") ]
                    [ renderSmallTitle "Solution"
                    , HH.span
                        [ HP.classes [ Css.c "text-lg", Css.c "font-semibold" ] ]
                        [ HH.text $ solutionLabel sec.solution ]
                    ]
                , if A.null priceBookOpts then
                    HH.text ""
                  else
                    HH.label [ HP.class_ (Css.c "w-1/2") ]
                      [ renderSmallTitle "Price Book"
                      , HH.slot
                          (Proxy :: Proxy "selectPriceBook")
                          secIdx
                          Select.component
                          ( Select.defaultInput
                              { selected =
                                do
                                  selPb <- sec.priceBook
                                  A.findIndex
                                    (\pb -> pb.id == selPb.id && pb.version == selPb.version)
                                    priceBooks
                              , values = priceBookOpts
                              , noSelectionText = "Please choose a price book"
                              }
                          )
                          (actionSetPriceBook priceBooks)
                      ]
                ]
            , renderOrderLines sec.solution sec.orderLines
            , HH.div
                [ HP.classes
                    [ Css.c "flex"
                    , Css.c "items-center"
                    , Css.c "space-x-4"
                    , Css.c "m-5"
                    , Css.c "mb-0"
                    , Css.c "pt-3"
                    , Css.c "border-t"
                    ]
                ]
                [ renderOrderSectionSummary sec.summary
                , HH.div [ HP.class_ (Css.c "grow") ] []
                , if not isInDraft || isAddingOrderLine then
                    HH.text ""
                  else
                    HH.button
                      [ HP.classes
                          [ Css.c "sofa-btn-secondary"
                          , Css.c "px-6"
                          , Css.c "gap-x-4"
                          ]
                      , HE.onClick \_ -> AddOrderLine { sectionIndex: secIdx }
                      ]
                      [ Icon.add
                          [ Icon.classes [ Css.c "w-6", Css.c "fill-tropical-500" ]
                          ]
                      , HH.text "Add product"
                      ]
                ]
            ]
    where
    SS.ProductCatalog pc = sof.productCatalog

    defaultCurrency =
      maybe
        (SS.ChargeCurrency (unsafeMkCurrency "FIX"))
        (SS.ChargeCurrency <<< unwrap)
        sof.currency

    body subBody =
      let
        removeBtn
          | not isInDraft = []
          | otherwise =
            [ HH.button
                [ HP.classes
                    [ Css.c "relative"
                    , Css.c "float-right"
                    , Css.c "p-2"
                    , Css.c "-m-3"
                    , Css.c "cursor-pointer"
                    ]
                , HE.onClick \_ -> RemoveSection { sectionIndex: secIdx }
                ]
                [ Icon.close3 [ Icon.ariaLabel "Remove order section" ] ]
            ]
      in
        HH.div
          [ HP.classes
              [ Css.c "p-3"
              , Css.c "rounded-md"
              , Css.c "bg-snow-100"
              ]
          ]
          (removeBtn <> subBody)

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
        $ A.index priceBooks i

    renderOrderLines sol orderLines = HH.div_ $ A.mapWithIndex renderOrderLine' orderLines
      where
      renderOrderLine' olIdx = renderOrderLine sol defaultCurrency { sectionIndex: secIdx, orderLineIndex: olIdx }

  renderSections ::
    StateOrderForm ->
    Array (Maybe OrderSection) ->
    H.ComponentHTML Action Slots m
  renderSections sof secs =
    HH.div [ HP.classes [ Css.c "flex", Css.c "flex-col", Css.c "space-y-5" ] ]
      $ A.mapWithIndex (renderSection sof) secs
      <> if not isInDraft then
          []
        else
          [ HH.div
              [ HP.classes
                  [ Css.c "flex"
                  , Css.c "p-3"
                  , Css.c "rounded-md"
                  , Css.c "bg-snow-100"
                  ]
              ]
              [ HH.div [ HP.class_ (Css.c "grow") ] []
              , HH.button
                  [ HP.classes
                      [ Css.c "sofa-btn-secondary"
                      , Css.c "px-6"
                      , Css.c "gap-x-4"
                      ]
                  , HE.onClick \_ -> AddSection
                  ]
                  [ Icon.add
                      [ Icon.classes [ Css.c "w-6", Css.c "fill-tropical-500" ]
                      ]
                  , HH.text "Add section"
                  ]
              ]
          ]

  renderOrderSectionSummary :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSectionSummary = Widgets.subTotalTable ""

  renderOrderSummary :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSummary subTotal
    | mempty == subTotal = HH.text ""
    | otherwise =
      HH.div
        [ HP.classes
            [ Css.c "p-3"
            , Css.c "space-x-4"
            , Css.c "rounded-md"
            , Css.c "bg-snow-100"
            ]
        ]
        [ Widgets.subTotalTable "Total " subTotal ]

  renderHeaderAndInfo :: OrderForm -> H.ComponentHTML Action Slots m
  renderHeaderAndInfo orderForm =
    HH.div
      [ HP.classes
          [ Css.c "flex"
          , Css.c "flex-col"
          , Css.c "space-y-6"
          , Css.c "p-8"
          , Css.c "rounded-md"
          , Css.c "bg-snow-100"
          ]
      ]
      [ renderOrderDisplayName orderForm.displayName --HH.h2 [ HP.class_ (Css.c "my-0") ] [ HH.text "Untitled order" ]
      , HH.p
          [ HP.classes [ Css.c "my-0", Css.c "text-stormy-300" ] ]
          [ HH.text "Below you can see the details of the order." ]
      , renderOrderHeader orderForm
      , renderOrderInfo orderForm
      ]

  renderOrderInfo :: OrderForm -> H.ComponentHTML Action Slots m
  renderOrderInfo orderForm =
    HH.div
      [ HP.classes
          [ Css.c "flex"
          , Css.c "flex-col"
          , Css.c "w-full"
          , Css.c "gap-y-4"
          , Css.c "p-8"
          , Css.c "rounded-md"
          , Css.c "border"
          , Css.c "border-snow-600"
          , Css.c "shadow-md"
          ]
      ]
      [ HH.div
          [ HP.classes
              [ Css.c "mb-6"
              , Css.c "flex"
              , Css.c "items-center"
              , Css.c "gap-x-6"
              ]
          ]
          [ HH.h3_ [ HH.text "Order information" ]
          , renderOrderStatus orderForm.status
          ]
      , withOriginal
          ( \o ->
              entry
                [ title "Order ID"
                , value [ HH.text $ maybe "Not Available" show o.id ]
                ]
          )
      , case orderForm.crmQuoteId of
          Nothing -> HH.text ""
          Just (SS.CrmQuoteId id) ->
            entry
              [ title "Quote ID"
              , value [ HH.text id ]
              ]
      , withOriginal
          ( \o ->
              entry
                [ title "Created"
                , value
                    [ maybe
                        (HH.text "Not Available")
                        Widgets.dateWithTimeTooltip
                        o.createTime
                    ]
                ]
          )
      , withOriginal
          ( \o ->
              entry
                [ title "Approval"
                , value [ HH.text $ SS.prettyOrderApprovalStatus o.approvalStatus ]
                ]
          )
      , entry
          [ title "Observers"
          , renderOrderObservers orderId orderForm.observers
          ]
      , entry
          [ title "Notes"
          , renderOrderNotes orderId orderForm.notes
          ]
      ]
    where
    entry = HH.div [ HP.classes [ Css.c "flex" ] ]

    title t = HH.h4 [ HP.classes [ Css.c "w-40" ] ] [ HH.text t ]

    value = HH.div_

    orderId = orderForm.original >>= \(SS.OrderForm order) -> order.id

    withOriginal renderEntry = case orderForm.original of
      Nothing -> HH.text ""
      Just (SS.OrderForm o) -> renderEntry o

  renderOrderDisplayName :: Maybe String -> H.ComponentHTML Action Slots m
  renderOrderDisplayName name =
    HH.slot
      (Proxy :: Proxy "orderName")
      unit
      EditableInput.component
      { value: fromMaybe "" name
      , placeholder: "Unnamed order"
      , classes: [ Css.c "w-fit", Css.c "max-w-128", Css.c "text-2xl" ]
      , editButtonProps: [ HPAria.label "Edit order name" ]
      , inputProps: [ HP.attr (H.AttrName "maxlength") "50" ]
      }
      SetOrderDisplayName

  renderOrderStatus :: SS.OrderStatus -> H.ComponentHTML Action Slots m
  renderOrderStatus selected =
    HH.slot
      SelectOrderStatus.proxy
      unit
      SelectOrderStatus.component
      selected
      SetOrderStatus

  renderOrderNotes :: Maybe SS.OrderId -> Array SS.OrderNote -> H.ComponentHTML Action Slots m
  renderOrderNotes orderId notes =
    HH.slot
      Notes.proxy
      unit
      Notes.component
      { orderId, notes }
      SetNotes

  renderOrderObservers :: Maybe SS.OrderId -> Array SS.OrderObserver -> H.ComponentHTML Action Slots m
  renderOrderObservers orderId observers =
    HH.slot
      Observers.proxy
      unit
      Observers.component
      { orderId, observers }
      SetObservers

  isInDraft = case state of
    Initialized (Loaded { orderForm: { status: SS.OsInDraft } }) -> true
    _ -> false

  renderOrderHeader :: OrderForm -> H.ComponentHTML Action Slots m
  renderOrderHeader orderForm =
    HH.div
      [ HP.classes
          [ Css.c "flex"
          , Css.c "flex-col"
          , Css.c "w-full"
          , Css.c "gap-y-4"
          , Css.c "p-8"
          , Css.c "rounded-md"
          , Css.c "border"
          , Css.c "border-snow-600"
          , Css.c "shadow-md"
          ]
      ]
      [ HH.h3 [ HP.class_ (Css.c "mb-6") ] [ HH.text "Seller/Buyer" ]
      , HH.div [ HP.classes [ Css.c "flex" ] ]
          [ title "Legal Entity"
          , renderSeller
          ]
      , HH.div [ HP.classes [ Css.c "flex" ] ]
          [ title "Customer"
          , renderBuyer
          ]
      , HH.div [ HP.classes [ Css.c "flex" ] ]
          [ title "Commercial"
          , renderCommercial
          ]
      ]
    where
    title t = HH.h4 [ HP.classes [ Css.c "w-40" ] ] [ HH.text t ]

    renderSeller =
      HH.slot Seller.proxy unit Seller.component
        ((\s -> { seller: s, readOnly: not isInDraft }) <$> orderForm.seller)
        SetSeller

    renderBuyer =
      HH.slot Buyer.proxy unit Buyer.component
        ((\b -> { buyer: b, readOnly: not isInDraft }) <$> orderForm.buyer)
        SetBuyer

    renderCommercial =
      HH.slot Commercial.proxy unit Commercial.component
        ( (\c cai -> { commercial: c, crmAccountId: cai, readOnly: not isInDraft })
            <$> orderForm.commercial
            <*> getCrmAccountId
        )
        SetCommercial
      where
      getCrmAccountId = orderForm.buyer >>= (\(SS.Buyer { crmAccountId }) -> crmAccountId)

  renderOrderFooter sof =
    HH.div
      [ HP.classes
          [ Css.c "flex"
          , Css.c "flex-wrap-reverse"
          , Css.c "items-center"
          , Css.c "space-x-4"
          ]
      ]
      [ renderOrderSummary sof.orderForm.summary
      , HH.div [ HP.class_ (Css.c "grow") ] []
      , if isFreshOrder then
          HH.button
            [ HP.class_ (Css.c "sofa-btn-destructive")
            , HP.disabled
                $ let
                    changed = sof.orderForm.changed

                    inDraft =
                      (sof.orderForm.status == SS.OsInDraft)
                        && isJust sof.orderForm.original
                  in
                    not (changed || inDraft)
            , HE.onClick $ \_ -> DiscardOrder
            ]
            [ HH.text "Discard order" ]
        else
          HH.button
            [ HP.class_ (Css.c "sofa-btn-primary")
            , HP.disabled preventFulfill
            , HE.onClick $ \_ -> FulfillOrder
            ]
            [ HH.text "Fulfill order"
            , if sof.orderFulfillInFlight then
                Widgets.spinner [ Css.c "ml-2", Css.c "align-text-bottom" ]
              else
                HH.text ""
            ]
      , HH.button
          [ HP.class_ (Css.c "sofa-btn-primary")
          , HP.disabled preventCreate
          , HE.onClick $ \_ -> CreateUpdateOrder
          ]
          [ HH.text $ maybe "Send order" (const "Update order") (getOrderId sof)
          , if sof.orderUpdateInFlight then
              Widgets.spinner [ Css.c "ml-2", Css.c "align-text-bottom" ]
            else
              HH.text ""
          ]
      ]
    where
    -- An order is in draft status or fresh (i.e., not yet created in the
    -- backend).
    isFreshOrder =
      sof.orderForm.status == SS.OsInDraft
        || isNothing sof.orderForm.original

    preventCreate =
      sof.orderUpdateInFlight
        || not sof.orderForm.changed
        || fromMaybe true do
            _ <- sof.orderForm.seller
            _ <- sof.orderForm.buyer
            _ <- sof.orderForm.commercial
            _ <- traverse checkOrderSection =<< sequence sof.orderForm.sections
            pure false
      where
      checkOrderSection os = do
        _ <- _.uri $ unwrap $ os.solution
        _ <- os.priceBook
        pure unit

    preventFulfill =
      sof.orderFulfillInFlight
        || maybe true (SS.OsInFulfillment /= _) (getOriginalOrderStatus sof)

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action Slots m)
  renderOrderForm sof =
    [ HH.div [ HP.classes [ Css.c "flex", Css.c "flex-col", Css.c "space-y-5" ] ]
        [ renderHeaderAndInfo sof.orderForm
        , renderSections sof sof.orderForm.sections
        , renderOrderFooter sof
        , HH.hr_
        , HH.details_
            [ HH.summary
                [ HP.class_ (Css.c "cursor-pointer") ]
                [ HH.text "Order Form JSON" ]
            , HH.pre_
                [ HH.code_
                    [ HH.text
                        $ either ("Cannot produce JSON: " <> _) identity
                        $ toJsonStr sof.orderForm
                    ]
                ]
            ]
        ]
    ]

  error err =
    [ HH.div
        [ HP.classes
            [ Css.c "p-5"
            , Css.c "bg-red-100"
            , Css.c "border"
            , Css.c "border-red-400"
            , Css.c "text-raspberry-500"
            ]
        ]
        [ HH.h3 [ HP.classes [ Css.c "text-lg" ] ] [ HH.text "Error" ]
        , HH.p_ [ HH.text err ]
        ]
    ]

  idle = [ HH.p_ [ HH.text "Idle …" ] ]

  loading =
    [ HH.p
        [ HP.classes [ Css.c "animate-pulse", Css.c "text-2xl" ] ]
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

  renderContent :: Array (H.ComponentHTML Action Slots m)
  renderContent =
    [ HH.h1_ [ HH.text "Order Form" ] ]
      <> case state of
          Initializing _ -> []
          Initialized state' -> defRender state' renderOrderForm

toJson :: OrderForm -> Either String SS.OrderForm
toJson orderForm = do
  commercial <- note "Missing commercial" $ orderForm.commercial
  buyer <- note "Missing buyer" $ orderForm.buyer
  seller <- note "Missing seller" $ orderForm.seller
  sections <-
    traverse toOrderSection
      =<< note "Incomplete order section" (sequence orderForm.sections)
  pure
    $ SS.OrderForm
        { id: (\(SS.OrderForm { id }) -> id) =<< orderForm.original
        , status: orderForm.status
        , approvalStatus: SS.OasUndecided
        , displayName: orderForm.displayName
        , crmQuoteId: orderForm.crmQuoteId
        , commercial
        , buyer
        , seller
        , orderObservers: orderForm.observers
        , orderNotes: orderForm.notes
        , sections
        , createTime: Nothing
        }
  where
  toOrderLine :: OrderLine -> SS.OrderLine
  toOrderLine ol =
    SS.OrderLine
      { orderLineId: ol.orderLineId
      , status: ol.status
      , statusReason: ol.statusReason
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

  toOrderSection :: OrderSection -> Either String SS.OrderSection
  toOrderSection os = do
    solutionUri <- note "Missing solution URI" $ _.uri $ unwrap $ os.solution
    basePriceBook <- note "Missing price book" $ toPriceBookRef solutionUri <$> os.priceBook
    orderLines <- sequence $ map (note "Incomplete order line" <<< map toOrderLine) os.orderLines
    pure
      $ SS.OrderSection
          { orderSectionId: os.orderSectionId
          , basePriceBook
          , orderLines
          }

toJsonStr :: OrderForm -> Either String String
toJsonStr = map (stringifyWithIndent 2 <<< encodeJson) <<< toJson

loadCatalog ::
  forall slots output m.
  MonadAff m =>
  Maybe SS.CrmQuoteId ->
  H.HalogenM State Action slots output m Unit
loadCatalog crmQuoteId = do
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
              , changed: false
              , displayName: Nothing
              , crmQuoteId
              , commercial: Nothing
              , buyer: Nothing
              , seller: Nothing
              , status: SS.OsInDraft
              , observers: []
              , notes: []
              , summary: mempty
              , sections: []
              }
          , configTabs: Map.empty
          , orderUpdateInFlight: false
          , orderFulfillInFlight: false
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
          $ FO.toUnfoldable x.properties
    in
      Just $ SS.CvObject defaults
  SS.CseOneOf { oneOf } -> case A.head oneOf of
    Just x -> mkDefaultConfig x
    Nothing -> Nothing

mkDefaultConfigs :: UUID -> SS.Product -> Array SS.OrderLineConfig
mkDefaultConfigs id (SS.Product p) =
  fromMaybe
    [ SS.OrderLineConfig
        { id: Just $ UUID.toString id
        , quantity: 1
        , config: Nothing
        }
    ]
    $ do
        schema <- p.orderConfigSchema
        default_ <- mkDefaultConfig schema
        pure
          [ SS.OrderLineConfig
              { id: Just $ UUID.toString id
              , quantity: 1
              , config: Just default_
              }
          ]

calcSubTotal :: OrderSection -> OrderSection
calcSubTotal os =
  os
    { orderLines = orderLines'
    , summary = sumOrderLines orderLines'
    }
  where
  defaultCurrency = maybe (SS.ChargeCurrency (unsafeMkCurrency "FIX")) _.currency os.priceBook

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

-- | Helper function to modify an indexed order section. The sub-total of the
-- | modified section is updated.
modifyOrderSection :: Int -> (OrderSection -> OrderSection) -> OrderForm -> OrderForm
modifyOrderSection secIdx updateOrderSection order =
  order
    { sections =
      fromMaybe order.sections
        $ modifyAt secIdx (map (calcSubTotal <<< updateOrderSection)) order.sections
    }

-- | Helper function to modify an indexed order line.
modifyOrderLine :: Int -> Int -> (OrderLine -> OrderLine) -> OrderForm -> OrderForm
modifyOrderLine secIdx olIdx updateOrderLine =
  modifyOrderSection secIdx \section ->
    section
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
            , changed: false
            , displayName: orderForm.displayName
            , crmQuoteId: orderForm.crmQuoteId
            , commercial: Just orderForm.commercial
            , buyer: Just orderForm.buyer
            , seller: Just orderForm.seller
            , status: orderForm.status
            , observers: orderForm.orderObservers
            , notes: orderForm.orderNotes
            , summary: mempty
            , sections: map (convertOrderSection productCatalog priceBooks) orderForm.sections
            }
      , configTabs: Map.empty
      , orderUpdateInFlight: false
      , orderFulfillInFlight: false
      }

  convertOrderSection :: SS.ProductCatalog -> Map String (Array PriceBook) -> SS.OrderSection -> Maybe OrderSection
  convertOrderSection (SS.ProductCatalog { solutions }) pbs (SS.OrderSection s) = do
    let
      SS.PriceBookRef pbRef = s.basePriceBook
    solution <-
      List.find (\(SS.Solution { uri }) -> pbRef.solutionUri == uri)
        $ Map.values solutions
    let
      SS.Solution sol = solution

      priceBook =
        if A.null sol.priceBooks then
          mkNilPriceBook solution
        else
          A.find (\pb -> pb.id == pbRef.priceBookId) =<< Map.lookup sol.id pbs
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
      , statusReason: l.statusReason
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

-- | Applies the given modification to the order form. After applying the
-- | modification, the totals are recalculated.
modifyOrderForm ::
  forall r.
  (OrderForm -> OrderForm) ->
  { orderForm :: OrderForm | r } ->
  { orderForm :: OrderForm | r }
modifyOrderForm f r = r { orderForm = calcTotal (f r.orderForm) { changed = true } }

-- | Finds all file IDs within the given section.
findSectionFileIds ::
  OrderSection ->
  Array String
findSectionFileIds section =
  A.concatMap findLineFileIds
    $ A.catMaybes section.orderLines

-- | Finds all file IDs within the given order line.
findLineFileIds ::
  OrderLine ->
  Array String
findLineFileIds orderLine = do
  SS.OrderLineConfig { config } <- orderLine.configs
  value <- maybe [] A.singleton config
  getFileIds value
  where
  fileTag = Just (SS.CvString "FILE_ATTACHMENT")

  getFileIds = case _ of
    SS.CvArray arr -> A.concatMap getFileIds arr
    SS.CvObject m
      | Map.lookup "type" m == fileTag -> case Map.lookup "fileId" m of
        Just (SS.CvString str) -> [ str ]
        _ -> []
      | otherwise -> A.concatMap getFileIds $ A.fromFoldable $ Map.values m
    _ -> []

-- | Deletes all given file attachments in parallel. Note, we ignore the result
-- | of these calls, i.e., fatalistically assume that they will succeed.
deleteFileAttachments ::
  forall slots output m.
  MonadAff m =>
  CredentialStore m =>
  Array String ->
  H.HalogenM State Action slots output m Unit
deleteFileAttachments fileIds =
  sequential
    $ for_ fileIds (parallel <<< H.lift <<< deleteFile)

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

mkNilPriceBook :: SS.Solution -> Maybe PriceBook
mkNilPriceBook solution = do
  let
    SS.Solution sol = solution
  -- If the solution has no price book then we'll assume this is intentional and
  -- simply invent an empty price book.
  guard (A.null sol.priceBooks)
  year <- toEnum 1970
  day <- toEnum 1
  currency <- mkCurrency "EUR"
  pure
    { id: ""
    , title: ""
    , version: canonicalDate year January day
    , currency: SS.ChargeCurrency currency
    , rateCards: Nothing
    }

handleAction ::
  forall output m.
  MonadAff m =>
  CredentialStore m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  Initialize -> do
    st <- H.get
    case st of
      Initializing NewOrder -> loadCatalog Nothing
      Initializing (ExistingOrder orderForm) -> loadExisting orderForm
      Initializing (ExistingOrderId id) -> do
        H.put $ Initialized Loading
        orderForm <- H.lift $ getOrder id
        case orderForm of
          Error err -> H.put $ Initialized (Error err)
          Idle -> H.put $ Initialized Idle
          Loaded order -> loadExisting order
          Loading -> H.put $ Initialized Loading
      Initializing (ExistingCrmQuoteId crmQuoteId) -> loadCatalog (Just crmQuoteId)
      _ -> pure unit
  SetOrderDisplayName name ->
    modifyInitialized
      $ \st ->
          st
            { orderForm =
              st.orderForm
                { changed = true
                , displayName =
                  let
                    sname = S.trim name
                  in
                    if sname == "" then Nothing else Just sname
                }
            }
  SetSeller seller -> do
    modifyInitialized
      $ \st ->
          st
            { orderForm =
              st.orderForm
                { changed = true
                , seller = Just seller
                }
            }
    H.tell Buyer.proxy unit (Buyer.ResetBuyer Nothing true)
    H.tell Commercial.proxy unit
      (Commercial.ResetCommercial { commercial: Nothing, crmAccountId: Nothing, enabled: false })
  SetBuyer buyer -> do
    modifyInitialized
      $ \st ->
          st
            { orderForm =
              st.orderForm
                { changed = true
                , buyer = Just buyer
                }
            }
    let
      SS.Buyer { crmAccountId } = buyer
    H.tell Commercial.proxy unit
      (Commercial.ResetCommercial { commercial: Nothing, crmAccountId, enabled: true })
  SetCommercial commercial ->
    modifyInitialized
      $ \st ->
          let
            currency = toPricingCurrency commercial

            priceBooks = mkPriceBooks st.productCatalog currency

            resetSection sec =
              sec
                { priceBook =
                  let
                    SS.Solution sol = sec.solution
                  in
                    if A.null sol.priceBooks then
                      mkNilPriceBook sec.solution
                    else
                      Nothing
                , summary = mempty :: SubTotal
                }
          in
            st
              { currency = currency
              , priceBooks = priceBooks
              , orderForm =
                st.orderForm
                  { changed = true
                  , commercial = Just commercial
                  --  If the currency changed then we can't use the same price
                  --  book, so the summary and all sections need to be updated.
                  , summary =
                    if st.currency == currency then
                      st.orderForm.summary
                    else
                      mempty
                  , sections =
                    if st.currency == currency then
                      st.orderForm.sections
                    else
                      map (map resetSection) st.orderForm.sections
                  }
              }
  SetObservers observers ->
    modifyInitialized
      $ modifyOrderForm _ { observers = observers }
  SetNotes notes ->
    modifyInitialized
      $ modifyOrderForm _ { notes = notes }
  SetOrderStatus status ->
    modifyInitialized
      $ modifyOrderForm _ { status = status }
  AddSection ->
    modifyInitialized
      $ modifyOrderForm \order -> order { sections = snoc order.sections Nothing }
  SectionSetSolution { sectionIndex, solutionId } ->
    modifyInitialized
      $ \st ->
          let
            SS.ProductCatalog pc = st.productCatalog
          in
            st
              { orderForm
                { changed = true
                , sections =
                  fromMaybe st.orderForm.sections
                    $ do
                        solution <- Map.lookup solutionId pc.solutions
                        modifyAt sectionIndex
                          ( \sec ->
                              Just
                                { orderSectionId: _.orderSectionId =<< sec
                                , solution: solution
                                , priceBook: mkNilPriceBook solution
                                , orderLines: [ Nothing ]
                                , summary: mempty
                                }
                          )
                          st.orderForm.sections
                }
              }
  SectionSetPriceBook { sectionIndex, priceBook } ->
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderSection sectionIndex _ { priceBook = priceBook }
  RemoveSection { sectionIndex } -> do
    state <- H.get
    maybe' pure deleteFileAttachments
      $ case state of
          Initialized (Loaded { orderForm: { sections } }) -> do
            section <- join $ A.index sections sectionIndex
            pure $ findSectionFileIds section
          _ -> Nothing
    modifyInitialized
      $ modifyOrderForm \order ->
          order
            { sections =
              fromMaybe order.sections $ A.deleteAt sectionIndex order.sections
            }
  AddOrderLine { sectionIndex } ->
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderSection sectionIndex \section ->
          section { orderLines = snoc section.orderLines Nothing }
  RemoveOrderLine { sectionIndex, orderLineIndex } -> do
    state <- H.get
    maybe' pure deleteFileAttachments
      $ case state of
          Initialized (Loaded { orderForm: { sections } }) -> do
            { orderLines } <- join $ A.index sections sectionIndex
            orderLine <- join $ A.index orderLines orderLineIndex
            pure $ findLineFileIds orderLine
          _ -> Nothing
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderSection sectionIndex \section ->
          section
            { orderLines =
              fromMaybe section.orderLines
                $ A.deleteAt orderLineIndex section.orderLines
            }
  OrderLineSetProduct { sectionIndex, orderLineIndex, product } ->
    let
      mkOrderLine :: UUID -> SS.Product -> OrderLine
      mkOrderLine configId prod =
        { orderLineId: Nothing
        , status: SS.OlsNew
        , statusReason: ""
        , product: prod
        , charges: Nothing
        , unitMap: Charge.productChargeUnitMap product
        , configs: mkDefaultConfigs configId product
        , estimatedUsage: Map.empty
        }

      updateOrderLine :: UUID -> Maybe OrderLine -> OrderLine
      updateOrderLine configId _ = mkOrderLine configId product

      -- | Build order lines for all required product options.
      requiredOptions :: UUID -> Map SS.SkuCode SS.Product -> Array (Maybe OrderLine)
      requiredOptions uuidNamespace solProds =
        let
          SS.Product p = product

          requiredSkuCode = case _ of
            SS.ProdOptSkuCode _ -> Nothing
            SS.ProductOption po -> if po.required then Just po.sku else Nothing

          requiredProds = A.mapMaybe (requiredSkuCode >=> (\o -> Map.lookup o solProds)) <$> p.options

          -- Since we don't have access to the Effect monad here we generate a
          -- v5 UUID instead. It's namespace is the ID of the originating order
          -- line.
          mkId i = UUID.genv5UUID (show i) uuidNamespace
        in
          maybe [] (A.mapWithIndex (\i -> Just <<< mkOrderLine (mkId i))) requiredProds

      updateOrderSection :: UUID -> OrderSection -> OrderSection
      updateOrderSection configId section =
        let
          solProds = SS.solutionProducts section.solution
        in
          calcSubTotal
            section
              { orderLines =
                maybe
                  section.orderLines
                  (\ls -> ls <> requiredOptions configId solProds)
                  ( do
                      ls <- modifyAt orderLineIndex (Just <<< updateOrderLine configId) section.orderLines
                      pure ls
                  )
              }
    in
      do
        configId <- H.liftEffect $ UUID.genUUID
        modifyInitialized
          $ modifyOrderForm
          $ modifyOrderSection sectionIndex (updateOrderSection configId)
  OrderLineSetQuantity { sectionIndex, orderLineIndex, configIndex, quantity } ->
    let
      updateOrderConfig :: SS.OrderLineConfig -> SS.OrderLineConfig
      updateOrderConfig (SS.OrderLineConfig olc) = SS.OrderLineConfig $ olc { quantity = quantity }

      -- | Remove the configuration entry. If this is the last entry then we
      -- | ignore the request.
      updateOrderLine :: UUID -> OrderLine -> OrderLine
      updateOrderLine configId ol =
        ol
          { configs =
            if A.null ol.configs then
              [ SS.OrderLineConfig
                  { id: Just $ UUID.toString configId
                  , quantity
                  , config: Nothing
                  }
              ]
            else
              fromMaybe ol.configs $ A.modifyAt configIndex updateOrderConfig ol.configs
          }
    in
      do
        configId <- H.liftEffect $ UUID.genUUID
        modifyInitialized
          $ modifyOrderForm
          $ modifyOrderLine sectionIndex orderLineIndex (updateOrderLine configId)
  OrderLineAddConfig { sectionIndex, orderLineIndex } -> do
    configId <- H.liftEffect $ UUID.genUUID
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderLine sectionIndex orderLineIndex \ol ->
          ol { configs = ol.configs <> mkDefaultConfigs configId ol.product }
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
      modifyInitialized $ modifyOrderForm
        $ modifyOrderLine sectionIndex orderLineIndex updateOrderLine
  OrderLineSetConfig { sectionIndex, orderLineIndex, configIndex, alter } ->
    let
      alterConfig (SS.OrderLineConfig olc) =
        SS.OrderLineConfig
          $ olc { config = Just $ alter olc.config }

      updateOrderLine :: UUID -> OrderLine -> OrderLine
      updateOrderLine configId ol =
        ol
          { configs =
            fromMaybe
              [ SS.OrderLineConfig
                  { id: Just $ UUID.toString configId
                  , quantity: 1
                  , config: Just $ alter Nothing
                  }
              ]
              $ A.modifyAt configIndex alterConfig ol.configs
          }
    in
      do
        configId <- H.liftEffect $ UUID.genUUID
        modifyInitialized
          $ modifyOrderForm
          $ modifyOrderLine sectionIndex orderLineIndex (updateOrderLine configId)
  OrderLineSetConfigTab entryIdx tabIdx ->
    modifyInitialized \orderForm ->
      orderForm { configTabs = Map.insert entryIdx tabIdx orderForm.configTabs }
  OrderLineSetCharges { sectionIndex, orderLineIndex, charges, estimatedUsage } ->
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderLine sectionIndex orderLineIndex
          _
            { charges = Just charges
            , estimatedUsage = estimatedUsage
            }
  DiscardOrder -> do
    state <- H.get
    case state of
      -- If the order exists in the backend then we'll also delete it there, but
      -- only if the order is in the draft status.
      Initialized
        ( Loaded
          { orderForm:
            { original: Just (SS.OrderForm { id: Just orderId })
            , status: SS.OsInDraft
            }
        }
      ) -> do
        result <- H.lift $ deleteOrder orderId
        case result of
          Error msg -> H.liftEffect $ Console.error $ "Error deleting order: " <> msg
          Loaded _ -> H.liftEffect $ Console.log $ "Deleted order: " <> show orderId
          _ -> pure unit
        pure unit
      _ -> pure unit
    -- Reloading the catalog will reset the state.
    loadCatalog Nothing
  CreateUpdateOrder -> do
    st <- H.get
    let
      -- Updates the current state to match the response order object.
      ld o = case o of
        Loaded o' -> loadExisting o'
        _ -> modifyInitialized $ _ { orderUpdateInFlight = false }

      run json =
        maybe'
          (\_ -> postOrder json)
          (\id -> patchOrder id json)
    case st of
      Initialized (Loaded st') -> case toJson st'.orderForm of
        Left msg -> do
          H.liftEffect $ Console.error $ "Could not produce order JSON: " <> msg
          pure unit
        Right json -> do
          modifyInitialized $ _ { orderUpdateInFlight = true }
          order <- H.lift $ run json (getOrderId st')
          ld order
      _ -> pure unit
  FulfillOrder -> do
    st <- H.get
    let
      -- Updates the current state to match the response order object.
      ld o = case o of
        Loaded o' -> loadExisting o'
        _ -> modifyInitialized $ _ { orderFulfillInFlight = false }
    case st of
      Initialized
        ( Loaded
          { orderForm: { original: Just (SS.OrderForm { id: Just id }) }
        }
      ) -> do
        modifyInitialized $ _ { orderFulfillInFlight = true }
        order <- H.lift $ postOrderFulfillment id
        ld order
      _ -> pure unit
