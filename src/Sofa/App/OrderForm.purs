-- | The main order form component. This shows and manages updates of a single
-- | order. Due to its complexity we've split this component into a bunch of
-- | sub-components.
module Sofa.App.OrderForm (Slot, Input(..), proxy, component) where

import Prelude
import Control.Alternative (guard, (<|>))
import Control.Parallel (parallel, sequential)
import Data.Argonaut (decodeJson, encodeJson, jsonParser, printJsonDecodeError, stringify, stringifyWithIndent)
import Data.Array (foldl, modifyAt, snoc)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Date (Date, Month(..), canonicalDate)
import Data.Either (Either(..), either, isLeft, note)
import Data.Enum (toEnum)
import Data.Int as Int
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe')
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String as S
import Data.Traversable (for_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (tuple3, uncurry3)
import Data.UUID (UUID, emptyUUID, genUUID, genv5UUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.App.Charge (Slot, component, proxy) as Charge
import Sofa.App.OrderForm.AssetModal as AssetModal
import Sofa.App.OrderForm.Buyer as Buyer
import Sofa.App.OrderForm.Commercial as Commercial
import Sofa.App.OrderForm.ConfigSchema as ConfigSchema
import Sofa.App.OrderForm.ConfirmFulfillModal as ConfirmFulfillModal
import Sofa.App.OrderForm.Notes as Notes
import Sofa.App.OrderForm.Observers as Observers
import Sofa.App.OrderForm.SelectProduct as SelectProduct
import Sofa.App.OrderForm.Seller as Seller
import Sofa.App.OrderForm.Widget.AssetConfigLink (SkuConfigs)
import Sofa.App.Requests as Requests
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
import Sofa.Component.Card as Card
import Sofa.Component.EditableInput as EditableInput
import Sofa.Component.Icon as Icon
import Sofa.Component.Select as Select
import Sofa.Component.Spinner as Spinner
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Charge (ChargeUnitMap, dims, productChargeUnitMap, unitIds) as Charge
import Sofa.Data.Currency (mkCurrency, unsafeMkCurrency)
import Sofa.Data.Deployment (class MonadDeployment, isBuyerFixed)
import Sofa.Data.IEId (IEId(..), genInternalId, genInternalId', toExternalId, toRawId)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.Quantity (QuantityMap, Quantity, fromSmartSpecQuantity, toSmartSpecQuantity)
import Sofa.Data.Route as Route
import Sofa.Data.Schema (mkDefaultConfig)
import Sofa.Data.Schema as Schema
import Sofa.Data.SmartSpec as SS
import Sofa.Data.SubTotal (SubTotal)
import Sofa.Data.SubTotal as SubTotal
import Sofa.HtmlUtils (scrollToElement, back, addClassToElement, removeClassToElement)
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))
import Web.Event.Event (stopPropagation) as Event
import Web.HTML as Html
import Web.HTML.History as HtmlHistory
import Web.HTML.Window as HtmlWindow
import Web.Storage.Storage as HtmlStorage
import Web.UIEvent.MouseEvent (MouseEvent, toEvent) as Event

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
    , selectSolution :: Select.Slot OrderSectionId String -- Output is solution ID.
    , selectPriceBook :: Select.Slot OrderSectionId Int -- Output is price book index.
    , selectProduct :: SelectProduct.Slot OrderLineFullId
    , productConfig :: ConfigSchema.Slot ConfigId
    , charge :: Charge.Slot OrderLineFullId
    , orderName :: EditableInput.Slot Unit
    , assetModal :: AssetModal.Slot OrderLineFullId
    , confirmFulfillModal :: ConfirmFulfillModal.Slot Unit
    )

-- | The order form component input. We can either start an entirely new order
-- | or use an existing order. The existing order can either be given directly
-- | or indirectly using an order or quote ID (which will be fetched from the
-- | backend).
data Input
  = NewOrder
  | ExistingOrder SS.OrderForm
  | ExistingOrderId SS.OrderId
  | ExistingCrmQuoteId SS.CrmQuoteId
  | NewOrderCrmAccountId SS.CrmAccountId
  | SalesforceNewOrder
    { buyer :: SS.Buyer
    , contacts :: Array SS.Contact
    , billingAccountId :: SS.BillingAccountId
    , legalEntityRegisteredName :: String
    }

data State
  = Initializing Input
  | Initialized (Loadable StateOrderForm)

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
    , currency :: Maybe SS.PricingCurrency
    , priceBooks :: Map String (Array PriceBook)
    -- ^ Map from solution title to price books in the current currency.
    , orderForm :: OrderForm
    , orderUpdateInFlight :: Boolean -- ^ Whether a current order update request is in flight.
    , orderFulfillStatus :: OrderFulfillStatus
    , assetModalOpen :: Maybe OrderLineId
    , assets :: Loadable (Array SS.AssetConfig)
    , crmAccountId :: Maybe SS.CrmAccountId
    }

-- | The order fulfillment status. This indicates in which state the order
-- | fulfillment is. Most importantly, it allows us to distinguish between "no
-- | active fulfillment", "waiting for fulfillment confirmation", and "waiting
-- | for fulfill request completion".
data OrderFulfillStatus
  = FulfillStatusIdle
  | FulfillStatusConfirming
  | FulfillStatusInFlight

-- Similar to SS.OrderForm but with a few optional fields.
type OrderForm
  = { original :: Maybe SS.OrderForm -- ^ The original order form, if one exists.
    , changed :: Boolean -- ^ Whether this order has unsaved changes.
    , crmQuoteId :: Maybe SS.CrmQuoteId
    , billingAccountId :: Maybe SS.BillingAccountId
    , commercial :: Maybe SS.Commercial
    , buyer :: Maybe SS.Buyer
    , fixedBuyer :: Boolean
    -- ^ Whether the buyer is fixed, when `false` then the user is allowed to
    -- choose a different buyer. Note, this also applies to the billing account,
    -- i.e., if the buyer is fixed then so is the billing account.
    , buyerAvailableContacts :: Maybe (Array SS.Contact)
    -- ^ The buyer contacts, if available. When nothing then we fetch the
    -- contacts from the ordering backend.
    , legalEntityRegisteredName :: Maybe String
    , seller :: Maybe SS.Seller
    , displayName :: Maybe String
    , status :: SS.OrderStatus
    , observers :: Array SS.OrderObserver
    , notes :: Array SS.OrderNote
    , orderTotal :: SubTotal
    , sections :: Array OrderSection
    }

type OrderSection
  = { orderSectionId :: OrderSectionId
    , solution :: Maybe SS.Solution
    , priceBook :: Maybe PriceBook
    , orderLines :: Array OrderLine
    -- ^ Order lines of the product options.
    , subTotal :: SubTotal
    }

type OrderLine
  = { orderLineId :: OrderLineId
    , status :: SS.OrderLineStatus
    , statusReason :: String
    , product :: Maybe SS.Product
    , charges :: Maybe (Array SS.Charge)
    , unitMap :: Charge.ChargeUnitMap
    , configs :: NonEmptyArray SS.OrderLineConfig
    , estimatedUsage :: QuantityMap
    }

type PriceBook
  = { id :: String
    , title :: String
    , version :: Date
    , currency :: SS.ChargeCurrency -- ^ The default charge currency.
    , rateCards :: Maybe (Map SS.SkuCode SS.RateCard) -- ^ Maybe a map from SKU to rate cards.
    }

type OrderSectionId
  = IEId SS.OrderSectionId

type OrderLineId
  = IEId SS.OrderLineId

type OrderLineFullId
  = { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    }

type ConfigId
  = { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    , configId :: SS.OrderLineConfigId
    }

data Action
  = Initialize
  | SetOrderDisplayName String
  | SetSeller SS.Seller
  | SetBuyer SS.Buyer
  | SetCommercial SS.BillingAccount
  | SetObservers (Array SS.OrderObserver)
  | SetNotes (Array SS.OrderNote)
  | SetOrderStatus SS.OrderStatus
  | GotoSection { orderSectionId :: OrderSectionId }
  | GotoOrderLine
    { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    }
  | AddSection
  | SectionSetSolution
    { orderSectionId :: OrderSectionId
    , solutionId :: String
    }
  | SectionSetPriceBook
    { orderSectionId :: OrderSectionId
    , priceBook :: Maybe PriceBook
    }
  | RemoveSection { orderSectionId :: OrderSectionId } Event.MouseEvent
  | AddOrderLine { orderSectionId :: OrderSectionId }
  | AddOrderLineForProducts
    { orderSectionId :: OrderSectionId
    , skus :: Array SS.SkuCode
    }
  | OrderLineSetProduct
    { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    , product :: SS.Product
    }
  | OrderLineSetQuantity
    { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    , configId :: SS.OrderLineConfigId
    , quantity :: Int
    }
  | OrderLineAddConfig
    { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    }
  | OrderLineSetConfig
    { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    , configId :: SS.OrderLineConfigId
    , configValue :: Maybe SS.ConfigValue
    }
  | OrderLineRemoveConfig
    { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    , configId :: SS.OrderLineConfigId
    }
  | OrderLineSetCharges
    { orderSectionId :: OrderSectionId
    , orderLineId :: OrderLineId
    , charges :: Array SS.Charge
    , estimatedUsage :: QuantityMap
    }
  | RemoveOrderLine
    { orderSectionId :: OrderSectionId, orderLineId :: OrderLineId }
    Event.MouseEvent
  | DiscardOrder -- ^ Discard the currently loaded order.
  | CreateUpdateOrder -- ^ Create or update the current order.
  | FulfillOrderStart -- ^ Show modal for user to confirm order fulfillment.
  | FulfillOrderModalResult ConfirmFulfillModal.Output
  | Back Event.MouseEvent

component ::
  forall query output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ renderContent ]
  where
  -- For some reason this doesn't seem to be predefined in Halogen.
  propOpen = HP.prop (HH.PropName "open")

  renderOrderLineStatus status reason =
    let
      wrap content
        | reason == "" = HH.text content
        | otherwise =
          Tooltip.render
            (Tooltip.defaultInput { text = reason })
            (Icon.textWithTooltip content)

      color = case status of
        SS.OlsNew -> "bg-informative-200"
        SS.OlsAccepted -> "bg-informative-200"
        SS.OlsStatusUnspecified -> "bg-informative-200"
        SS.OlsSucceeded -> "bg-success-200"
        SS.OlsFailed -> "bg-error-200"
        SS.OlsCancelled -> "bg-informative-200"
    in
      HH.span
        [ Css.classes [ "nectary-tag", "rounded-sm", "w-fit", color ] ]
        [ wrap (SS.prettyOrderLineStatus status) ]

  renderCharges ::
    OrderLineFullId ->
    Charge.ChargeUnitMap ->
    SS.ChargeCurrency ->
    QuantityMap ->
    Array SS.Charge ->
    H.ComponentHTML Action Slots m
  renderCharges olId unitMap defaultCurrency estimatedUsage charges =
    HH.slot
      Charge.proxy
      olId
      Charge.component
      { unitMap
      , defaultCurrency
      , charges
      , estimatedUsage
      , priceOnly: false
      , readOnly: not isInDraft
      }
      ( \result ->
          OrderLineSetCharges
            { orderSectionId: olId.orderSectionId
            , orderLineId: olId.orderLineId
            , charges: result.charges
            , estimatedUsage: result.estimatedUsage
            }
      )

  renderChargeDetails ::
    OrderLineFullId ->
    Charge.ChargeUnitMap ->
    SS.ChargeCurrency ->
    QuantityMap ->
    Maybe (Array SS.Charge) ->
    H.ComponentHTML Action Slots m
  renderChargeDetails olId unitMap defaultCurrency estimatedUsage = maybe empty withCharges
    where
    empty = HH.text ""

    withCharges = case _ of
      [] -> empty
      charges ->
        HH.details_
          [ HH.summary
              [ Css.classes [ "text-lg", "cursor-pointer" ] ]
              [ HH.text "Charges" ]
          , renderCharges olId unitMap defaultCurrency estimatedUsage charges
          ]

  renderOrderLine ::
    SS.Solution ->
    SS.ChargeCurrency ->
    OrderSectionId ->
    Int ->
    OrderLine ->
    Tuple String (H.ComponentHTML Action Slots m)
  renderOrderLine (SS.Solution sol) defaultCurrency orderSectionId olIdx ol = case ol.product of
    Nothing ->
      body
        [ HH.h2_ [ HH.text "Product" ]
        , renderSelectProduct Nothing
        , renderRemoveOrderLineButton
        ]
    Just (SS.Product product) ->
      let
        olId :: OrderLineFullId
        olId = { orderSectionId, orderLineId: ol.orderLineId }

        renderProductTitle = HH.text $ fromMaybe (show product.sku) product.title
      in
        body
          $ [ HH.div [ Css.classes [ "flex", "flex-wrap", "gap-8", "items-center" ] ]
                [ HH.h3 [ Css.classes [ "grow" ] ]
                    [ HH.text "Product "
                    , HH.text $ show $ olIdx + 1
                    , HH.text " – "
                    , renderProductTitle
                    ]
                , HH.div [ Css.classes [ "flex", "gap-4", "items-center" ] ]
                    [ HH.span [ Css.class_ "font-semibold" ] [ HH.text "Status" ]
                    , renderOrderLineStatus ol.status ol.statusReason
                    ]
                -- , HH.label [ Css.classes [ "flex", "gap-4", "items-center" ] ]
                --     [ HH.div [ Css.class_ "font-semibold" ] [ HH.text "Quantity" ]
                --     , renderQuantityInput $ NA.head ol.configs
                --     ]
                ]
            , renderSelectProduct ol.product
            , renderProductOptions product
            , renderChargeDetails olId ol.unitMap defaultCurrency ol.estimatedUsage ol.charges
            , if isNothing product.orderConfigSchema then
                HH.text ""
              else
                HH.details
                  [ propOpen true ]
                  $ [ HH.summary
                        [ Css.classes [ "text-lg", "cursor-pointer" ] ]
                        [ HH.text "Configuration" ]
                    ]
                  <> renderProductConfigs product ol.configs
            , renderRemoveOrderLineButton
            ]
    where
    refLabel = orderLineRefLabel orderSectionId ol.orderLineId

    body subBody =
      Tuple (unwrap refLabel)
        $ HH.div
            [ Css.classes
                [ "p-6"
                , "border"
                , "border-snow-800"
                , "rounded-lg"
                , "flex"
                , "flex-col"
                , "gap-5"
                ]
            , HP.ref refLabel
            , HP.id $ unwrap refLabel
            ]
            subBody

    renderRemoveOrderLineButton
      | not isInDraft = HH.text ""
      | otherwise =
        HH.div [ Css.class_ "flex" ]
          [ HH.div [ Css.class_ "grow" ] []
          , HH.button
              [ Css.classes [ "nectary-btn-destructive", "h-8", "px-6", "gap-4" ]
              , HE.onClick
                  $ RemoveOrderLine
                      { orderSectionId
                      , orderLineId: ol.orderLineId
                      }
              ]
              [ HH.text "Discard product"
              ]
          ]

    isOptionOnly (SS.Product { optionOnly }) = optionOnly

    -- | Render the select product component. Note, we rendering it only if
    -- |
    -- | - the order is in draft status and
    -- |
    -- | - the order line product is a top-level one (not an option one).
    renderSelectProduct selected
      | not isInDraft = HH.text ""
      | maybe false isOptionOnly selected = HH.text ""
      | otherwise =
        HH.slot
          SelectProduct.proxy
          { orderSectionId, orderLineId: ol.orderLineId }
          SelectProduct.component
          { name: unwrap refLabel
          , selected: (\(SS.Product { sku }) -> sku) <$> selected
          , products:
              A.filter
                (\(SS.Product { optionOnly }) -> not optionOnly)
                sol.products
          }
          ( \product ->
              OrderLineSetProduct
                { orderSectionId
                , orderLineId: ol.orderLineId
                , product
                }
          )

    renderProductOptions { options: mOptions, features: mFeatures }
      | not isInDraft = HH.text ""
      | otherwise = rendered
        where
        rendered = case Tuple renderedOptions renderedFeatures of
          Tuple [] [] -> HH.text ""
          _ ->
            HH.details
              [ propOpen true ]
              [ HH.summary
                  [ Css.classes [ "text-lg", "cursor-pointer" ] ]
                  [ HH.text "Product features / options" ]
              , HH.div
                  [ Css.classes
                      [ "my-4"
                      , "grid"
                      , "grid-cols-1"
                      , "lg:grid-cols-2"
                      , "xl:grid-cols-3"
                      , "gap-5"
                      ]
                  ]
                  ( renderedFeatures
                      <> separator
                      <> renderedOptions
                  )
              ]

        -- Include separator if we have both features and options.
        separator
          | A.null renderedFeatures || A.null renderedOptions = []
          | otherwise = [ HH.hr [ Css.class_ "col-span-full" ] ]

        -- Keeps the options in the input array that are not referenced by a
        -- product feature.
        filterStandaloneOptions :: Array SS.ProductOption -> Array SS.ProductOption
        filterStandaloneOptions options = case mFeatures of
          Nothing -> options
          Just [] -> options
          Just features ->
            let
              optionSkus (SS.ProductFeature { options: skus }) = skus

              featOpts = Set.fromFoldable $ A.concatMap optionSkus features

              getSku = case _ of
                SS.ProdOptSkuCode sku -> sku
                SS.ProductOption { sku } -> sku
            in
              A.filter (\option -> not $ Set.member (getSku option) featOpts) options

        renderedOptions =
          A.concatMap renderProductOption
            $ filterStandaloneOptions
            $ fromMaybe [] mOptions

        renderedFeatures =
          let
            renderFeatureButton (SS.ProductFeature { title, description, options }) =
              renderOptionButton
                (fromMaybe "Untitled" title)
                description
                options
          in
            map renderFeatureButton $ fromMaybe [] mFeatures

    renderProductOption = case _ of
      SS.ProdOptSkuCode sku ->
        [ renderOptionButton
            (optionLabel sku)
            (optionDescription sku)
            [ sku ]
        ]
      SS.ProductOption { sku, title, required: false } ->
        [ renderOptionButton
            (fromMaybe' (\_ -> optionLabel sku) title)
            (optionDescription sku)
            [ sku ]
        ]
      _ -> []
      where
      optionLabel sku =
        fromMaybe (show sku) do
          SS.Product product <-
            A.find
              (\(SS.Product { sku: sku' }) -> sku == sku')
              sol.products
          product.title

      optionDescription sku = do
        SS.Product product <-
          A.find
            (\(SS.Product { sku: sku' }) -> sku == sku')
            sol.products
        product.description

    renderOptionButton title description skus =
      Card.renderButton
        { title: HH.text title
        , body: maybe [] (A.singleton <<< HH.text) description
        , onClick: \_ -> AddOrderLineForProducts { orderSectionId, skus }
        }

    renderQuantityInput (SS.OrderLineConfig olc)
      | not isInDraft = HH.text $ show olc.quantity
      | otherwise =
        HH.input
          [ Css.classes
              [ "nectary-input"
              , "nectary-input-number"
              , "w-24"
              ]
          , HP.type_ HP.InputNumber
          , HP.min 1.0
          , HP.value $ show olc.quantity
          , HE.onValueChange
              $ \v ->
                  OrderLineSetQuantity
                    { orderSectionId
                    , orderLineId: ol.orderLineId
                    , configId: fromMaybe (SS.OrderLineConfigId "") olc.id
                    , quantity: fromMaybe olc.quantity $ Int.fromString v
                    }
          ]

    renderProductConfigs product configs =
      let
        allowRemove = NA.length configs > 1 && isInDraft
      in
        A.concat
          $ renderProductConfig allowRemove product
          <$> NA.toArray configs

    renderProductConfig allowRemove product (SS.OrderLineConfig { id: configId, config }) =
      [ HH.div [ Css.classes [ "mt-3", "p-5", "bg-snow-500", "rounded-lg" ] ]
          [ if allowRemove then
              HH.button
                [ Css.classes
                    [ "relative"
                    , "float-right"
                    , "nectary-btn-destructive"
                    , "h-auto"
                    , "ml-2"
                    , "py-0"
                    ]
                , HE.onClick \_ ->
                    OrderLineRemoveConfig
                      { orderSectionId
                      , orderLineId: ol.orderLineId
                      , configId: fromMaybe (SS.OrderLineConfigId "") configId
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
                  [ Css.classes
                      [ "relative"
                      , "float-right"
                      , "ml-2"
                      , "text-stormy-300"
                      ]
                  ]
                  [ Tooltip.render
                      ( Tooltip.defaultInput
                          { text = "Configuration ID: " <> show id
                          , orientation = Tooltip.Left
                          , width = Just "20rem"
                          }
                      )
                      (HH.text "#")
                  ]
          , case Tuple config configId of
              Tuple (Just c) (Just cId) ->
                maybe (HH.text "")
                  ( \schema ->
                      HH.slot
                        (Proxy :: Proxy "productConfig")
                        { orderSectionId
                        , orderLineId: ol.orderLineId
                        , configId: cId
                        }
                        ConfigSchema.component
                        { orderLineId: toExternalId ol.orderLineId
                        , configValue: c
                        , schemaEntry: schema
                        , readOnly: not isInDraft
                        , dataSourceVars:
                            { getCommercial:
                                \_ -> case state of
                                  Initialized (Loaded { orderForm: { commercial } }) -> commercial
                                  _ -> Nothing
                              , getBuyer :
                                \_ -> case state of
                                  Initialized (Loaded { orderForm: { buyer } }) -> buyer
                                  _ -> Nothing
                            }
                        , getConfigs: orderSchemaGetConfigs state orderSectionId
                        }
                        ( \configValue ->
                            OrderLineSetConfig
                              { orderSectionId
                              , orderLineId: ol.orderLineId
                              , configId: cId
                              , configValue
                              }
                        )
                  )
                  product.orderConfigSchema
              _ -> HH.text ""
          ]
      ]

  renderSection ::
    StateOrderForm ->
    OrderSection ->
    Tuple String (H.ComponentHTML Action Slots m)
  renderSection sof sec = case sec.solution of
    Nothing ->
      body
        [ HH.h2 [ Css.class_ "my-0" ] [ HH.text "Category" ]
        , renderSelectSolution Nothing
        , HH.div [ Css.class_ "flex" ]
            [ HH.div [ Css.class_ "grow" ] []
            , renderRemoveSectionButton
            ]
        ]
    Just solution@(SS.Solution sol) ->
      let
        priceBooks = fromMaybe [] $ Map.lookup sol.id sof.priceBooks

        priceBookOpts =
          let
            mkPriceBookOption i pb =
              Tuple
                (HH.div [HP.title $ pb.title <> " (" <> SS.prettyDate pb.version <> ")"] [HH.text $ pb.title <> " (" <> SS.prettyDate pb.version <> ")"])
                i
          in
            A.mapWithIndex mkPriceBookOption priceBooks
      in
        body
          $ [ HH.div [ Css.classes [ "flex", "flex-wrap", "gap-4", "items-end" ] ]
                [ HH.div [ Css.class_ "grow" ]
                    [ HH.h2
                        [ Css.class_ "my-0" ]
                        [ HH.text "Category – "
                        , HH.text $ solutionLabel solution
                        ]
                    ]
                , if A.null priceBookOpts then
                    HH.text ""
                  else
                    HH.label_
                      [ HH.div [ Css.class_ "font-semibold" ] [ HH.text "Price book" ]
                      , HH.slot
                          (Proxy :: Proxy "selectPriceBook")
                          sec.orderSectionId
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
            , renderSelectSolution (Just solution)
            , renderOrderLines solution sec.orderLines
            , HH.div
                [ Css.classes [ "flex", "items-center", "gap-4" ] ]
                [ renderOrderSectionSubTotal sec.subTotal
                , HH.div [ Css.class_ "grow" ] []
                , renderRemoveSectionButton
                , renderAddProductButton
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
      Tuple (unwrap refLabel)
        $ HH.div
            [ Css.classes [ "flex", "flex-col", "gap-8", "p-6", "rounded-md", "bg-snow-100" ]
            , HP.ref refLabel
            ]
            subBody
      where
      refLabel = sectionRefLabel sec.orderSectionId

    solutionLabel (SS.Solution s) = fromMaybe s.id s.title

    actionSetSolution solId =
      SectionSetSolution
        { orderSectionId: sec.orderSectionId
        , solutionId: solId
        }

    actionSetPriceBook priceBooks i =
      ( \pb ->
          SectionSetPriceBook
            { orderSectionId: sec.orderSectionId
            , priceBook: pb
            }
      )
        $ A.index priceBooks i

    renderRemoveSectionButton
      | not isInDraft = HH.text ""
      | otherwise =
        HH.button
          [ Css.classes [ "nectary-btn-destructive", "h-8" ]
          , HE.onClick $ RemoveSection { orderSectionId: sec.orderSectionId }
          ]
          [ HH.text "Discard category"
          ]

    renderAddProductButton =
      if not isInDraft || isAddingOrderLine then
        HH.text ""
      else
        HH.button
          [ Css.classes [ "nectary-btn-secondary", "h-8" ]
          , HE.onClick \_ -> AddOrderLine { orderSectionId: sec.orderSectionId }
          ]
          [ HH.text "Add product"
          ]
      where
      -- We're in the process of adding an order line if there is a order line
      -- that doesn't have a product.
      isAddingOrderLine = A.any (\{ product } -> isNothing product) sec.orderLines

    renderSelectSolution selectedSolution
      | not isInDraft = HH.text ""
      | otherwise =
        HH.div
          [ Css.classes
              [ "grid"
              , "grid-cols-1"
              , "lg:grid-cols-2"
              , "xl:grid-cols-3"
              , "gap-5"
              ]
          ]
          $ A.fromFoldable
          $ renderSolutionButton
          <$> availableSolutions
        where
        selectedSolId = (\(SS.Solution { id }) -> id) <$> selectedSolution

        renderSolutionButton { solution, available } =
          if not (available || isSelected) then
            HH.text ""
          else
            Card.renderRadio
              { title: HH.text $ solutionLabel solution
              , body: maybe [] (A.singleton <<< HH.text) description
              , name: "selsol-" <> show (toRawId sec.orderSectionId)
              , selected: isSelected
              , enabled: available || isSelected
              , onChange: \_ -> actionSetSolution id
              }
          where
          SS.Solution { id, description } = solution

          isSelected = selectedSolId == Just id

        -- Filters out solution identifiers that are already used for other
        -- order sections.
        availableSolutions :: List { solution :: SS.Solution, available :: Boolean }
        availableSolutions =
          let
            notSameSolutionId id' { solution } =
              maybe
                true
                (\(SS.Solution { id }) -> id' /= id)
                solution

            toResult (Tuple id solution) =
              { solution
              , available: A.all (notSameSolutionId id) sof.orderForm.sections
              }
          in
            map toResult $ Map.toUnfoldable pc.solutions

    renderOrderLines sol orderLines =
      -- Need a keyed div since otherwise the references seem to be ignored.
      -- Probably related to
      -- https://github.com/purescript-halogen/purescript-halogen/issues/423.
      HK.div
        [ Css.classes [ "flex", "flex-col", "gap-6" ] ]
        (A.mapWithIndex renderOrderLine' orderLines)
      where
      renderOrderLine' olIdx =
        renderOrderLine
          sol
          defaultCurrency
          sec.orderSectionId
          olIdx

  renderSectionSummary ::
    StateOrderForm ->
    H.ComponentHTML Action Slots m
  renderSectionSummary sof =
    HH.div
      [ Css.classes
          [ "p-6"
          , "rounded-md"
          , "bg-snow-100"
          ]
      ]
      [ HH.div [ Css.classes [ "flex", "items-center", "mb-6" ] ]
          [ HH.h2 [ Css.classes [ "grow", "m-0" ] ] [ HH.text "Order summary" ]
          , if not isInDraft then
              HH.text ""
            else
              HH.button
                [ Css.classes [ "nectary-btn-primary", "h-8" ]
                , HE.onClick \_ -> AddSection
                ]
                [ HH.text "Add category"
                ]
          ]
      , HH.table [ Css.classes [ "table-auto", "w-full" ] ]
          [ HH.thead_
              [ HH.tr [ Css.classes [ "border-b-2", "border-stormy-500" ] ]
                  [ th [ "w-full" ] [ HH.text "Name" ]
                  , th [ "px-5" ] [ HH.text "Status" ]
                  -- , th [ "px-5" ] [ HH.text "Quantity" ]
                  , th [] [ HH.text "Asset" ]
                  , if isInDraft then th [] [ HH.text "Remove" ] else HH.text ""
                  ]
              ]
          , HH.tbody_
              $ if A.null sectionsWithSolution then
                  [ HH.tr_
                      [ HH.td
                          [ HP.colSpan 4
                          , Css.classes [ "p-2", "text-stormy-300", "text-center" ]
                          ]
                          [ HH.text "No category added" ]
                      ]
                  ]
                else
                  A.concat $ sectionRow <$> sectionsWithSolution
          ]
      ]
    where
    sectionsWithSolution = A.filter (\{ solution } -> isJust solution) sof.orderForm.sections

    th cls =
      HH.th
        [ Css.classes $ [ "p-2", "font-semibold", "text-left" ] <> cls
        ]

    tdDelete onClick
      | not isInDraft = HH.text ""
      | otherwise =
        HH.td [ Css.class_ "text-center" ]
          [ HH.button
              [ Css.classes [ "p-2", "fill-error-500", "hover:fill-error-800" ]
              , HE.onClick onClick
              ]
              [ Icon.delete
                  [ Icon.classes [ Css.c "w-5" ]
                  , Icon.ariaLabel "Delete"
                  ]
              ]
          ]

    tdAsset orderLineFullId statusReason = case _ of
      Nothing ->
        HH.td [ Css.class_ "text-center" ]
          [ HH.text "" ]
      Just oId ->
        HH.td [ Css.class_ "text-center" ]
          [ HH.slot_ (Proxy :: Proxy "assetModal") orderLineFullId AssetModal.component { statusReason, orderId: oId }
          ]

    sectionRow { orderSectionId, solution, orderLines } = case solution of
      Nothing -> [ HH.text "" ]
      Just (SS.Solution sol) ->
        [ HH.tr
            [ Css.classes
                [ "border-b"
                , "border-stormy-100"
                , "hover:bg-snow-500"
                , "cursor-pointer"
                ]
            , HE.onClick \_ -> GotoSection { orderSectionId }
            ]
            [ HH.td [ HP.colSpan 3, Css.class_ "p-2" ]
                [ HH.span
                    [ Css.class_ "text-tropical-500" ]
                    [ HH.text "Category" ]
                , HH.br_
                , HH.text $ fromMaybe (show sol.id) sol.title
                ]
            -- , HH.td [ Css.class_ "text-center" ] []
            , tdDelete $ RemoveSection { orderSectionId }
            ]
        ]
          <> A.mapWithIndex (orderRow orderSectionId) orderLines

    orderId = case sof.orderForm.original of
      Nothing -> Nothing
      Just (SS.OrderForm original) -> original.id

    orderRow orderSectionId orderLineIndex = case _ of
      { product: Nothing } -> HH.text ""
      ol@{ product: Just (SS.Product prod) } ->
        HH.tr
          [ Css.classes [ "border-b", "border-stormy-100", "hover:bg-snow-500", "cursor-pointer" ]
          , HE.onClick \_ -> GotoOrderLine { orderSectionId, orderLineId: ol.orderLineId }
          ]
          [ HH.td [ Css.classes [ "p-2", "pl-12" ] ]
              [ HH.span
                  [ Css.class_ "text-tropical-500" ]
                  [ HH.text "Product ", HH.text $ show $ orderLineIndex + 1 ]
              , HH.br_
              , HH.text $ fromMaybe (show prod.sku) prod.title
              ]
          , HH.td [ Css.classes [ "p-2", "px-5" ] ]
              [ renderOrderLineStatus ol.status ol.statusReason
              ]
          -- , HH.td
          --     [ Css.classes [ "p-2", "px-5" ] ]
          --     [ HH.text $ show $ orderLineQuantity ol ]
          , tdAsset { orderSectionId, orderLineId: ol.orderLineId } ol.statusReason orderId
          , tdDelete $ RemoveOrderLine { orderSectionId, orderLineId: ol.orderLineId }
          ]

  renderSections ::
    StateOrderForm ->
    Array OrderSection ->
    H.ComponentHTML Action Slots m
  renderSections sof secs =
    -- Need a keyed div since otherwise the references seem to be ignored.
    -- Probably related to
    -- https://github.com/purescript-halogen/purescript-halogen/issues/423.
    HK.div [ Css.classes [ "flex", "flex-col", "gap-5" ] ]
      $ renderSection sof
      <$> secs

  renderOrderSectionSubTotal :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSectionSubTotal = Widgets.subTotalTable ""

  renderOrderSubTotal :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSubTotal subTotal
    | mempty == subTotal = HH.text ""
    | otherwise =
      HH.div
        [ Css.classes
            [ "p-3"
            , "space-x-4"
            , "rounded-md"
            , "bg-snow-100"
            ]
        ]
        [ Widgets.subTotalTable "Total " subTotal ]

  renderHeaderAndInfo :: OrderForm -> H.ComponentHTML Action Slots m
  renderHeaderAndInfo orderForm =
    HH.div
      [ Css.classes
          [ "flex"
          , "flex-col"
          , "gap-y-10"
          , "p-8"
          , "rounded-md"
          , "bg-snow-100"
          ]
      ]
      [ renderOrderTitle orderForm
      , renderOrderInfo orderForm
      , renderOrderHeader orderForm
      ]

  renderOrderTitle :: OrderForm -> H.ComponentHTML Action Slots m
  renderOrderTitle orderForm =
    HH.div
      [ Css.classes [ "flex", "flex-wrap", "items-center", "gap-x-8", "gap-y-4" ] ]
      [ HH.div
          [ Css.class_ "grow" ]
          [ renderOrderDisplayName orderForm.displayName ]
      , withOriginal \{ createTime } ->
          HH.div
            [ Css.classes [ "flex", "gap-4" ] ]
            [ HH.div [ Css.class_ "font-semibold" ] [ HH.text "Created" ]
            , maybe'
                (\_ -> HH.text "Not Available")
                Widgets.dateWithTimeTooltip
                createTime
            ]
      , HH.div
          [ Css.classes [ "flex", "gap-4" ] ]
          [ HH.div [ Css.class_ "font-semibold" ] [ HH.text "Status" ]
          , renderOrderStatus orderForm.status
          ]
      ]
    where
    withOriginal renderEntry = case orderForm.original of
      Nothing -> HH.text ""
      Just (SS.OrderForm o) -> renderEntry o

  renderOrderInfo :: OrderForm -> H.ComponentHTML Action Slots m
  renderOrderInfo orderForm =
    withOriginal
          ( \o ->
    HH.div
      [ Css.classes
          [ "flex"
          , "flex-col"
          , "gap-y-4"
          , "pl-8"
          ]
      ]
      [ withOriginal
          ( \o ->
              entry
                [ title "Order ID"
                , value [ HH.text $ maybe "Not Available" show o.id ]
                ]
          )
      {- , case orderForm.crmQuoteId of
          Nothing -> HH.text ""
          Just (SS.CrmQuoteId id) ->
            entry
              [ title "Quote ID"
              , value [ HH.text id ]
              ] -}
      , withOriginal
          ( \o ->
              entry
                [ title "Created by"
                , value [ HH.text $ fromMaybe "Not Available" o.createdBy ]
                ]
          )
      , withOriginal
          ( \o ->
              entry
                [ title "Approval"
                , value [ HH.text $ SS.prettyOrderApprovalStatus o.approvalStatus ]
                ]
          )
              -- , entry
                  -- [ title "Observers"
                  -- , renderOrderObservers orderId orderForm.observers
                  -- ]
          ]
            )
    where
    entry = HH.div [ Css.classes [ "flex" ] ]

    title t = HH.h4 [ Css.classes [ "w-40" ] ] [ HH.text t ]

    value = HH.div_

    orderId = case orderForm.original of
      Nothing -> Nothing
      Just (SS.OrderForm original) -> original.id

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
      , classes: [ Css.c "w-fit", Css.c "max-w-4x1", Css.c "text-2xl" ]
      , editButtonProps: [ HPAria.label "Edit order name" ]
      , inputProps: [ HP.attr (H.AttrName "maxlength") "150" ]
      }
      SetOrderDisplayName

  renderOrderStatus :: SS.OrderStatus -> H.ComponentHTML Action Slots m
  renderOrderStatus orderStatus =
    HH.div
      [ HP.classes [ Css.c "nectary-tag", Css.statusColorClass orderStatus ] ]
      [ HH.text (SS.prettyOrderStatus orderStatus) ]

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
  
  isMarioFF = if isMarioOrder  && isStateFF then 
      true 
    else 
      false
    where 
    isMarioSection sec = case sec.solution of
      Nothing -> false
      Just (SS.Solution { id }) ->
        (id == "Mario - Order Products")
          || (id == "Mario - Everything Else")
          || (id == "Mario - Edit Existing Products")
          || (id == "hello-world-example")

    isMarioOrder = case state of  -- A.any  isMarioSection Initialized (Loaded { orderForm: { sections } }
      Initialized (Loaded { orderForm: { sections } }) -> A.any isMarioSection sections
      _ -> false

  isStateFF = case state of 
    Initialized (Loaded { orderForm: { status: SS.OsInFulfillment } }) -> true
    Initialized (Loaded { orderForm: { status: SS.OsFulfilled } }) -> true
    Initialized (Loaded { orderForm: { status: SS.OsFailed } }) -> true
    _ -> false

  renderJiraUrl = if isMarioFF then
      getJiraIntUrl state
    else
      HH.text ""

  getJiraIntUrl state =
    case state of
      Initialized
        ( Loaded --{ assets } 
          { orderForm:
            { original: Just (SS.OrderForm { crmQuoteId: Just (SS.CrmQuoteId crmQuoteId)})}
          }
        ) -> HH.a [
              HP.href  crmQuoteId
              ,HP.target "_blank"
            ]
            [ HH.button [ Css.classes [ "nectary-btn-secondary", "h-7" ]][ HH.text "Open Jira Ticket"]]
      _ -> HH.text ""

  renderOrderHeader :: OrderForm -> H.ComponentHTML Action Slots m
  renderOrderHeader orderForm =
    HH.div
      [ Css.classes
          [ "flex"
          , "flex-col"
          , "w-full"
          , "gap-y-4"
          , "p-8"
          , "rounded-md"
          , "border"
          , "border-snow-600"
          , "shadow-md"
          ]
      ]
      [ HH.h3 [ Css.class_ "mb-6" ] [ HH.text "Order details" ]
      , HH.div [ Css.class_ "flex" ]
          [ title "Customer"
          , renderBuyer
          ]
      , HH.div [ Css.class_ "flex" ]
          [ title "Commercial"
          , renderCommercial
          ]
      , HH.div [ Css.class_ "flex" ]
          [ title "Legal entity"
          -- , renderSeller
          , sellerName
          ]
      ]
    where
    title t = HH.h4 [ Css.classes [ "w-40" ] ] [ HH.text t ]

    sellerName = case orderForm.seller of
      Nothing -> HH.text ""
      Just (SS.Seller { registeredName }) -> HH.text registeredName

    renderBuyer = HH.slot Buyer.proxy unit Buyer.component input SetBuyer
      where
      mkInput b =
        { buyer: b
        , buyerAvailableContacts: orderForm.buyerAvailableContacts
        , readOnly: not isInDraft
        , fixedBuyer: orderForm.fixedBuyer
        }

      input = mkInput <$> orderForm.buyer

    renderCommercial =
      let
        inputCommercial = do
          commercial <- orderForm.commercial
          SS.Buyer buyer <- orderForm.buyer
          crmAccountId <- buyer.crmAccountId
          pure
            $ Commercial.InputCommercial
                { commercial
                , crmAccountId
                , readOnly: not isInDraft
                }

        inputBillingAccountId = do
          billingAccountId <- orderForm.billingAccountId
          SS.Buyer buyer <- orderForm.buyer
          crmAccountId <- buyer.crmAccountId
          pure
            $ Commercial.InputIds
                { billingAccountId
                , crmAccountId
                , readOnly: not isInDraft || orderForm.fixedBuyer
                }

        input =
          fromMaybe Commercial.InputNothing
            (inputCommercial <|> inputBillingAccountId)
      in
        HH.slot Commercial.proxy unit Commercial.component input SetCommercial

    -- Temporary commenting render seller
    -- renderSeller = HH.slot Seller.proxy unit Seller.component input SetSeller
    --   where
    --   input = case orderForm.seller of
    --     Just seller ->
    --       Seller.InputSeller
    --         { seller
    --         , readOnly: not isInDraft
    --         }
    --     Nothing -> case orderForm.legalEntityRegisteredName of
    --       Just registeredName ->
    --         Seller.InputRegisteredName
    --           { registeredName
    --           , readOnly: not isInDraft || orderForm.fixedBuyer
    --           }
    --       Nothing -> Seller.InputNothing

  renderOrderFooter sof =
    HH.div
      [ Css.classes
          [ "flex"
          , "flex-wrap-reverse"
          , "items-center"
          , "space-x-4"
          ]
      ]
      [ renderOrderSubTotal sof.orderForm.orderTotal
      , HH.div [ Css.class_ "grow" ] []
      , if sof.orderForm.status == SS.OsInDraft then
          HH.button
            [ Css.class_ "nectary-btn-destructive"
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
          HH.text ""
      , HH.button
          [ Css.class_ "nectary-btn-primary"
          -- Temporary disabled
          , HP.disabled $ isLeft preventFulfill
          -- , HP.disabled true
          , HP.title (either identity (const "") preventFulfill)
          , HE.onClick $ \_ -> FulfillOrderStart
          ] 
          [ HH.text $ if sof.orderForm.changed then "Save & fulfill order" else "Fulfill order"
          , case sof.orderFulfillStatus of
             FulfillStatusInFlight -> buttonSpinner unit
             _ -> HH.text ""
          ]
{-           [ Tooltip.render
                      ( Tooltip.defaultInput
                          { text = "Order Fulfillment is currently disabled, please contact Sarah Cook for further information"
                          , orientation = Tooltip.Top
                          , width = Just "20rem"
                          }
                      )
                      (HH.text "Fulfill order")] -}
      , HH.button
          [ Css.class_ "nectary-btn-primary"
          , HP.disabled $ isLeft preventCreate
          , HP.title (either identity (const "") preventCreate)
          , HE.onClick $ \_ -> CreateUpdateOrder
          ]
          [ HH.text "Save order"
          , if sof.orderUpdateInFlight then buttonSpinner unit else HH.text ""
          ]
      ]
    where
    buttonSpinner _ =
      Spinner.render
        $ Spinner.defaults { classes = Css.cs [ "ml-2", "align-text-bottom" ] }

    -- | Validates that the order in the current state is valid. If an error is
    -- | found then this is a left value containing the error message.
    checkOrder :: Either String Unit
    checkOrder = do
      _ <- note "Legal entity not set" sof.orderForm.seller
      _ <- note "Customer not set" sof.orderForm.buyer
      _ <- note "Commercial not set" sof.orderForm.commercial
      -- Need valid order sections.
      _ <- traverse checkOrderSection sof.orderForm.sections
      pure unit
      where
      checkOrderSection os = do
        SS.Solution solution <- note "Missing category in section" os.solution
        _ <- note "Missing category URI in section" solution.uri
        _ <-
          if A.null solution.priceBooks then
            Right unit
          else
            note "Missing price book in section" (void os.priceBook)
        -- Need valid order lines.
        _ <- traverse checkOrderLine os.orderLines
        pure unit

      checkOrderLine ol = do
        product <- note "Missing product in order line" ol.product
        _ <- traverse (checkOrderLineConfig product) ol.configs
        pure unit

      -- The order config schema and the actual configuration need to match.
      checkOrderLineConfig product orderLineConfig =
        let
          SS.Product { orderConfigSchema } = product

          SS.OrderLineConfig { config } = orderLineConfig
        in
          case Tuple orderConfigSchema config of
            Tuple Nothing Nothing -> Right unit
            Tuple (Just ocs) (Just conf) -> Schema.checkValue ocs conf
            -- If the product has no configuration schema then we allow an empty object.
            Tuple Nothing (Just (SS.CvObject obj))
              | Map.isEmpty obj -> Right unit
            _ -> Left "Invalid configuration in order line"

    -- Prevent order creation/update if left value, otherwise allow.
    preventCreate :: Either String Unit
    preventCreate
      | sof.orderUpdateInFlight = Left "Order updating…"
      | not sof.orderForm.changed = Left "Order unchanged"
      | otherwise = checkOrder

    -- Prevent order fulfill if left value, otherwise allow.
    preventFulfill :: Either String Unit
    preventFulfill
      | sof.orderUpdateInFlight = Left "Saving order…"
      | otherwise = case sof.orderFulfillStatus of
        FulfillStatusIdle -> case getOriginalOrderStatus sof of
          Just status
            | SS.isFinalOrderStatus status -> Left "Order in a final status"
            | status == SS.OsInFulfillment -> Left "Order is already in fulfillment"
          _ -> checkOrder
        _ -> Left "Fulfilling order…"

  renderFulfillmentConfirmModal sof = case sof.orderFulfillStatus of
    FulfillStatusConfirming ->
      HH.slot
        (Proxy :: Proxy "confirmFulfillModal")
        unit
        ConfirmFulfillModal.component
        { isMarioOrder }
        FulfillOrderModalResult
    _ -> HH.text ""
    where
    isMarioSection sec = case sec.solution of
      Nothing -> false
      Just (SS.Solution { id }) ->
        (id == "Mario - Order Products")
          || (id == "Mario - Everything Else")
          || (id == "Mario - Edit Existing Products")

    isMarioOrder = A.any isMarioSection sof.orderForm.sections

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action Slots m)
  renderOrderForm sof =
    [ HH.div [ Css.classes [ "flex", "flex-col", "space-y-5" ] ]
        [ renderHeaderAndInfo sof.orderForm
        , renderSectionSummary sof
        , renderSections sof sof.orderForm.sections
        , renderOrderFooter sof
        , HH.hr_
        , HH.details_
            [ HH.summary
                [ Css.class_ "cursor-pointer" ]
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
    , renderFulfillmentConfirmModal sof
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
        [ HH.h3 [ Css.classes [ "text-lg" ] ] [ HH.text "Error" ]
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

  renderContent :: Array (H.ComponentHTML Action Slots m)
  renderContent =
      [ HH.div
        [ Css.classes
            [ "my-5"
            , "flex"
            , "flex-wrap"
            , "items-center"
            , "gap-4"
            ]
        ]
        [ HH.h1 [ Css.classes [ "grow", "my-0" ] ] [ HH.text "Order form" ]
          ,renderJiraUrl
        ]
    ]
    -- [ HH.h1_ [ HH.text "Order form" ] ]
      <> case state of
          Initializing _ -> []
          Initialized state' -> defRender state' renderOrderForm

-- | Fetches all configurations within the given order section.
orderSchemaGetConfigs ∷
  State ->
  OrderSectionId ->
  Unit ->
  Array SkuConfigs
orderSchemaGetConfigs state orderSectionId _ = case state of
  Initialized (Loaded { orderForm: { sections } }) -> do
    section <- maybe [] A.singleton $ findSection orderSectionId sections
    Tuple oIdx orderLine <- A.mapWithIndex Tuple section.orderLines
    fromMaybe [] do
      SS.Product { sku, title } <- orderLine.product
      let
        label =
          "Product "
            <> show (oIdx + 1)
            <> " – "
            <> fromMaybe (show sku) title
      pure
        [ { sku
          , configs:
              map (\config -> { label, config })
                $ NA.toArray orderLine.configs
          }
        ]
  _ -> []

toJson :: OrderForm -> Either String SS.OrderForm
toJson orderForm = do
  commercial <- note "Missing commercial" $ orderForm.commercial
  buyer <- note "Missing buyer" $ orderForm.buyer
  seller <- note "Missing seller" $ orderForm.seller
  sections <- traverse toOrderSection orderForm.sections
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
        , createdBy: Nothing
        }
  where
  toOrderLine :: OrderLine -> Either String SS.OrderLine
  toOrderLine ol = do
    sku <-
      note "Missing product SKU"
        $ do
            SS.Product { sku } <- ol.product
            pure sku
    pure
      $ SS.OrderLine
          { orderLineId: toExternalId ol.orderLineId
          , status: ol.status
          , statusReason: ol.statusReason
          , sku
          , charges: fromMaybe [] ol.charges
          , configs: NA.toArray ol.configs
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
    solutionUri <-
      note "Missing category URI"
        $ do
            SS.Solution { uri } <- os.solution
            uri
    basePriceBook <- note "Missing price book" $ toPriceBookRef solutionUri <$> os.priceBook
    orderLines <- traverse toOrderLine os.orderLines
    pure
      $ SS.OrderSection
          { orderSectionId: toExternalId os.orderSectionId
          , basePriceBook
          , orderLines
          }

toJsonStr :: OrderForm -> Either String String
toJsonStr = map (stringifyWithIndent 2 <<< encodeJson) <<< toJson

orderFormSessionStorageKey :: String
orderFormSessionStorageKey = "sofa-order-form"

setSessionOrderForm :: SS.OrderForm -> Effect Unit
setSessionOrderForm orderForm =
  Html.window
    >>= HtmlWindow.sessionStorage
    >>= HtmlStorage.setItem orderFormSessionStorageKey json
  where
  json = stringify $ encodeJson orderForm

getSessionOrderForm :: Effect (Either String SS.OrderForm)
getSessionOrderForm = do
  mJsonStr <-
    HtmlStorage.getItem orderFormSessionStorageKey
      =<< HtmlWindow.sessionStorage
      =<< Html.window
  let
    mapLeft f = either (Left <<< f) Right
  pure do
    jsonStr <- note "No stored order" mJsonStr
    json <- jsonParser jsonStr
    mapLeft printJsonDecodeError $ decodeJson json

clearSessionOrderForm :: Effect Unit
clearSessionOrderForm =
  HtmlStorage.removeItem orderFormSessionStorageKey
    =<< HtmlWindow.sessionStorage
    =<< Html.window

loadCatalog ::
  forall slots output m.
  MonadAff m =>
  Maybe SS.CrmQuoteId ->
  Maybe
    { buyer :: SS.Buyer
    , billingAccountId :: SS.BillingAccountId
    , contacts :: Array SS.Contact
    , legalEntityRegisteredName :: String
    } ->
  H.HalogenM State Action slots output m Unit
loadCatalog crmQuoteId customerData = do
  H.put $ Initialized Loading
  productCatalog <- H.liftAff Requests.getProductCatalog
  orderSectionId <- genOrderSectionId
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
              , billingAccountId: (_.billingAccountId) <$> customerData
              , commercial: Nothing
              , buyer: (_.buyer) <$> customerData
              , fixedBuyer: isJust customerData
              , buyerAvailableContacts: (_.contacts) <$> customerData
              , legalEntityRegisteredName: (_.legalEntityRegisteredName) <$> customerData
              , seller: Nothing
              , status: SS.OsInDraft
              , observers: []
              , notes: []
              , orderTotal: mempty
              , sections: [ emptyOrderSection orderSectionId ]
              }
          , orderUpdateInFlight: false
          , orderFulfillStatus: FulfillStatusIdle
          , assetModalOpen: Nothing
          , assets: Loading
          , crmAccountId: Nothing
          }
      )
        <$> productCatalog
  H.put $ Initialized res

mkDefaultConfigs :: SS.OrderLineConfigId -> SS.Product -> NonEmptyArray SS.OrderLineConfig
mkDefaultConfigs id (SS.Product p) =
  fromMaybe' (\_ -> NA.singleton empty) do
    schema <- p.orderConfigSchema
    default_ <- mkDefaultConfig schema
    pure
      $ NA.singleton
      $ SS.OrderLineConfig
      $ emptyRec
          { quantity = 1
          , config = Just default_
          }
  where
  empty@(SS.OrderLineConfig emptyRec) = emptyOrderLineConfig id

calcSubTotal :: OrderSection -> OrderSection
calcSubTotal os =
  os
    { orderLines = orderLines'
    , subTotal = sumOrderLines orderLines'
    }
  where
  defaultCurrency = maybe (SS.ChargeCurrency (unsafeMkCurrency "FIX")) _.currency os.priceBook

  orderLines' = updateOrderLineCharges os.priceBook <$> os.orderLines

  -- | Sets the order line charge and default quantity from the given price
  -- | book. If the order line already has a charge, then it is returned
  -- | unchanged.
  updateOrderLineCharges :: Maybe PriceBook -> OrderLine -> OrderLine
  updateOrderLineCharges mpb ol
    | isJust ol.charges = ol
    | otherwise =
      fromMaybe ol do
        product <- ol.product
        pb <- mpb
        let
          charges = lookupCharges product pb
        pure
          $ ol
              { charges = charges
              , estimatedUsage = maybe Map.empty (mkDefaultEstimatedUsage ol.unitMap) charges
              }

  lookupCharges :: SS.Product -> PriceBook -> Maybe (Array SS.Charge)
  lookupCharges (SS.Product product) pb = do
    rateCards <- pb.rateCards
    SS.RateCard rateCard <- Map.lookup product.sku rateCards
    pure rateCard.charges

  mkDefaultEstimatedUsage :: Charge.ChargeUnitMap -> Array SS.Charge -> QuantityMap
  mkDefaultEstimatedUsage unitMap charges =
    Map.fromFoldable
      $ List.concatMap mk
      $ List.fromFoldable charges
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

  sumOrderLines :: Array OrderLine -> SubTotal
  sumOrderLines = A.foldl (\a b -> a <> conv b) mempty

  conv :: OrderLine -> SubTotal
  conv ol =
    fromMaybe mempty do
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
calcTotal orderForm = orderForm { orderTotal = sumOrderSecs orderForm.sections }
  where
  sumOrderSecs = A.foldl (\acc { subTotal } -> acc <> subTotal) mempty

orderLineQuantity :: OrderLine -> Quantity
orderLineQuantity ol =
  foldl (\a (SS.OrderLineConfig b) -> a + b.quantity) 0
    $ NA.toArray ol.configs

findSection :: OrderSectionId -> Array OrderSection -> Maybe OrderSection
findSection secId = A.find (\{ orderSectionId } -> secId == orderSectionId)

deleteSection :: OrderSectionId -> Array OrderSection -> Maybe (Array OrderSection)
deleteSection secId sections = do
  idx <- A.findIndex (\{ orderSectionId } -> secId == orderSectionId) sections
  A.deleteAt idx sections

findOrderLine :: OrderLineId -> Array OrderLine -> Maybe OrderLine
findOrderLine olId = A.find (\{ orderLineId } -> olId == orderLineId)

deleteOrderLine :: OrderLineId -> Array OrderLine -> Maybe (Array OrderLine)
deleteOrderLine olId orderLines = do
  idx <- A.findIndex (\{ orderLineId } -> olId == orderLineId) orderLines
  A.deleteAt idx orderLines

deleteOrderLineConfig ::
  SS.OrderLineConfigId ->
  NonEmptyArray SS.OrderLineConfig ->
  Maybe (Array SS.OrderLineConfig)
deleteOrderLineConfig configId configs = do
  idx <- NA.findIndex (\(SS.OrderLineConfig { id }) -> Just configId == id) configs
  NA.deleteAt idx configs

-- | Helper function to modify an identified order section. The sub-total of the
-- | modified section is updated.
modifyOrderSection ::
  OrderSectionId ->
  (OrderSection -> OrderSection) ->
  OrderForm ->
  OrderForm
modifyOrderSection secId updateOrderSection order =
  order
    { sections =
      fromMaybe order.sections do
        idx <- A.findIndex (\{ orderSectionId } -> secId == orderSectionId) order.sections
        modifyAt idx (calcSubTotal <<< updateOrderSection) order.sections
    }

-- | Helper function to modify an indexed order line.
modifyOrderLine ::
  OrderSectionId ->
  OrderLineId ->
  (OrderLine -> OrderLine) ->
  OrderForm ->
  OrderForm
modifyOrderLine secId olId updateOrderLine =
  modifyOrderSection secId \section ->
    section
      { orderLines =
        fromMaybe section.orderLines do
          idx <- A.findIndex (\{ orderLineId } -> olId == orderLineId) section.orderLines
          modifyAt idx updateOrderLine section.orderLines
      }

modifyOrderLineConfig ::
  SS.OrderLineConfigId ->
  (SS.OrderLineConfig -> SS.OrderLineConfig) ->
  OrderLine ->
  OrderLine
modifyOrderLineConfig configId alter orderLine =
  orderLine
    { configs =
      fromMaybe orderLine.configs do
        idx <- NA.findIndex (\(SS.OrderLineConfig c) -> Just configId == c.id) orderLine.configs
        NA.modifyAt idx alter orderLine.configs
    }

loadWithCrmAccount ::
  forall slots output f m.
  MonadAff m =>
  CredentialStore f m =>
  SS.CrmAccountId ->
  H.HalogenM State Action Slots output m Unit
loadWithCrmAccount id = do
  H.put $ Initialized Loading
  productCatalog <- H.liftAff Requests.getProductCatalog
  buyerLoadable <- H.lift $ Requests.getBuyer id
  contacts <- H.lift $ Requests.getBuyerContacts id
  orderSectionId <- genOrderSectionId
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
              , crmQuoteId: Nothing
              , billingAccountId: Nothing
              , commercial: Nothing
              , buyer: Loadable.toMaybe buyerLoadable
              , fixedBuyer: false
              , buyerAvailableContacts: Loadable.toMaybe contacts
              , legalEntityRegisteredName: Nothing
              , seller: Nothing
              , status: SS.OsInDraft
              , observers: []
              , notes: []
              , orderTotal: mempty
              , sections: [ emptyOrderSection orderSectionId ]
              }
          , orderUpdateInFlight: false
          , orderFulfillStatus: FulfillStatusIdle
          , assetModalOpen: Nothing
          , assets: Loading
          , crmAccountId: crmAccountId
          }
      )
        <$> productCatalog
    buyerFull = Loadable.toMaybe buyerLoadable
    crmAccountId = case buyerFull of
      Nothing -> Nothing
      Just (SS.Buyer buyerFull) -> buyerFull.crmAccountId
    
    
  modifyInitialized
      $ \st ->
          st
            { orderForm =
              st.orderForm
                { changed = true
                , buyer = buyerFull
                }
            }
  H.put $ Initialized res
  H.tell Commercial.proxy unit
        (Commercial.ResetCommercialFixed { commercial: Nothing, crmAccountId, enabled: true, open: true })


loadExisting ::
  forall slots output m.
  MonadAff m =>
  MonadDeployment m =>
  SS.OrderForm ->
  Boolean ->
  H.HalogenM State Action slots output m Unit
loadExisting original@(SS.OrderForm orderForm) changed = do
  H.put $ Initialized Loading
  productCatalog <- H.liftAff Requests.getProductCatalog
  -- id <- case orderForm.id of 
  --  Just oid -> oid
  --  _ -> SS.OrderId ("")
  -- assets <- H.liftAff ( Requests.getAsset id )
  fixedBuyer <- H.lift isBuyerFixed
  H.put $ Initialized $ convertOrderForm fixedBuyer =<< productCatalog
  where
  convertOrderForm :: Boolean -> SS.ProductCatalog  -> Loadable StateOrderForm
  convertOrderForm fixedBuyer productCatalog  = do
    let
      -- The value to use as v5 UUID root namespace for generated internal IDs.
      uuidRootNs = UUID.emptyUUID

      currency = toPricingCurrency orderForm.commercial

      priceBooks = mkPriceBooks productCatalog currency
    sections <-
      traverseWithIndex
        (convertOrderSection productCatalog priceBooks uuidRootNs)
        orderForm.sections
    pure
      $ { productCatalog
        , currency
        , priceBooks
        , orderForm:
            calcTotal
              { original: Just original
              , changed
              , displayName: orderForm.displayName
              , crmQuoteId: orderForm.crmQuoteId
              , billingAccountId:
                  let
                    SS.Commercial { billingAccountId } = orderForm.commercial
                  in
                    billingAccountId
              , commercial: Just orderForm.commercial
              , buyer: Just orderForm.buyer
              , fixedBuyer
              , buyerAvailableContacts: Nothing
              , legalEntityRegisteredName:
                  let
                    SS.Seller { registeredName } = orderForm.seller
                  in
                    Just registeredName
              , seller: Just orderForm.seller
              , status: orderForm.status
              , observers: orderForm.orderObservers
              , notes: orderForm.orderNotes
              , orderTotal: mempty
              , sections
              }
        , orderUpdateInFlight: false
        , orderFulfillStatus: FulfillStatusIdle
        , assetModalOpen: Nothing
        , assets: Loading
        , crmAccountId: Nothing
        }

  convertOrderSection ::
    SS.ProductCatalog ->
    Map String (Array PriceBook) ->
    UUID ->
    Int ->
    SS.OrderSection ->
    Loadable OrderSection
  convertOrderSection (SS.ProductCatalog { solutions }) pbs uuidNs idx (SS.OrderSection s) = do
    let
      SS.PriceBookRef pbRef = s.basePriceBook

      uuidSectionId = genv5UUID (show idx) uuidNs
    solution@(SS.Solution { id: solId, priceBooks: solPbs }) <-
      maybe' (\_ -> Error "No matching category found") Loaded
        $ List.find (\(SS.Solution { uri }) -> pbRef.solutionUri == uri)
        $ Map.values solutions
    priceBook <-
      maybe' (\_ -> Error "No matching price book found") Loaded
        $ if A.null solPbs then
            mkNilPriceBook solution
          else do
            priceBooks <- Map.lookup solId pbs
            A.find (\pb -> pb.id == pbRef.priceBookId) priceBooks
    pure
      $ calcSubTotal
          { orderSectionId:
              maybe'
                (\_ -> InternalId $ SS.OrderSectionId $ UUID.toString uuidSectionId)
                ExternalId
                s.orderSectionId
          , solution: Just solution
          , priceBook: Just priceBook
          , orderLines: A.mapWithIndex (convertOrderLine solution uuidSectionId) s.orderLines
          , subTotal: mempty
          }

  convertOrderLine (SS.Solution solution) uuidNs idx (SS.OrderLine l) =
    { orderLineId:
        maybe'
          (\_ -> genInternalId' SS.OrderLineId (show idx) uuidNs)
          ExternalId
          l.orderLineId
    , status: l.status
    , statusReason: l.statusReason
    , product
    , charges: Just l.charges
    , unitMap: maybe Map.empty Charge.productChargeUnitMap product
    , configs:
        fromMaybe' (\_ -> NA.singleton $ genEmptyOrderLineConfig uuidNs)
          $ NA.fromArray l.configs
    , estimatedUsage: fromSmartSpecQuantity l.estimatedUsage
    }
    where
    product = List.find (\(SS.Product { sku }) -> l.sku == sku) $ solution.products

modifyInitialized ::
  forall slots output m.
  MonadAff m =>
  (StateOrderForm -> StateOrderForm) ->
  H.HalogenM State Action slots output m Unit
modifyInitialized f = do
  state <-
    H.modify
      $ case _ of
          Initialized st -> Initialized (f <$> st)
          initializing -> initializing
  -- Try to persist the modified order form in the browser session storage.
  H.liftEffect case state of
    Initialized (Loaded { orderForm }) ->
      either (\_ -> pure unit) setSessionOrderForm
        $ toJson orderForm
    _ -> pure unit

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
findSectionFileIds section = A.concatMap findLineFileIds section.orderLines

-- | Finds all file IDs within the given order line.
findLineFileIds ::
  OrderLine ->
  Array String
findLineFileIds orderLine = do
  SS.OrderLineConfig { config } <- NA.toArray orderLine.configs
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
  forall slots output f m.
  MonadAff m =>
  CredentialStore f m =>
  Array String ->
  H.HalogenM State Action slots output m Unit
deleteFileAttachments fileIds =
  sequential
    $ for_ fileIds (parallel <<< H.lift <<< Requests.deleteFile)

toPricingCurrency :: SS.Commercial -> Maybe SS.PricingCurrency
toPricingCurrency (SS.Commercial { billingCurrency }) = Just billingCurrency

getOrderId :: StateOrderForm -> Maybe SS.OrderId
getOrderId = (\(SS.OrderForm o) -> o.id) <=< _.orderForm.original

getOriginalOrderStatus :: StateOrderForm -> Maybe SS.OrderStatus
getOriginalOrderStatus = map (\(SS.OrderForm o) -> o.status) <<< _.orderForm.original

emptyOrderSection :: OrderSectionId -> OrderSection
emptyOrderSection orderSectionId =
  { orderSectionId
  , solution: Nothing
  , priceBook: Nothing
  , orderLines: mempty
  , subTotal: mempty
  }

emptyOrderLineConfig :: SS.OrderLineConfigId -> SS.OrderLineConfig
emptyOrderLineConfig id =
  SS.OrderLineConfig
    { id: Just id
    , quantity: 1
    , config: Nothing
    }

genEmptyOrderLineConfig :: UUID -> SS.OrderLineConfig
genEmptyOrderLineConfig uuidNs =
  emptyOrderLineConfig
    $ SS.OrderLineConfigId
    $ UUID.toString
    $ genv5UUID "config" uuidNs

emptyOrderLine :: OrderLineId -> OrderLine
emptyOrderLine orderLineId =
  { orderLineId
  , status: SS.OlsNew
  , statusReason: ""
  , product: Nothing
  , charges: Nothing
  , unitMap: Map.empty
  , configs: NA.singleton $ genEmptyOrderLineConfig emptyUUID
  , estimatedUsage: Map.empty
  }

genOrderLineId :: forall m. MonadEffect m => m OrderLineId
genOrderLineId = H.liftEffect $ genInternalId SS.OrderLineId

genOrderSectionId :: forall m. MonadEffect m => m OrderSectionId
genOrderSectionId = H.liftEffect $ genInternalId SS.OrderSectionId

genOrderLineConfigId :: forall m. MonadEffect m => m SS.OrderLineConfigId
genOrderLineConfigId =
  H.liftEffect
    $ (SS.OrderLineConfigId <<< UUID.toString)
    <$> UUID.genUUID

mkOrderLine :: OrderLineId -> SS.OrderLineConfigId -> SS.Product -> OrderLine
mkOrderLine orderLineId configId product =
  (emptyOrderLine orderLineId)
    { product = Just product
    , unitMap = Charge.productChargeUnitMap product
    , configs = mkDefaultConfigs configId product
    }

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

sectionRefLabel :: OrderSectionId -> H.RefLabel
sectionRefLabel sectionId =
  H.RefLabel
    $ "ordersection/"
    <> show (toRawId sectionId)

orderLineRefLabel :: OrderSectionId -> OrderLineId -> H.RefLabel
orderLineRefLabel orderSectionId orderLineId =
  H.RefLabel
    $ "orderline/"
    <> show (toRawId orderSectionId)
    <> "/"
    <> show (toRawId orderLineId)

-- | Saves the order in the given state to the backend. Returns the saved order
-- | form object, or nothing if the save failed.
createUpdateOrder ::
  forall output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  State -> H.HalogenM State Action Slots output m (Maybe SS.OrderForm)
createUpdateOrder state = do
  let
    -- Updates the current state to match the response order object.
    loadOrder = case _ of
      Loaded order' -> do
        loadExisting order' false
        pure $ Just order'
      _ -> do
        modifyInitialized $ _ { orderUpdateInFlight = false }
        pure Nothing

    alert = case _ of
      Error msg ->
        Alerts.push
          $ Alert.errorAlert "Failed to save order." msg
      _ ->
        Alerts.push
          $ Alert.defaultAlert
              { type_ = Alert.Success
              , content = HH.text "Order successfully saved."
              }

    run json =
      maybe'
        (\_ -> Requests.postOrder json)
        (\id -> Requests.patchOrder id json)
  case state of
    Initialized (Loaded st') -> case toJson st'.orderForm of
      Left msg -> do
        H.liftEffect $ Console.error $ "Could not produce order JSON: " <> msg
        pure Nothing
      Right json -> do
        modifyInitialized $ _ { orderUpdateInFlight = true }
        order <- H.lift $ run json (getOrderId st')
        H.lift $ alert order
        loadOrder order
    _ -> pure Nothing

handleAction ::
  forall output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    H.liftEffect $ addClassToElement "new-form" "sofa-navbar-selected"
    H.liftEffect $ removeClassToElement "order-lists" "sofa-navbar-selected"
    st <- H.get
    let
      loadOrder = case _ of
        Error err -> H.put $ Initialized (Error err)
        Idle -> H.put $ Initialized Idle
        Loaded order -> loadExisting order false
        Loading -> H.put $ Initialized Loading
    case st of
      Initializing (NewOrderCrmAccountId crmAccountId) -> do
        -- pure unit
        loadWithCrmAccount crmAccountId
      Initializing NewOrder -> do
        -- Check if there is an order in session storage. If there is one then
        -- load that order, otherwise simply load the product catalog and start
        -- with a completely empty order form.
        --
        -- This lets us to do things like a login flow or a hard reload of the
        -- current tab without losing order form content.
        --
        -- Note, we only load an order from session storage if it has not been
        -- saved to the backend, i.e., when it is lacking an order ID.
        eStoredOrder <- H.liftEffect getSessionOrderForm
        let
          noOrder err = do
            H.liftEffect $ Console.log err
            loadCatalog Nothing Nothing

          gotOrder order = do
            H.liftEffect $ Console.log "Got stored order"
            case order of
              SS.OrderForm { id: Nothing } -> loadExisting order true
              _ -> loadCatalog Nothing Nothing
        either noOrder gotOrder eStoredOrder
      Initializing (ExistingOrder orderForm) -> loadExisting orderForm false
      Initializing (ExistingOrderId id) -> do
        H.put $ Initialized Loading
        orderForm <- H.lift $ Requests.getOrder id
        loadOrder orderForm
      Initializing (ExistingCrmQuoteId crmQuoteId) -> do
        H.put $ Initialized Loading
        orderForm <- H.lift $ Requests.getOrderForQuote crmQuoteId
        loadOrder orderForm
      Initializing (SalesforceNewOrder sfData) -> do
        loadCatalog Nothing (Just sfData)
      Initialized _ -> pure unit
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
  SetSeller seller ->
    modifyInitialized
      $ \st ->
          st
            { orderForm =
              st.orderForm
                { changed = true
                , seller = Just seller
                }
            }
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
    -- Reset the commercial field if we are using a non-fixed billing account.
    state <- H.get
    case state of
      Initialized (Loaded { orderForm: { fixedBuyer } })
        | fixedBuyer -> pure unit
      _ ->
        H.tell Commercial.proxy unit
          (Commercial.ResetCommercial { commercial: Nothing, crmAccountId, enabled: true })
  SetCommercial (SS.BillingAccount { displayName, shortId, commercial, legalEntity }) -> do
    result <- H.lift $ Requests.getLegalEntityByShortName legalEntity
    let
      legalEntityResult = Loadable.toMaybe result
      sellerFull = toSeller <$> legalEntityResult
    modifyInitialized
      $ \st ->
          let
            currency = toPricingCurrency commercial

            priceBooks = mkPriceBooks st.productCatalog currency

            resetSection sec =
              sec
                { priceBook =
                  do
                    solution@(SS.Solution sol) <- sec.solution
                    if A.null sol.priceBooks then
                      mkNilPriceBook solution
                    else
                      Nothing
                , subTotal = mempty :: SubTotal
                }
          in
            st
              { currency = currency
              , priceBooks = priceBooks
              , orderForm =
                st.orderForm
                  { changed = true
                  , displayName =
                    case st.orderForm.displayName of
                      Nothing -> Just $ displayName <> " / " <> shortId
                      _ -> st.orderForm.displayName
                  , commercial = Just commercial
                  --  If the currency changed then we can't use the same price
                  --  book, so the summary and all sections need to be updated.
                  , orderTotal =
                    if st.currency == currency then
                      st.orderForm.orderTotal
                    else
                      mempty
                  , sections =
                    if st.currency == currency then
                      st.orderForm.sections
                    else
                      map resetSection st.orderForm.sections
                  , seller = sellerFull
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
  GotoSection { orderSectionId } ->
    scrollToElement
      $ sectionRefLabel orderSectionId
  GotoOrderLine { orderSectionId, orderLineId } ->
    scrollToElement
      $ orderLineRefLabel orderSectionId orderLineId
  AddSection -> do
    orderSectionId <- genOrderSectionId
    modifyInitialized
      $ modifyOrderForm \order ->
          order
            { sections = snoc order.sections (emptyOrderSection orderSectionId)
            }
    scrollToElement
      $ sectionRefLabel orderSectionId
  SectionSetSolution { orderSectionId, solutionId } -> do
    orderLineId <- genOrderLineId
    modifyInitialized \state ->
      let
        SS.ProductCatalog pc = state.productCatalog

        updateSection :: OrderSection -> OrderSection
        updateSection section =
          fromMaybe section do
            solution <- Map.lookup solutionId pc.solutions
            pure
              $ section
                  { solution = Just solution
                  , priceBook = mkNilPriceBook solution
                  , orderLines = [ emptyOrderLine orderLineId ]
                  , subTotal = (mempty :: SubTotal)
                  }
      in
        modifyOrderForm
          (modifyOrderSection orderSectionId updateSection)
          state
    scrollToElement
      $ sectionRefLabel orderSectionId
  SectionSetPriceBook { orderSectionId, priceBook } ->
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderSection orderSectionId _ { priceBook = priceBook }
  RemoveSection { orderSectionId } event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    -- Verify that user really wants to delete the section.
    confirm <-
      H.liftEffect do
        window <- Html.window
        HtmlWindow.confirm "Really remove order section? This cannot be undone." window
    if not confirm then
      pure unit
    else do
      state <- H.get
      maybe' pure deleteFileAttachments
        $ case state of
            Initialized (Loaded { orderForm: { sections } }) -> do
              section <- findSection orderSectionId sections
              pure $ findSectionFileIds section
            _ -> Nothing
      deleteResult <-
        H.lift
          $ maybe' (pure <<< Loaded) (uncurry Requests.deleteOrderSection)
          $ case state of
              Initialized (Loaded { orderForm: { original: Just (SS.OrderForm { id: Just orderId }), sections } }) -> do
                section <- findSection orderSectionId sections
                sectionId <- toExternalId section.orderSectionId
                pure (Tuple orderId sectionId)
              _ -> Nothing
      case deleteResult of
        Loading -> pure unit
        Idle -> pure unit
        Loaded _ -> do
          modifyInitialized
            $ modifyOrderForm \order ->
                order
                  { sections =
                    fromMaybe order.sections
                      $ deleteSection orderSectionId order.sections
                  }
          H.lift
            $ Alerts.push
            $ Alert.defaultAlert
                { type_ = Alert.Success
                , content = HH.text "Deleted order section"
                }
        Error errMsg ->
          H.lift
            $ Alerts.push
            $ Alert.errorAlert "Error deleting order section" errMsg
  AddOrderLine { orderSectionId } -> do
    orderLineId <- genOrderLineId
    let
      orderLine = emptyOrderLine orderLineId
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderSection orderSectionId \section ->
          section { orderLines = snoc section.orderLines orderLine }
    scrollToElement
      $ orderLineRefLabel orderSectionId orderLineId
  AddOrderLineForProducts { orderSectionId, skus } -> do
    ids <-
      H.liftEffect
        $ List.replicateM (A.length skus) do
            orderLineId <- genInternalId SS.OrderLineId
            configId <- (SS.OrderLineConfigId <<< UUID.toString) <$> genUUID
            pure { orderLineId, configId }
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderSection orderSectionId \section ->
          let
            make ::
              { orderLineId :: OrderLineId, configId :: SS.OrderLineConfigId } ->
              SS.SkuCode -> Maybe OrderLine
            make { orderLineId, configId } sku = do
              solProds <- SS.solutionProducts <$> section.solution
              product <- Map.lookup sku solProds
              pure $ mkOrderLine orderLineId configId product

            newOrderLines =
              A.fromFoldable
                $ List.catMaybes
                $ List.zipWith make ids
                $ List.fromFoldable skus
          in
            section { orderLines = section.orderLines <> newOrderLines }
  RemoveOrderLine { orderSectionId, orderLineId } event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    -- Verify that user really wants to delete the order line.
    confirm <-
      H.liftEffect do
        window <- Html.window
        HtmlWindow.confirm "Really remove order line? This cannot be undone." window
    if not confirm then
      pure unit
    else do
      state <- H.get
      maybe' pure deleteFileAttachments
        $ case state of
            Initialized (Loaded { orderForm: { sections } }) -> do
              { orderLines } <- findSection orderSectionId sections
              orderLine <- findOrderLine orderLineId orderLines
              pure $ findLineFileIds orderLine
            _ -> Nothing
      deleteResult <-
        H.lift
          $ maybe' (pure <<< Loaded) (uncurry3 Requests.deleteOrderLine)
          $ case state of
              Initialized (Loaded { orderForm: { original: Just (SS.OrderForm { id: Just orderId }), sections } }) -> do
                section <- findSection orderSectionId sections
                orderLine <- findOrderLine orderLineId section.orderLines
                sectionId <- toExternalId section.orderSectionId
                lineId <- toExternalId orderLine.orderLineId
                pure (tuple3 orderId sectionId lineId)
              _ -> Nothing
      case deleteResult of
        Loading -> pure unit
        Idle -> pure unit
        Loaded _ -> do
          modifyInitialized
            $ modifyOrderForm
            $ modifyOrderSection orderSectionId \section ->
                section
                  { orderLines =
                    fromMaybe section.orderLines
                      $ deleteOrderLine orderLineId section.orderLines
                  }
          H.lift
            $ Alerts.push
            $ Alert.defaultAlert
                { type_ = Alert.Success
                , content = HH.text "Deleted order line."
                }
        Error errMsg ->
          H.lift
            $ Alerts.push
            $ Alert.errorAlert "Error deleting order line." errMsg
  OrderLineSetProduct { orderSectionId, orderLineId: oldOrderLineId, product } -> do
    let
      mkOrderLineId :: { orderLineId :: UUID, configId :: UUID } -> IEId SS.OrderLineId
      mkOrderLineId freshIds =
        InternalId
          $ SS.OrderLineId
          $ UUID.toString freshIds.orderLineId

      mkOrderLine' :: { orderLineId :: UUID, configId :: UUID } -> SS.Product -> OrderLine
      mkOrderLine' freshIds prod =
        mkOrderLine
          (mkOrderLineId freshIds)
          (SS.OrderLineConfigId $ UUID.toString freshIds.configId)
          prod

      -- | Build order lines for all required product options.
      requiredOptions ::
        { orderLineId :: UUID, configId :: UUID } ->
        Map SS.SkuCode SS.Product ->
        Array OrderLine
      requiredOptions freshIds solProds =
        let
          SS.Product p = product

          requiredSkuCode = case _ of
            SS.ProdOptSkuCode _ -> Nothing
            SS.ProductOption po -> if po.required then Just po.sku else Nothing

          requiredProds = A.mapMaybe (requiredSkuCode >=> (\o -> Map.lookup o solProds)) <$> p.options

          -- Since we don't have access to the Effect monad here we generate v5
          -- UUIDs instead. Its namespace is the ID of the originating order
          -- line.
          mkId i =
            { orderLineId: UUID.genv5UUID ("ol" <> show i) freshIds.orderLineId
            , configId: UUID.genv5UUID ("oc" <> show i) freshIds.orderLineId
            }
        in
          maybe [] (A.mapWithIndex (\i -> mkOrderLine' $ mkId i)) requiredProds

      updateOrderSection ::
        { orderLineId :: UUID, configId :: UUID } ->
        OrderSection -> OrderSection
      updateOrderSection freshIds section =
        fromMaybe section do
          solProds <- SS.solutionProducts <$> section.solution
          pure
            $ calcSubTotal
                section
                  { orderLines =
                    maybe
                      section.orderLines
                      (\ls -> ls <> requiredOptions freshIds solProds)
                      ( do
                          idx <- A.findIndex (\ol -> ol.orderLineId == oldOrderLineId) section.orderLines
                          ls <- A.updateAt idx (mkOrderLine' freshIds product) section.orderLines
                          pure ls
                      )
                  }
    freshIds <-
      H.liftEffect do
        orderLineId <- UUID.genUUID
        configId <- UUID.genUUID
        pure { orderLineId, configId }
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderSection orderSectionId (updateOrderSection freshIds)
    scrollToElement
      $ orderLineRefLabel orderSectionId (mkOrderLineId freshIds)
  OrderLineSetQuantity { orderSectionId, orderLineId, configId, quantity } -> do
    let
      updateOrderConfig :: SS.OrderLineConfig -> SS.OrderLineConfig
      updateOrderConfig (SS.OrderLineConfig olc) = SS.OrderLineConfig $ olc { quantity = quantity }
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderLine orderSectionId orderLineId
      $ modifyOrderLineConfig configId updateOrderConfig
  OrderLineAddConfig { orderSectionId, orderLineId } -> do
    configId <- genOrderLineConfigId
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderLine orderSectionId orderLineId \ol ->
          ol
            { configs =
              fromMaybe ol.configs do
                product <- ol.product
                pure $ ol.configs <> mkDefaultConfigs configId product
            }
  OrderLineRemoveConfig { orderSectionId, orderLineId, configId } -> do
    let
      -- | Remove the configuration entry. If this is the last entry then we
      -- | ignore the request.
      updateOrderLine :: OrderLine -> OrderLine
      updateOrderLine ol =
        ol
          { configs =
            fromMaybe ol.configs
              $ NA.fromArray
              =<< deleteOrderLineConfig configId ol.configs
          }
    modifyInitialized $ modifyOrderForm
      $ modifyOrderLine orderSectionId orderLineId updateOrderLine
  OrderLineSetConfig { orderSectionId, orderLineId, configId, configValue } -> do
    let
      updateConfig (SS.OrderLineConfig olc) =
        SS.OrderLineConfig
          $ olc { config = configValue }
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderLine orderSectionId orderLineId
      $ modifyOrderLineConfig configId updateConfig
  OrderLineSetCharges { orderSectionId, orderLineId, charges, estimatedUsage } ->
    modifyInitialized
      $ modifyOrderForm
      $ modifyOrderLine orderSectionId orderLineId
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
        result <- H.lift $ Requests.deleteOrder orderId
        case result of
          Error msg -> H.liftEffect $ Console.error $ "Error deleting order: " <> msg
          Loaded _ -> H.liftEffect $ Console.log $ "Deleted order: " <> show orderId
          _ -> pure unit
        pure unit
      _ -> pure unit
    -- Flush the order form state from the browser store.
    H.liftEffect clearSessionOrderForm
    -- Reloading the catalog will reset the component state.
    loadCatalog Nothing Nothing
  CreateUpdateOrder -> do
    state <- H.get
    void $ createUpdateOrder state
    pure unit
  FulfillOrderStart ->
    modifyInitialized
      $ _ { orderFulfillStatus = FulfillStatusConfirming }
  FulfillOrderModalResult ConfirmFulfillModal.FulfillCancel -> do
    modifyInitialized
      $ _ { orderFulfillStatus = FulfillStatusIdle }
  FulfillOrderModalResult (ConfirmFulfillModal.FulfillConfirm result) -> do
    -- Add the note if necessary.
    case result.note of
      Nothing -> pure unit
      Just note ->
        modifyInitialized
          $ modifyOrderForm \sof ->
              sof
                { notes =
                  sof.notes
                    <> [ SS.OrderNote
                          { orderNoteId: Nothing
                          , createTime: Nothing
                          , note
                          }
                      ]
                }
    -- If the order is unsaved then make sure to save it first.
    state <- H.get
    mSavedOrder <- case state of
      Initialized (Loaded { orderForm: { changed, original: Just savedOrder } })
        | not changed -> pure $ Just savedOrder
      _ -> createUpdateOrder state
    -- Fulfill the saved order.
    case mSavedOrder of
      Just (SS.OrderForm { id: Just id }) -> do
        modifyInitialized $ _ { orderFulfillStatus = FulfillStatusInFlight }
        lOrder <- H.lift $ Requests.postOrderFulfillment id result.marioPriority result.note result.requestParticipants
        -- Updates the current state to match the response order object.
        case lOrder of
          Loaded o' -> do
            loadExisting o' false
            H.lift
              $ Alerts.push
              $ Alert.defaultAlert
                  { type_ = Alert.Success
                  , content = HH.text "Sent order for fulfillment."
                  }
          Error errMsg ->
            H.lift
              $ Alerts.push
              $ Alert.errorAlert "Failed to send order for fulfillment." errMsg
          _ -> pure unit
        modifyInitialized $ _ { orderFulfillStatus = FulfillStatusIdle }
      _ -> do
        H.liftEffect $ Console.log "Could not fulfill unsaved order"
        pure unit
  Back event -> do
    H.liftEffect $ back
 
toSeller :: SS.LegalEntity -> SS.Seller
toSeller (SS.LegalEntity le) =
  SS.Seller
    { sellerId: Nothing
    , registeredName: le.registeredName
    , novaShortName: le.novaShortName
    , address: le.address
    , contacts: le.contacts
    }