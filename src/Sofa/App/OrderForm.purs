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
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (tuple3, uncurry3)
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
import Sofa.App.OrderForm.Buyer as Buyer
import Sofa.App.OrderForm.Commercial as Commercial
import Sofa.App.OrderForm.ConfigSchema as ConfigSchema
import Sofa.App.OrderForm.Notes as Notes
import Sofa.App.OrderForm.Observers as Observers
import Sofa.App.OrderForm.SelectOrderStatus as SelectOrderStatus
import Sofa.App.OrderForm.SelectProduct as SelectProduct
import Sofa.App.OrderForm.Seller as Seller
import Sofa.App.Requests (deleteFile, deleteOrder, deleteOrderLine, deleteOrderSection, getOrder, getOrderForQuote, getProductCatalog, patchOrder, postOrder, postOrderFulfillment)
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
import Sofa.Component.EditableInput as EditableInput
import Sofa.Component.Icon as Icon
import Sofa.Component.Select as Select
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Charge (ChargeUnitMap, dims, productChargeUnitMap, unitIds) as Charge
import Sofa.Data.Currency (mkCurrency, unsafeMkCurrency)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Quantity (QuantityMap, Quantity, fromSmartSpecQuantity, toSmartSpecQuantity)
import Sofa.Data.SmartSpec as SS
import Sofa.Data.SubTotal (SubTotal)
import Sofa.Data.SubTotal as SubTotal
import Sofa.HtmlUtils (scrollToElement)
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
    , productConfig :: ConfigSchema.Slot ConfigIndex
    , charge :: Charge.Slot OrderLineIndex
    , orderName :: EditableInput.Slot Unit
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
  | GotoSection { sectionIndex :: Int }
  | GotoOrderLine { sectionIndex :: Int, orderLineIndex :: Int }
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
  forall query output f m.
  MonadAff m =>
  CredentialStore f m =>
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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ renderContent ]
  where
  renderSmallTitle t = HH.div [ Css.class_ "sofa-small-title" ] [ HH.text t ]

  renderCharges ::
    OrderLineIndex ->
    Charge.ChargeUnitMap ->
    SS.ChargeCurrency ->
    QuantityMap ->
    Array SS.Charge ->
    H.ComponentHTML Action Slots m
  renderCharges olIdx unitMap defaultCurrency estimatedUsage charges =
    HH.slot Charge.proxy olIdx Charge.component
      { unitMap
      , defaultCurrency
      , charges
      , estimatedUsage
      , priceOnly: false
      , readOnly: not isInDraft
      }
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
    H.ComponentHTML Action Slots m
  renderChargeDetails olIdx unitMap defaultCurrency estimatedUsage = maybe empty withCharges
    where
    empty = HH.text ""

    withCharges = case _ of
      [] -> empty
      charges ->
        HH.details [ Css.class_ "mt-5" ]
          [ HH.summary
              [ Css.classes [ "text-lg", "cursor-pointer" ] ]
              [ HH.text "Charges" ]
          , renderCharges olIdx unitMap defaultCurrency estimatedUsage charges
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

        renderProductTitle = case product.title of
          Nothing -> HH.text (show product.sku)
          Just t ->
            HH.div_
              [ HH.span
                  [ Css.classes [ "text-lg", "font-semibold" ] ]
                  [ HH.text t ]
              , HH.br_
              , HH.span
                  [ Css.classes [ "text-xs", "text-stormy-300" ] ]
                  [ HH.text (show product.sku) ]
              ]
      in
        body
          $ [ HH.div [ Css.class_ "flex" ]
                [ HH.div [ Css.class_ "w-3/5" ]
                    [ renderSmallTitle "Product"
                    , renderProductTitle
                    ]
                , HH.div [ Css.class_ "w-1/5" ]
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
                          [ Css.class_ "font-semibold" ]
                          [ wrap (SS.prettyOrderLineStatus ol.status) ]
                    ]
                , HH.label [ Css.class_ "w-1/5" ]
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
            , renderChargeDetails olIdx ol.unitMap defaultCurrency ol.estimatedUsage ol.charges
            ]
          <> ( if isNothing product.orderConfigSchema then
                []
              else
                [ HH.details [ Css.classes [ "mt-5" ] ]
                    $ [ HH.summary
                          [ Css.classes [ "text-lg", "cursor-pointer" ] ]
                          [ HH.text "Configuration" ]
                      ]
                    <> renderProductConfigs product ol.orderLineId ol.configs
                ]
            )
    where
    body subBody =
      HH.div
        [ Css.classes [ "m-5", "mr-0", "border-t" ]
        , HP.ref $ orderLineRefLabel olIdx.sectionIndex olIdx.orderLineIndex
        ]
        subBody

    renderQuantityInput cfgIdx (SS.OrderLineConfig olc) =
      HH.input
        [ Css.classes
            [ "nectary-input"
            , "nectary-input-number"
            , "w-full"
            , "max-w-96"
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

    renderProductConfig allowRemove product orderLineId cfgIdx (SS.OrderLineConfig { id: configId, config }) =
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
                  [ Css.classes
                      [ "relative"
                      , "float-right"
                      , "ml-2"
                      , "text-stormy-300"
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
          , case config of
              Nothing -> HH.text ""
              Just c ->
                maybe (HH.text "")
                  ( \schema ->
                      HH.slot
                        (Proxy :: Proxy "productConfig")
                        { sectionIndex: olIdx.sectionIndex
                        , orderLineIndex: olIdx.orderLineIndex
                        , configIndex: cfgIdx
                        }
                        ConfigSchema.component
                        { orderLineId
                        , configValue: c
                        , schemaEntry: schema
                        , readOnly: not isInDraft
                        , dataSourceVars:
                            { getCommercial:
                                \_ -> case state of
                                  Initialized (Loaded { orderForm: { commercial } }) -> commercial
                                  _ -> Nothing
                            }
                        , getConfigs: orderSchemaGetConfigs state olIdx
                        }
                        ( \value ->
                            OrderLineSetConfig
                              { sectionIndex: olIdx.sectionIndex
                              , orderLineIndex: olIdx.orderLineIndex
                              , configIndex: cfgIdx
                              , alter: const value
                              }
                        )
                  )
                  product.orderConfigSchema
          ]
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
          $ [ HH.div [ Css.classes [ "flex", "flex-wrap", "gap-4", "items-end" ] ]
                [ HH.div [ Css.class_ "grow" ]
                    [ HH.h2 [ Css.class_ "mt-0" ] [ HH.text "Section" ]
                    , HH.div
                        [ Css.classes [ "font-semibold", "text-lg" ] ]
                        [ HH.text "Solution – "
                        , HH.text $ solutionLabel sec.solution
                        ]
                    ]
                , if A.null priceBookOpts then
                    HH.text ""
                  else
                    HH.label_
                      [ HH.div [ Css.class_ "font-semibold" ] [ HH.text "Price book" ]
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
                [ Css.classes
                    [ "flex"
                    , "items-center"
                    , "space-x-4"
                    , "m-5"
                    , "mb-0"
                    , "pt-3"
                    , "border-t"
                    ]
                ]
                [ renderOrderSectionSummary sec.summary
                , HH.div [ Css.class_ "grow" ] []
                , if not isInDraft || isAddingOrderLine then
                    HH.text ""
                  else
                    HH.button
                      [ Css.classes
                          [ "nectary-btn-secondary"
                          , "px-6"
                          , "gap-x-4"
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
      HH.div
        [ Css.classes [ "p-6", "rounded-md", "bg-snow-100" ]
        , HP.ref $ sectionRefLabel secIdx
        ]
        subBody

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
          [ HH.h2 [ Css.classes [ "grow", "m-0" ] ] [ HH.text "Section summary" ]
          , HH.button
              [ Css.classes [ "nectary-btn-primary", "h-8" ]
              , HE.onClick \_ -> AddSection
              ]
              [ HH.text "Add section"
              ]
          ]
      , HH.table [ Css.classes [ "table-auto", "w-full" ] ]
          [ HH.thead_
              [ HH.tr [ Css.classes [ "border-b", "border-stormy-500" ] ]
                  [ th [ HH.text "Name" ]
                  , th [ HH.text "Status" ]
                  , th [ HH.text "Quantity" ]
                  , th [ HH.text "Edit" ]
                  ]
              ]
          , HH.tbody_
              $ if A.null sof.orderForm.sections then
                  [ HH.tr_
                      [ HH.td
                          [ HP.colSpan 4
                          , Css.classes [ "p-2", "text-stormy-300", "text-center" ]
                          ]
                          [ HH.text "No sections added" ]
                      ]
                  ]
                else
                  A.concat $ A.mapWithIndex sectionRow sof.orderForm.sections
          ]
      ]
    where
    th = HH.th [ Css.classes [ "p-2", "font-semibold", "text-left" ] ]

    tdDelete onClick =
      HH.td_
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

    sectionRow _ Nothing = [ HH.text "" ]

    sectionRow sectionIndex (Just { solution: SS.Solution sol, orderLines }) =
      [ HH.tr_
          [ HH.td_
              [ HH.button
                  [ Css.classes [ "p-2", "w-full", "text-left", "hover:bg-snow-500" ]
                  , HE.onClick \_ -> GotoSection { sectionIndex }
                  ]
                  [ HH.span [ Css.class_ "text-tropical-500" ] [ HH.text "Solution" ]
                  , HH.br_
                  , HH.text $ fromMaybe (show sol.id) sol.title
                  ]
              ]
          , HH.td [ HP.colSpan 2 ] []
          , tdDelete \_ -> RemoveSection { sectionIndex }
          ]
      ]
        <> A.mapWithIndex (orderRow sectionIndex) orderLines

    orderRow _ _ Nothing = HH.text ""

    orderRow sectionIndex orderLineIndex (Just ol@{ product: SS.Product prod, status }) =
      HH.tr_
        [ HH.td_
            [ HH.button
                [ Css.classes [ "p-2", "pl-12", "w-full", "text-left", "hover:bg-snow-500" ]
                , HE.onClick \_ -> GotoOrderLine { sectionIndex, orderLineIndex }
                ]
                [ HH.span [ Css.class_ "text-tropical-500" ] [ HH.text "Product" ]
                , HH.br_
                , HH.text $ fromMaybe (show prod.sku) prod.title
                ]
            ]
        , HH.td [ Css.class_ "p-2" ]
            [ HH.span
                [ Css.classes [ "nectary-tag", "w-fit" ] ]
                [ HH.text $ SS.prettyOrderLineStatus status ]
            ]
        , HH.td
            [ Css.class_ "p-2" ]
            [ HH.text $ show $ orderLineQuantity ol ]
        , tdDelete \_ -> RemoveOrderLine { sectionIndex, orderLineIndex }
        ]

  renderSections ::
    StateOrderForm ->
    Array (Maybe OrderSection) ->
    H.ComponentHTML Action Slots m
  renderSections sof secs =
    HH.div [ Css.classes [ "flex", "flex-col", "space-y-5" ] ]
      $ A.mapWithIndex (renderSection sof) secs

  renderOrderSectionSummary :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSectionSummary = Widgets.subTotalTable ""

  renderOrderSummary :: SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSummary subTotal
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
    entry = HH.div [ Css.classes [ "flex" ] ]

    title t = HH.h4 [ Css.classes [ "w-40" ] ] [ HH.text t ]

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
          [ title "Legal entity"
          , renderSeller
          ]
      , HH.div [ Css.class_ "flex" ]
          [ title "Customer"
          , renderBuyer
          ]
      , HH.div [ Css.class_ "flex" ]
          [ title "Commercial"
          , renderCommercial
          ]
      ]
    where
    title t = HH.h4 [ Css.classes [ "w-40" ] ] [ HH.text t ]

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
      [ Css.classes
          [ "flex"
          , "flex-wrap-reverse"
          , "items-center"
          , "space-x-4"
          ]
      ]
      [ renderOrderSummary sof.orderForm.summary
      , HH.div [ Css.class_ "grow" ] []
      , if isFreshOrder then
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
          HH.button
            [ Css.class_ "nectary-btn-primary"
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
          [ Css.class_ "nectary-btn-primary"
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
    [ HH.h1_ [ HH.text "Order form" ] ]
      <> case state of
          Initializing _ -> []
          Initialized state' -> defRender state' renderOrderForm

orderSchemaGetConfigs ∷
  State ->
  OrderLineIndex ->
  Unit ->
  Array (Tuple SS.SkuCode (Array SS.OrderLineConfig))
orderSchemaGetConfigs state olIdx _ = case state of
  Initialized (Loaded { orderForm: { sections } }) -> do
    section <-
      maybe [] A.singleton
        $ join
        $ A.index sections olIdx.sectionIndex
    mOrderLine <- section.orderLines
    case mOrderLine of
      Nothing -> []
      Just orderLine ->
        let
          SS.Product { sku } = orderLine.product
        in
          [ Tuple sku orderLine.configs ]
  _ -> []

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
  forall slots output f m.
  MonadAff m =>
  CredentialStore f m =>
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

sectionRefLabel :: Int -> H.RefLabel
sectionRefLabel sectionIndex = H.RefLabel $ "section-" <> show sectionIndex

orderLineRefLabel :: Int -> Int -> H.RefLabel
orderLineRefLabel sectionIndex orderLineIndex =
  H.RefLabel
    $ "orderline-"
    <> show sectionIndex
    <> "-"
    <> show orderLineIndex

handleAction ::
  forall output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  Initialize -> do
    st <- H.get
    let
      load = case _ of
        Error err -> H.put $ Initialized (Error err)
        Idle -> H.put $ Initialized Idle
        Loaded order -> loadExisting order
        Loading -> H.put $ Initialized Loading
    case st of
      Initializing NewOrder -> loadCatalog Nothing
      Initializing (ExistingOrder orderForm) -> loadExisting orderForm
      Initializing (ExistingOrderId id) -> do
        H.put $ Initialized Loading
        orderForm <- H.lift $ getOrder id
        load orderForm
      Initializing (ExistingCrmQuoteId crmQuoteId) -> do
        H.put $ Initialized Loading
        orderForm <- H.lift $ getOrderForQuote crmQuoteId
        load orderForm
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
  GotoSection { sectionIndex } -> scrollToElement $ sectionRefLabel sectionIndex
  GotoOrderLine { sectionIndex, orderLineIndex } ->
    scrollToElement
      $ orderLineRefLabel sectionIndex orderLineIndex
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
    deleteResult <-
      H.lift
        $ maybe' (pure <<< Loaded) (uncurry deleteOrderSection)
        $ case state of
            Initialized (Loaded { orderForm: { original: Just (SS.OrderForm { id: Just orderId }), sections } }) -> do
              section <- join $ A.index sections sectionIndex
              sectionId <- section.orderSectionId
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
                  fromMaybe order.sections $ A.deleteAt sectionIndex order.sections
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
  AddOrderLine { sectionIndex } -> do
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
    deleteResult <-
      H.lift
        $ maybe' (pure <<< Loaded) (uncurry3 deleteOrderLine)
        $ case state of
            Initialized (Loaded { orderForm: { original: Just (SS.OrderForm { id: Just orderId }), sections } }) -> do
              section <- join $ A.index sections sectionIndex
              orderLine <- join $ A.index section.orderLines orderLineIndex
              sectionId <- section.orderSectionId
              lineId <- orderLine.orderLineId
              pure (tuple3 orderId sectionId lineId)
            _ -> Nothing
    case deleteResult of
      Loading -> pure unit
      Idle -> pure unit
      Loaded _ -> do
        modifyInitialized
          $ modifyOrderForm
          $ modifyOrderSection sectionIndex \section ->
              section
                { orderLines =
                  fromMaybe section.orderLines
                    $ A.deleteAt orderLineIndex section.orderLines
                }
        H.lift
          $ Alerts.push
          $ Alert.defaultAlert
              { type_ = Alert.Success
              , content = HH.text "Deleted order line"
              }
      Error errMsg ->
        H.lift
          $ Alerts.push
          $ Alert.defaultAlert
              { type_ = Alert.Error
              , content =
                HH.div_
                  [ HH.p_ [ HH.text "Error deleting order line" ]
                  , HH.p [ Css.classes [ "mt-1", "text-sm" ] ]
                      [ HH.strong_ [ HH.text "Error" ]
                      , HH.text ": "
                      , HH.text errMsg
                      ]
                  ]
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
        , unitMap: Charge.productChargeUnitMap prod
        , configs: mkDefaultConfigs configId prod
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
          -- v5 UUID instead. Its namespace is the ID of the originating order
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
  OrderLineRemoveConfig { sectionIndex, orderLineIndex, configIndex } -> do
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
          H.lift $ alert order
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
