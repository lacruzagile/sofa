module App.OrderForm (Slot, proxy, component) where

import Prelude
import App.Charge as Charge
import App.OrderForm.Customer as Customer
import Control.Alternative ((<|>))
import Css as Css
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Array (modifyAt, snoc)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Estimate (Estimate(..))
import Data.Estimate as Est
import Data.Int as Int
import Data.List.Lazy as List
import Data.Loadable (Loadable(..), getJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe, maybe')
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Number.Format (toStringWith, fixed)
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

type Input
  = Maybe SS.OrderForm

data State
  = Initializing SS.OrderForm
  | Initialized (Loadable StateOrderForm)

type StateOrderForm
  = { productCatalog :: SS.ProductCatalog
    , currency :: Maybe SS.Currency
    , priceBooks :: Map String (Array PriceBook)
    -- ^ Map from solution name to price books in the current currency.
    , orderForm :: OrderForm
    }

type SubTotalEntry
  = { listPrice :: Estimate (Additive Number)
    , salesPrice :: Estimate (Additive Number)
    }

newtype SubTotal
  = SubTotal
  { onetime :: SubTotalEntry -- ^ Onetime price.
  , monthly :: SubTotalEntry -- ^ Monthly price.
  , quarterly :: SubTotalEntry -- ^ Quarterly price.
  , usage :: SubTotalEntry -- ^ Usage price.
  , segment :: SubTotalEntry -- ^ Monthly segment price.
  , quarterlySegment :: SubTotalEntry -- ^ Quarterly segment price.
  }

derive newtype instance semigroupSubTotal :: Semigroup SubTotal

derive newtype instance monoidSubTotal :: Monoid SubTotal

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
    , quantity :: QuantityMap
    , configs :: Array (Map String SS.ConfigValue)
    }

type QuantityMap
  = Charge.QuantityMap

type PriceBook
  = { id :: String
    , name :: String
    , version :: String
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
  | OrderLineSetCharge
    { sectionIndex :: Int
    , orderLineIndex :: Int
    , charge :: SS.Charge
    , quantity :: QuantityMap
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
            }
    }

initialState :: Input -> State
initialState = maybe (Initialized Idle) Initializing

initialize :: Maybe Action
initialize = Just Initialize

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ renderContent ]
  where
  renderCharge ::
    OrderLineIndex ->
    SS.ChargeUnitMap ->
    QuantityMap ->
    SS.Charge ->
    H.ComponentHTML Action Slots m
  renderCharge olIdx unitMap quantity charge =
    HH.slot Charge.proxy olIdx Charge.component
      { unitMap, charge, quantity }
      ( \result ->
          OrderLineSetCharge
            { sectionIndex: olIdx.sectionIndex
            , orderLineIndex: olIdx.orderLineIndex
            , charge: result.charge
            , quantity: result.quantity
            }
      )

  renderChargeModal ::
    OrderLineIndex ->
    SS.ChargeUnitMap ->
    QuantityMap ->
    Maybe SS.Charge ->
    Array (H.ComponentHTML Action Slots m)
  renderChargeModal olIdx unitMap quantity = maybe noCharge withCharge
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
            [ renderCharge olIdx unitMap quantity charge ]
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
                [ HH.label [ HP.classes [ Css.full, Css.fourFifth1000 ] ]
                    [ HH.text "Product"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.disabled true
                        , HP.value (show product.sku)
                        ]
                    ]
                , HH.div [ HP.classes [ Css.full, Css.fifth1000 ] ]
                    $ renderChargeModal olIdx (SS.productChargeUnits ol.product) ol.quantity ol.charge
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
          Just (SS.Currency code) ->
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
          <> [ HH.div [ HP.class_ Css.orderLine ]
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
      <> [ HH.div [ HP.class_ Css.orderSection ]
            [ HH.button
                [ HP.class_ Css.addSection, HE.onClick \_ -> AddSection ]
                [ HH.text "+" ]
            ]
        ]

  renderOrderSectionSummary :: Maybe SS.Currency -> SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSectionSummary = renderSubTotal "Sub-totals"

  renderOrderSummary :: Maybe SS.Currency -> SubTotal -> H.ComponentHTML Action Slots m
  renderOrderSummary = renderSubTotal "Totals"

  renderSubTotal :: String -> Maybe SS.Currency -> SubTotal -> H.ComponentHTML Action Slots m
  renderSubTotal title currency (SubTotal summary) =
    let
      price = renderWithCurrency currency

      ifNonZero e
        | e == mempty = const []
        | otherwise = A.singleton
    in
      HH.table [ HP.class_ Css.subTotal ]
        [ HH.tr_
            $ [ HH.th [ HP.rowSpan 2 ] [ HH.text title ] ]
            <> ifNonZero summary.usage (HH.th_ [ HH.text "Usage" ])
            <> ifNonZero summary.monthly (HH.th_ [ HH.text "Monthly" ])
            <> ifNonZero summary.quarterly (HH.th_ [ HH.text "Quarterly" ])
            <> ifNonZero summary.onetime (HH.th_ [ HH.text "Onetime" ])
        , HH.tr_
            $ []
            <> ifNonZero summary.usage (HH.td_ [ price summary.usage ])
            <> ifNonZero summary.monthly (HH.td_ [ price summary.monthly ])
            <> ifNonZero summary.quarterly (HH.td_ [ price summary.quarterly ])
            <> ifNonZero summary.onetime (HH.td_ [ price summary.onetime ])
        ]

  renderWithCurrency :: Maybe SS.Currency -> SubTotalEntry -> H.ComponentHTML Action Slots m
  renderWithCurrency currency amount = case currency of
    Nothing -> HH.text "N/A"
    Just (SS.Currency "") -> HH.text "N/A"
    Just (SS.Currency code) ->
      if amount.listPrice == amount.salesPrice then
        HH.text $ showMonetary amount.listPrice <> " " <> code
      else
        Widgets.withTooltip Widgets.Top ("Without discounts: " <> showMonetary amount.listPrice)
          $ HH.span_
              [ HH.span [ HP.style "color:red" ] [ HH.text $ showMonetary amount.salesPrice ]
              , HH.text " "
              , HH.text code
              ]

  renderCustomer :: Maybe SS.Customer -> H.ComponentHTML Action Slots m
  renderCustomer customer =
    HH.div_
      [ HH.h3_ [ HH.text "Customer" ]
      , HH.slot Customer.proxy unit Customer.component customer SetCustomer
      ]

  renderOrderForm :: StateOrderForm -> Array (H.ComponentHTML Action Slots m)
  renderOrderForm sof =
    [ renderCustomer sof.orderForm.customer
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

showMonetary :: Estimate (Additive Number) -> String
showMonetary = showEst <<< map (\(Additive n) -> toStringWith (fixed 2) n)
  where
  showEst = case _ of
    Exact s -> s
    Estimate s -> "~" <> s

fromQuantity :: Array SS.QuantityPerUnit -> QuantityMap
fromQuantity = Map.unions <<< map fromQuantityPerUnit
  where
  toEst quantity estimated
    | estimated = Estimate quantity
    | otherwise = Exact quantity

  fromQuantityPerUnit qpu = case qpu of
    SS.QuantityPerUnit { unit, quantity, estimated } -> Map.singleton unit (Left $ toEst quantity estimated)
    SS.QuantityByDimPerUnit { unit, quantityByDim } -> Map.singleton unit (Right $ fromQuantityByDim quantityByDim)

  fromQuantityByDim = Map.fromFoldable <<< map fromQuantityPerDim

  fromQuantityPerDim (SS.QuantityPerDim { dim, quantity, estimated }) = Tuple dim (toEst quantity estimated)

toQuantity :: QuantityMap -> Array SS.QuantityPerUnit
toQuantity quantityMap =
  let
    toList :: forall k v. Map k v -> List.List (Tuple k v)
    toList = Map.toUnfoldable

    transform :: forall k v a. (Tuple k v -> a) -> Map k v -> Array a
    transform f = A.fromFoldable <<< map f <<< toList

    r1 :: Tuple SS.ChargeUnitRef (Either (Estimate Int) (Map SS.DimValue (Estimate Int))) -> SS.QuantityPerUnit
    r1 (Tuple unitRef unitMap) = case unitMap of
      Left quantity ->
        SS.QuantityPerUnit
          { unit: unitRef
          , quantity: Est.toValue quantity
          , estimated: Est.isEstimate quantity
          }
      Right dimMap ->
        SS.QuantityByDimPerUnit
          { unit: unitRef
          , quantityByDim: transform r2 dimMap
          }

    r2 :: Tuple SS.DimValue (Estimate Int) -> SS.QuantityPerDim
    r2 (Tuple dim quantity) =
      SS.QuantityPerDim
        { dim
        , quantity: Est.toValue quantity
        , estimated: Est.isEstimate quantity
        }
  in
    transform r1 quantityMap

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
          { sku: _.sku $ unwrap $ ol.product
          , charge: ch
          , quantity: toQuantity ol.quantity
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
  H.put $ Initialized Loading
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
  H.put $ Initialized res

mkDefaultConfigs :: SS.Product -> Array (Map String SS.ConfigValue)
mkDefaultConfigs (SS.Product p) = maybe [] (A.singleton <<< mkDefaults) p.orderConfigSchema
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

  -- | Sets the order line charge and default quantity from the given price
  -- | book. If the order line already has a charge, then it is returned
  -- | unchanged.
  updateOrderLineCharge :: Maybe PriceBook -> OrderLine -> OrderLine
  updateOrderLineCharge mpb ol
    | isJust ol.charge = ol
    | otherwise =
      fromMaybe ol
        $ ( \pb ->
              let
                charge = lookupCharge ol.product pb
              in
                ol
                  { charge = charge
                  , quantity = mkDefaultQuantity charge
                  }
          )
        <$> mpb

  lookupCharge :: SS.Product -> PriceBook -> Maybe SS.Charge
  lookupCharge (SS.Product product) pb = do
    rateCards <- pb.rateCards
    SS.RateCard rateCard <- Map.lookup product.sku rateCards
    pure rateCard.charge

  mkDefaultQuantity :: Maybe SS.Charge -> QuantityMap
  mkDefaultQuantity Nothing = Map.empty

  mkDefaultQuantity (Just (SS.ChargeArray ces)) = List.foldl accumulate Map.empty $ unravelled
    where
    unravelled = do
      SS.ChargeElement ce <- List.fromFoldable ces
      SS.PriceByUnitPerDim { prices } <- List.fromFoldable ce.priceByUnitByDim
      SS.PricePerUnit { unit: unitRef } <- List.fromFoldable prices
      pure $ { unitRef, quantity: Exact 1 }

    accumulate acc { unitRef, quantity } = Map.alter (Just <<< Left <<< const quantity) unitRef acc

  sumOrderLines :: Array (Maybe OrderLine) -> SubTotal
  sumOrderLines = A.foldl (\a b -> a <> (conv b)) mempty

  conv :: Maybe OrderLine -> SubTotal
  conv mol =
    fromMaybe mempty
      $ do
          ol <- mol
          charge <- ol.charge
          pure $ calcCharge ol.product ol.quantity charge

  calcCharge :: SS.Product -> QuantityMap -> SS.Charge -> SubTotal
  calcCharge product quantityMap (SS.ChargeArray cs) = A.foldl (\a b -> a <> calcChargeElem b) mempty cs
    where
    priceToAmount dim (SS.PricePerUnit p) =
      let
        quantity = do
          dimMap <- Map.lookup p.unit quantityMap
          either (Just <<< identity) (Map.lookup dim) dimMap
      in
        maybe mempty (mkChargeSummary $ priceInSegment quantity p.price)
          $ chargeType p.unit product

    priceByUnitToAmount (SS.PriceByUnitPerDim p) = A.foldl (\a b -> a <> priceToAmount p.dim b) mempty p.prices

    calcChargeElem (SS.ChargeElement c) = A.foldl (\a b -> a <> priceByUnitToAmount b) mempty c.priceByUnitByDim

  mkChargeSummary :: SubTotalEntry -> SS.ChargeType -> SubTotal
  mkChargeSummary c = case _ of
    SS.ChargeTypeOnetime -> SubTotal $ zero { onetime = c }
    SS.ChargeTypeMonthly -> SubTotal $ zero { monthly = c }
    SS.ChargeTypeQuarterly -> SubTotal $ zero { quarterly = c }
    SS.ChargeTypeUsage -> SubTotal $ zero { usage = c }
    SS.ChargeTypeSegment -> SubTotal $ zero { segment = c }
    SS.ChargeTypeQuarterlySegment -> SubTotal $ zero { quarterlySegment = c }
    where
    SubTotal zero = mempty

  chargeType (SS.ChargeUnitRef unit) (SS.Product { chargeUnits }) =
    map (\(SS.ChargeUnit u) -> u.chargeType)
      $ A.find (\(SS.ChargeUnit u) -> u.id == unit.unitID)
      $ chargeUnits

  priceInSegment :: Maybe (Estimate Int) -> SS.Price -> SubTotalEntry
  priceInSegment quantity (SS.Price segments) = A.foldl (\a b -> a <> calcSegmentPrice b) mempty segments
    where
    calcSegmentPrice (SS.PricePerSegment p) =
      let
        q = maybe (Exact 0.0) (map Int.toNumber) quantity
      in
        maybe'
          ( \_ ->
              { listPrice: Additive <<< (p.listPrice * _) <$> q
              , salesPrice: Additive <<< (p.listPrice * _) <$> q
              }
          )
          ( \sp ->
              { listPrice: Additive <<< (p.listPrice * _) <$> q
              , salesPrice: Additive <<< (sp * _) <$> q
              }
          )
          p.salesPrice

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

loadExisting ::
  forall slots output m.
  MonadAff m =>
  SS.OrderForm ->
  H.HalogenM State Action slots output m Unit
loadExisting (SS.OrderForm orderForm) = do
  H.put $ Initialized Loading
  productCatalog <- H.liftAff $ getJson "v1alpha1/examples/product-catalog.normalized.json"
  let
    result = convertOrderForm <$> productCatalog
  H.put $ Initialized result
  where
  convertOrderForm :: SS.ProductCatalog -> StateOrderForm
  convertOrderForm productCatalog =
    let
      currency = toCurrency orderForm.customer

      priceBooks = mkPriceBooks productCatalog orderForm.customer
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
    solution <- List.find (\(SS.Solution { uri }) -> Just s.solutionURI == uri) $ Map.values solutions
    let
      SS.PriceBookRef pbRef = s.basePriceBook

      SS.Solution sol = solution

      priceBook = A.find (\pb -> pb.id == pbRef.priceBookID) =<< Map.lookup sol.id pbs
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
      { product
      , charge: Just l.charge
      , quantity: fromQuantity l.quantity
      , configs: l.configs
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

toCurrency :: SS.Customer -> Maybe SS.Currency
toCurrency = case _ of
  SS.NewCustomer cust -> case cust.commercial of
    SS.Commercial comm -> Just comm.priceCurrency
  _ -> Nothing

-- | Assemble a map from solution ID to its associated price books. The price
-- | books are limited to the currency of the given customer.
mkPriceBooks :: SS.ProductCatalog -> SS.Customer -> Map String (Array PriceBook)
mkPriceBooks (SS.ProductCatalog pc) customer = maybe Map.empty (Map.fromFoldableWith (<>) <<< mkPriceBookPairs) $ toCurrency customer
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
            , name: pb.name
            , version: pbv.version
            , rateCards: rateCardMap <$> pbc.rateCards
            }
          ]
      ]
    else
      []

handleAction ::
  forall slots output m.
  MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  Initialize -> do
    st <- H.get
    case st of
      Initializing orderForm -> loadExisting orderForm
      Initialized Idle -> loadCatalog "v1alpha1/examples/product-catalog.normalized.json"
      _ -> pure unit
  SetCustomer customer ->
    modifyInitialized
      $ \st ->
          let
            currency = toCurrency customer

            priceBooks = mkPriceBooks st.productCatalog customer
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
        { product
        , charge: Nothing
        , quantity: Map.empty
        , configs: mkDefaultConfigs product
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
      modifyInitialized
        $ \st -> modifyOrderLine sectionIndex orderLineIndex st updateOrderLine
  OrderLineSetCharge { sectionIndex, orderLineIndex, charge, quantity } ->
    modifyInitialized
      $ \st ->
          modifyOrderLine sectionIndex orderLineIndex st
            _
              { charge = Just charge
              , quantity = quantity
              }
