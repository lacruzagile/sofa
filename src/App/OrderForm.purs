module App.OrderForm (Slot, proxy, component) where

import Prelude
import Affjax (printError)
import Css as Css
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Data.Array (deleteAt, mapWithIndex, modifyAt, snoc)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
import Data.String as String
import Data.Traversable (sequence, traverse)
import Data.Variant (default, on)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.Ajax (_affjaxError, _notFound, _parseError)
import Simple.Ajax as AJX
import Type.Proxy (Proxy(..))
import Widgets as Widgets

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "orderForm"
proxy = Proxy

type State
  = SubState StateMeta

data SubState a
  = Idle
  | Success a
  | Loading
  | Error String

instance functorSubState :: Functor SubState where
  map f = case _ of
    Idle -> Idle
    Success x -> Success $ f x
    Loading -> Loading
    Error x -> Error x

instance applySubState :: Apply SubState where
  apply Idle _ = Idle
  apply (Success f) r = f <$> r
  apply Loading _ = Loading
  apply (Error e) _ = Error e

instance applicationSubState :: Applicative SubState where
  pure = Success

instance bindSubState :: Bind SubState where
  bind Idle _ = Idle
  bind (Success x) f = f x
  bind Loading _ = Loading
  bind (Error e) _ = Error e

type StateMeta
  = { meta :: SS.Meta
    , solutions :: Array StateSolution
    , orderForm :: OrderForm
    }

type StateSolution
  = { name :: String
    , solution :: SS.Solution
    --   , products :: Map String SS.ConfigSchemaEntry
    }

-- Similar to SS.OrderForm but with a few optional fields.
type OrderForm
  = { id :: Maybe String
    , customer :: Maybe SS.Customer
    , status :: Maybe SS.OrderStatus
    , summary :: SS.OrderSummary
    , sections :: Array OrderSection
    }

newtype OrderSection
  = OrderSection
  { solution :: SS.Solution
  , orderLines :: Array SS.OrderLine
  , summary :: SS.OrderSectionSummary
  }

data Action
  = GetMeta
  | AddSection { solution :: SS.Solution }
  | RemoveSection { sectionIndex :: Int }
  | AddOrderLine { sectionIndex :: Int }
  | RemoveOrderLine { sectionIndex :: Int, orderLineIndex :: Int }
  | UpdateOrderLineQuantity
    { sectionIndex :: Int, orderLineIndex :: Int, quantity :: Int
    }

component ::
  forall query input output m.
  MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just GetMeta }
    }

initialState :: forall input. input -> State
initialState _ = Idle

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div_
    ( [ HH.h1_
          [ HH.text "Order Form" ]
      ]
        <> content
    )
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

  loading = [ HH.p_ [ HH.text "Loading …" ] ]

  defRender ::
    forall a.
    SubState a ->
    (a -> Array (H.ComponentHTML Action slots m)) ->
    Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> []
    Loading -> loading
    Success dat -> rend dat
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
    ]

  seller =
    [ HH.label [ HP.for "of-seller", HP.class_ Css.button ] [ HH.text "Seller" ]
    , Widgets.modal "of-seller" "Seller"
        $ [ HH.label_
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
    ]

  newCustomer = HH.div [ HP.class_ Css.tabBody ] (commercial <> purchaser <> seller)

  billingAccountRef =
    [ HH.label [ HP.for "of-billing-account", HP.class_ Css.button ] [ HH.text "Billing Account" ]
    , Widgets.modal "of-billing-account" "Billing Account"
        $ [ HH.label_
              [ HH.text "Identifier"
              , HH.input [ HP.type_ HP.InputText, HP.name "billing-account-id" ]
              ]
          ]
    ]

  returnCustomer =
    HH.div [ HP.class_ Css.tabBody ]
      ( commercial <> billingAccountRef
      )

  orderLine :: Array (H.ComponentHTML Action slots m) -> Int -> Int -> SS.OrderLine -> H.ComponentHTML Action slots m
  orderLine products secIdx olIdx (SS.OrderLine ol) =
    HH.div [ HP.classes [ Css.orderSection ] ]
      [ HH.a
          [ HP.class_ Css.close
          , HE.onClick \_ ->
              RemoveOrderLine
                { sectionIndex: secIdx
                , orderLineIndex: olIdx
                }
          ]
          [ HH.text "×" ]
      , HH.label_
          [ HH.text "Product"
          , HH.select_ products
          ]
      , HH.label_
          [ HH.text "Quantity"
          , HH.input
              [ HP.type_ HP.InputNumber
              , HE.onValueChange \input ->
                  UpdateOrderLineQuantity
                    { sectionIndex: secIdx
                    , orderLineIndex: olIdx
                    , quantity: maybe 0 identity $ Int.fromString input
                    }
              ]
          ]
      ]

  section :: Int -> OrderSection -> H.ComponentHTML Action slots m
  section idx (OrderSection sec) =
    HH.div [ HP.classes [ Css.orderSection ] ]
      $ [ HH.a [ HP.class_ Css.close, HE.onClick \_ -> RemoveSection { sectionIndex: idx } ] [ HH.text "×" ]
        , HH.label_
            [ HH.text "Solution"
            , HH.input [ HP.type_ HP.InputText, HP.disabled true, HP.value solution.description ]
            ]
        ]
      <> sectionBody sec.solution
    where
    SS.Solution solution = sec.solution

    SS.OrderSectionSummary summary = sec.summary

    sectionBody (SS.Solution sol) =
      let
        products = map (\(SS.Product p) -> HH.option_ [ HH.text p.sku ]) sol.products
      in
        [ HH.label_
            [ HH.text "Top Level Product"
            , HH.select_ products
            ]
        , HH.div_ (mapWithIndex (orderLine products idx) sec.orderLines)
        , HH.button [ HE.onClick \_ -> AddOrderLine { sectionIndex: idx } ] [ HH.text "Add Order Line" ]
        , HH.div [ HP.classes [ Css.flex, Css.four ] ]
            [ HH.div_ [ HH.strong_ [ HH.text "Sub-totals" ] ]
            , HH.div_ [ HH.text "Estimated usage: ", HH.text (show summary.estimatedUsageSubTotal) ]
            , HH.div_ [ HH.text "Monthly: ", HH.text (show summary.monthlySubTotal) ]
            , HH.div_ [ HH.text "Onetime: ", HH.text (show summary.onetimeSubTotal) ]
            ]
        ]

  sections :: Array StateSolution -> Array OrderSection -> H.ComponentHTML Action slots m
  sections stateSols secs =
    HH.div_
      $ [ HH.h3_ [ HH.text "Sections" ]
        ]
      <> mapWithIndex section secs
      <> addSection
    where
    solutionButtons = map (\s -> HH.button [ HE.onClick \_ -> AddSection { solution: s.solution } ] [ HH.text s.name ]) stateSols

    addSection =
      [ HH.label [ HP.for "of-add-section", HP.class_ Css.button ] [ HH.text "Add Section" ]
      , Widgets.modal "of-add-section" "Choose Section Solution" solutionButtons
      ]

  orderForm :: Array StateSolution -> OrderForm -> H.ComponentHTML Action slots m
  orderForm sols order =
    HH.div_
      [ Widgets.tabbed2 "customer"
          { label: HH.text "New Customer", content: newCustomer }
          { label: HH.text "Return Customer", content: returnCustomer }
      , sections sols order.sections
      , HH.div [ HP.classes [ Css.flex, Css.four ] ]
          [ HH.div_ [ HH.strong_ [ HH.text "Totals" ] ]
          , HH.div_ [ HH.text "Estimated usage: ", HH.text (show summary.estimatedUsageTotal) ]
          , HH.div_ [ HH.text "Monthly: ", HH.text (show summary.monthlyTotal) ]
          , HH.div_ [ HH.text "Onetime: ", HH.text (show summary.onetimeTotal) ]
          ]
      ]
    where
    SS.OrderSummary summary = order.summary

  meta m = [ orderForm m.solutions m.orderForm ]

  content = defRender state meta

baseUrl :: String
baseUrl = "v1alpha1/solutions"

metaUrl :: String
metaUrl = baseUrl <> "/meta.json"

solutionUrl :: String -> String
solutionUrl solUri =
  if isWebUrl then
    solUri
  else
    baseUrl <> "/" <> solUri <> "/nsolution.json"
  where
  isWebUrl = String.contains (String.Pattern "^https?:") solUri

getSolution ::
  forall m.
  Bind m => MonadAff m => String -> m (SubState StateSolution)
getSolution name = getJson (solutionUrl name) (\sol -> { name, solution: sol })

getSolutions ::
  forall m.
  Bind m => MonadAff m => SubState SS.Meta -> m (SubState (Array StateSolution))
getSolutions = case _ of
  Idle -> pure Idle
  Success meta -> sequence <$> get meta
  Loading -> pure Idle
  Error x -> pure $ Error x
  where
  get :: SS.Meta -> m (Array (SubState StateSolution))
  get meta = traverse getSolution meta.solutions

handleAction ::
  forall o m.
  MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  GetMeta -> do
    H.modify_ \_ -> Loading
    meta <- getJson metaUrl identity
    solutions <- getSolutions meta
    let
      res =
        ( \(m :: SS.Meta) (ss :: Array StateSolution) ->
            { meta: m
            , solutions: ss
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
          <$> meta
          <*> solutions
    H.modify_ \_ -> res
  AddSection { solution } ->
    H.modify_
      $ map \st ->
          st
            { orderForm
              { sections =
                snoc
                  st.orderForm.sections
                  ( OrderSection
                      { solution
                      , orderLines: []
                      , summary:
                          SS.OrderSectionSummary
                            { estimatedUsageSubTotal: 0.0
                            , monthlySubTotal: 0.0
                            , onetimeSubTotal: 0.0
                            }
                      }
                  )
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
      addOrderLine :: OrderSection -> OrderSection
      addOrderLine (OrderSection section) =
        OrderSection
          $ section
              { orderLines =
                snoc section.orderLines
                  ( SS.OrderLine
                      { product: SS.ProductInstance {}
                      , quantity: 1
                      , onetimeCharges: []
                      , monthlyCharges: []
                      , usageCharges: []
                      }
                  )
              }
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
      removeOrderLine :: OrderSection -> OrderSection
      removeOrderLine (OrderSection section) =
        OrderSection
          $ section
              { orderLines =
                maybe section.orderLines identity (deleteAt orderLineIndex section.orderLines)
              }
    in
      H.modify_
        $ map \st ->
            st
              { orderForm
                { sections =
                  maybe st.orderForm.sections identity (modifyAt sectionIndex removeOrderLine st.orderForm.sections)
                }
              }
  UpdateOrderLineQuantity { sectionIndex, orderLineIndex, quantity } ->
    let
      updateQuantity :: SS.OrderLine -> SS.OrderLine
      updateQuantity (SS.OrderLine ol) = SS.OrderLine $ ol { quantity = quantity }

      updateOrderLine :: OrderSection -> OrderSection
      updateOrderLine (OrderSection section) =
        OrderSection
          $ section
              { orderLines =
                maybe section.orderLines identity (modifyAt orderLineIndex updateQuantity section.orderLines)
              }

      updateSections :: Array OrderSection -> Array OrderSection
      updateSections sections =
        maybe sections identity
          $ modifyAt sectionIndex updateOrderLine
          $ sections
    in
      H.modify_
        $ map \st -> st { orderForm { sections = updateSections st.orderForm.sections } }

getJson ::
  forall m a b.
  Bind m => MonadAff m => DecodeJson a => String -> (a -> b) -> m (SubState b)
getJson url handle = do
  res <- H.liftAff (AJX.get url)
  case res of
    Left error ->
      let
        errorStr =
          default "Generic error"
            # on _affjaxError printError
            # on _notFound (const "Not found")
            # on _parseError printJsonDecodeError
            $ error
      in
        pure $ Error errorStr
    Right content -> pure $ Success $ handle content