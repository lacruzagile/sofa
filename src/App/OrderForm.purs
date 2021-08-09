module App.OrderForm (Slot, proxy, component) where

import Prelude
import Affjax (printError)
import Css as Css
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Data.Array (concatMap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.SmartSpec as SS
import Data.Traversable (sequence, traverse)
import Data.Variant (default, on)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
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
    }

newtype Solution
  = Solution
  { uri :: String
  , products :: Map String SS.ConfigSchemaEntry
  }

-- Similar to SS.OrderForm but with a few optional fields.
type OrderForm
  = { id :: Maybe String
    , customer :: Maybe SS.Customer
    , status :: Maybe SS.OrderStatus
    , summary :: SS.OrderSummary
    , solutionURIs :: Array SS.Uri
    , sections :: Array SS.OrderSection
    }

data Action
  = GetMeta

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

  solution :: StateSolution -> H.ComponentHTML Action slots m
  solution s = HH.li_ [ HH.text s.name ]

  solutions :: Array StateSolution -> H.ComponentHTML Action slots m
  solutions ss =
    HH.details_
      [ HH.summary [ HP.class_ Css.button ] [ HH.text "Solutions" ]
      , HH.ul_ (map solution ss)
      ]

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

  returningCustomer = HH.div [ HP.class_ Css.tabBody ] [ HH.text "Returning Customer" ]

  section s =
    HH.div [ HP.class_ Css.orderSection ]
      [ HH.label_
          [ HH.text "Product"
          , HH.select_ products
          ]
      ]
    where
    sols = map _.solution s

    products = concatMap (map (\(SS.Product p) -> HH.option_ [ HH.text p.sku ]) <<< \(SS.Solution x) -> x.products) sols

  sections s = HH.div_ [ HH.h3_ [ HH.text "Sections" ], section s ]

  orderForm :: Array StateSolution -> OrderForm -> H.ComponentHTML Action slots m
  orderForm s _o =
    HH.div_
      [ Widgets.tabbed2 "customer"
          { label: HH.text "New Customer", content: newCustomer }
          { label: HH.text "Return Customer", content: returningCustomer }
      , sections s
      ]

  meta m =
    [ solutions m.solutions
    , orderForm m.solutions m.orderForm
    ]

  content = defRender state meta

baseUrl :: String
baseUrl = "v1alpha1/solutions"

metaUrl :: String
metaUrl = baseUrl <> "/meta.json"

solutionUrl :: String -> String
solutionUrl solName = baseUrl <> "/" <> solName <> "/nsolution.json"

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
                , solutionURIs: []
                , sections: []
                }
            }
        )
          <$> meta
          <*> solutions
    H.modify_ \_ -> res

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
