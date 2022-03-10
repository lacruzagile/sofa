module App.Orders (Slot, proxy, component) where

import Prelude
import App.OrderForm as OrderForm
import App.Requests (getOrders)
import Css as Css
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Route as Route
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

proxy :: Proxy "orders"
proxy = Proxy

type Slots
  = ( orderForm :: OrderForm.Slot Unit
    )

type State
  = Loadable StateInner

type StateInner
  = { orders :: Array SS.OrderForm
    , nextPageToken :: Maybe String
    }

data Action
  = NoOp
  | ClearState
  | LoadOrders (Maybe String)

component ::
  forall query input output m.
  MonadAff m =>
  CredentialStore m =>
  H.Component query input output m
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

initialState :: forall input. input -> State
initialState = const Idle

initialize :: Maybe Action
initialize = Just (LoadOrders Nothing)

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ renderContent ]
  where
  error err =
    HH.div
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

  idle = HH.p_ [ HH.text "Idle …" ]

  loading =
    HH.p
      [ HP.classes [ Css.c "animate-pulse", Css.c "text-2xl", Css.c "text-center" ] ]
      [ HH.text "Loading …" ]

  defRender ::
    forall a.
    Loadable a ->
    (a -> H.ComponentHTML Action Slots m) ->
    H.ComponentHTML Action Slots m
  defRender s rend = case s of
    Idle -> idle
    Loading -> loading
    Loaded dat -> rend dat
    Error err -> error err

  renderOrder :: SS.OrderForm -> H.ComponentHTML Action Slots m
  renderOrder (SS.OrderForm o) =
    trow
      [ tcell [ maybe (HH.text "N/A") Widgets.dateWithTimeTooltip o.createTime ]
      , tcell [ HH.text $ SS.prettyOrderStatus o.status ]
      , tcell [ HH.text buyer ]
      , tcell [ HH.text seller ]
      , tcell [ HH.text $ fromMaybe "" $ o.displayName ]
      ]
    where
    rowClasses = [ Css.c "table-row", Css.c "hover:bg-gray-100" ]

    trow = case o.id of
      Nothing -> HH.div [ HP.classes rowClasses ]
      Just id -> HH.a [ Route.href (Route.Order id), HP.classes rowClasses ]

    tcell = HH.div [ HP.classes [ Css.c "table-cell", Css.c "p-5" ] ]

    Tuple buyer seller =
      let
        SS.Buyer { corporateName: b } = o.buyer

        SS.Seller { registeredName: s } = o.seller
      in
        Tuple b s

  renderOrders :: StateInner -> H.ComponentHTML Action Slots m
  renderOrders { orders: os, nextPageToken } =
    HH.div_
      [ table
          [ thead
              [ trow
                  [ thcell [ HH.text "Created" ]
                  , thcell [ HH.text "Status" ]
                  , thcell [ HH.text "Customer" ]
                  , thcell [ HH.text "Legal Entity" ]
                  , thcell [ HH.text "Name" ]
                  ]
              ]
          , tbody $ map renderOrder os
          ]
      , maybe (HH.text "") renderNextPage nextPageToken
      ]
    where
    table =
      HH.div
        [ HP.classes
            [ Css.c "table"
            , Css.c "w-full"
            ]
        ]

    thead =
      HH.div
        [ HP.classes
            [ Css.c "table-header-group"
            , Css.c "font-semibold"
            , Css.c "text-left"
            , Css.c "text-sm"
            , Css.c "text-stormy-200"
            ]
        ]

    tbody = HH.div [ HP.classes [ Css.c "table-row-group" ] ]

    trow = HH.div [ HP.classes [ Css.c "table-row" ] ]

    thcell =
      HH.div
        [ HP.classes
            [ Css.c "table-cell"
            , Css.c "px-5"
            , Css.c "py-3"
            , Css.c "border-b"
            ]
        ]

  renderNextPage :: String -> H.ComponentHTML Action Slots m
  renderNextPage nextPageToken =
    HH.button
      [ HP.classes
          [ Css.c "sofa-btn-secondary"
          , Css.c "float-right"
          , Css.c "my-3"
          ]
      , HE.onClick \_ -> LoadOrders (Just nextPageToken)
      ]
      [ HH.text "Next Page" ]

  renderNewOrderLink =
    HH.a
      [ Route.href Route.OrderForm
      , HP.classes
          [ Css.c "relative"
          , Css.c "float-right"
          , Css.c "sofa-btn-primary"
          ]
      ]
      [ HH.text "+ New Order" ]

  renderContent =
    [ renderNewOrderLink
    , HH.h1_ [ HH.text "Orders" ]
    , defRender state renderOrders
    ]

handleAction ::
  forall slots output m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ClearState -> H.put Idle
  LoadOrders nextPageToken -> do
    H.put Loading
    result <- H.lift $ getOrders nextPageToken
    H.put result
