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
  = Loadable { orders :: Array SS.OrderForm }

data Action
  = NoOp
  | ClearState
  | LoadOrders

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
initialize = Just LoadOrders

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

  renderOrders :: { orders :: Array SS.OrderForm } -> H.ComponentHTML Action Slots m
  renderOrders { orders: os } =
    table
      [ thead
          [ trow
              [ tcell [ HH.text "Created" ]
              , tcell [ HH.text "Status" ]
              , tcell [ HH.text "Buyer" ]
              , tcell [ HH.text "Seller" ]
              , tcell [ HH.text "Name" ]
              ]
          ]
      , tbody $ map renderOrder os
      ]
    where
    table =
      HH.div
        [ HP.classes
            [ Css.c "table"
            , Css.c "w-full"
            , Css.c "bg-white"
            , Css.c "shadow-sm"
            , Css.c "rounded-md"
            , Css.c "overflow-hidden"
            ]
        ]

    thead =
      HH.div
        [ HP.classes
            [ Css.c "table-header-group"
            , Css.c "bg-gray-200"
            , Css.c "uppercase"
            , Css.c "text-left"
            , Css.c "text-sm"
            , Css.c "text-gray-600"
            , Css.c "border-b"
            ]
        ]

    tbody = HH.div [ HP.classes [ Css.c "table-row-group" ] ]

    trow = HH.div [ HP.classes [ Css.c "table-row" ] ]

    tcell = HH.div [ HP.classes [ Css.c "table-cell", Css.c "px-5", Css.c "py-3" ] ]

  renderNewOrderLink =
    HH.a
      [ Route.href Route.OrderForm
      , HP.classes
          [ Css.c "relative"
          , Css.c "float-right"
          , Css.c "sofa-btn-tropical"
          ]
      ]
      [ HH.text "+ New Order" ]

  renderContent =
    [ renderNewOrderLink
    , HH.h1_ [ HH.text "Orders" ]
    , defRender state renderOrders
    ]

loadOrders ::
  forall slots output m.
  MonadAff m =>
  CredentialStore m =>
  H.HalogenM State Action slots output m Unit
loadOrders = do
  H.modify_ \_ -> Loading
  orders <- H.lift getOrders
  H.modify_ \_ -> (\os -> { orders: os }) <$> orders

handleAction ::
  forall slots output m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ClearState -> H.put Idle
  LoadOrders -> loadOrders
