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
    [ HH.div
        [ HP.classes
            [ Css.tw.p5
            , Css.tw.bgRed100
            , Css.tw.border
            , Css.tw.borderRed400
            , Css.tw.textRed700
            ]
        ]
        [ HH.h3 [ HP.classes [ Css.tw.textLg ] ] [ HH.text "Error" ]
        , HH.p_ [ HH.text err ]
        ]
    ]

  idle = [ HH.p_ [ HH.text "Idle …" ] ]

  loading = [ HH.p_ [ HH.text "Loading …" ] ]

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

  renderOrder :: SS.OrderForm -> H.ComponentHTML Action Slots m
  renderOrder (SS.OrderForm o) =
    trow
      [ tcell [ maybe (HH.text "N/A") renderDateTime o.createTime ]
      , tcell [ HH.text $ SS.prettyOrderStatus o.status ]
      , tcell [ HH.text buyer ]
      , tcell [ HH.text seller ]
      , tcell [ HH.text $ fromMaybe "" $ o.displayName ]
      ]
    where
    rowClasses = [ Css.tw.tableRow, Css.tw.hoverBgGray100 ]

    trow = case o.id of
      Nothing -> HH.div [ HP.classes rowClasses ]
      Just id -> HH.a [ Route.href (Route.Order id), HP.classes rowClasses ]

    tcell = HH.div [ HP.classes [ Css.tw.tableCell, Css.tw.p5 ] ]

    renderDateTime t =
      Widgets.withTooltip_
        Widgets.Top
        (SS.prettyDateTime t)
        (HH.text $ SS.prettyDate t)

    Tuple buyer seller =
      let
        SS.Buyer { corporateName: b } = o.buyer

        SS.Seller { registeredName: s } = o.seller
      in
        Tuple b s

  renderOrders :: { orders :: Array SS.OrderForm } -> Array (H.ComponentHTML Action Slots m)
  renderOrders { orders: os } =
    [ renderNewOrderLink
    , HH.h1_ [ HH.text "Orders" ]
    , table
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
    ]
    where
    renderNewOrderLink =
      HH.a
        [ Route.href Route.OrderForm
        , HP.classes
            [ Css.tw.relative
            , Css.tw.floatRight
            , Css.btnSky100
            ]
        ]
        [ HH.text "+ New Order" ]

    table =
      HH.div
        [ HP.classes
            [ Css.tw.table
            , Css.tw.wFull
            , Css.tw.bgWhite
            , Css.tw.shadowSm
            , Css.tw.roundedMd
            , Css.tw.overflowHidden
            ]
        ]

    thead =
      HH.div
        [ HP.classes
            [ Css.tw.tableHeaderGroup
            , Css.tw.bgGray200
            , Css.tw.uppercase
            , Css.tw.textLeft
            , Css.tw.textSm
            , Css.tw.textGray600
            , Css.tw.borderB
            ]
        ]

    tbody = HH.div [ HP.classes [ Css.tw.tableRowGroup ] ]

    trow = HH.div [ HP.classes [ Css.tw.tableRow ] ]

    tcell = HH.div [ HP.classes [ Css.tw.tableCell, Css.tw.px5, Css.tw.py3 ] ]

  renderContent = defRender state renderOrders

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
