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

  renderOrder :: SS.OrderForm -> H.ComponentHTML Action Slots m
  renderOrder (SS.OrderForm o) =
    HH.tr_
      [ HH.td_ [ maybe (HH.text "N/A") renderOrderId o.id ]
      , HH.td_ [ maybe (HH.text "N/A") renderDateTime o.createTime ]
      , HH.td_ [ HH.text $ SS.prettyOrderStatus o.status ]
      , HH.td_ [ HH.text buyer ]
      , HH.td_ [ HH.text seller ]
      , HH.td_ [ HH.text $ fromMaybe "" $ o.displayName ]
      ]
    where
    renderOrderId id =
      HH.a [ Route.href (Route.Order id) ]
        [ HH.text (SS.abbreviatedOrderId id) ]

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
    [ HH.h1_ [ HH.text "Orders" ]
    , HH.table [ HP.style "width: 100%" ]
        $ [ HH.tr_
              [ HH.th_ [ HH.text "ID" ]
              , HH.th_ [ HH.text "Created" ]
              , HH.th_ [ HH.text "Status" ]
              , HH.th_ [ HH.text "Buyer" ]
              , HH.th_ [ HH.text "Seller" ]
              , HH.th_ [ HH.text "Name" ]
              ]
          ]
        <> map renderOrder os
    ]

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
