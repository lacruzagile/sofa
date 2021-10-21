module App.Orders (Slot, proxy, component) where

import Prelude
import App.OrderForm as OrderForm
import App.Requests (getOrders)
import Css as Css
import Data.Loadable (Loadable(..))
import Data.Maybe (Maybe(..))
import Data.SmartSpec as SS
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "orders"
proxy = Proxy

type Slots
  = ( orderForm :: OrderForm.Slot Unit
    )

type State
  = Loadable
      { orders :: SS.Orders
      , selected :: Maybe SS.OrderForm
      }

data Action
  = NoOp
  | ClearState
  | LoadOrders
  | OpenOrder SS.OrderForm
  | CloseOrder

component ::
  forall query input output m.
  MonadAff m => H.Component query input output m
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

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
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
  renderOrder orderForm@(SS.OrderForm o) =
    HH.tr_
      [ HH.td_ [ HH.text $ showID o.id ]
      , HH.td_ [ HH.text $ show o.status ]
      , HH.td_ [ HH.text purchaser ]
      , HH.td_ [ HH.text seller ]
      , HH.td_ [ HH.button [ HE.onClick \_ -> OpenOrder orderForm ] [ HH.text "Open" ] ]
      ]
    where
    Tuple purchaser seller = case o.customer of
      SS.NewCustomer
        { purchaser: SS.Purchaser { corporateName: p }
      , seller: SS.Seller { name: s }
      } -> Tuple p s
      _ -> Tuple "?" "?"

  renderOrders :: { orders :: SS.Orders, selected :: Maybe SS.OrderForm } -> Array (H.ComponentHTML Action Slots m)
  renderOrders { orders: SS.Orders os, selected } = case selected of
    Just orderForm ->
      [ HH.button [ HE.onClick \_ -> CloseOrder ] [ HH.text "← Back" ]
      , HH.slot_ OrderForm.proxy unit OrderForm.component (Just orderForm)
      ]
    Nothing ->
      [ HH.h1_ [ HH.text "Orders" ]
      , HH.table_
          $ [ HH.tr_
                [ HH.th_ [ HH.text "ID" ]
                , HH.th_ [ HH.text "Status" ]
                , HH.th_ [ HH.text "Purchaser" ]
                , HH.th_ [ HH.text "Seller" ]
                , HH.th_ [ HH.text "Action" ]
                ]
            ]
          <> map renderOrder os.items
      ]

  renderContent = defRender state renderOrders

showID :: String -> String
showID id =
  let
    len = S.length id
  in
    if len > 10 then
      S.take 4 id <> "…" <> S.drop (len - 4) id
    else
      id

loadOrders ::
  forall slots output m.
  MonadAff m =>
  H.HalogenM State Action slots output m Unit
loadOrders = do
  H.modify_ \_ -> Loading
  orders <- H.liftAff getOrders
  H.modify_ \_ -> (\os -> { orders: os, selected: Nothing }) <$> orders

handleAction ::
  forall slots output m.
  MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ClearState -> H.put Idle
  LoadOrders -> loadOrders
  OpenOrder orderForm -> H.modify_ $ map \st -> st { selected = Just orderForm }
  CloseOrder -> H.modify_ $ map \st -> st { selected = Nothing }
