module App.Orders (Slot, proxy, component) where

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
import Data.String as S
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

proxy :: Proxy "orders"
proxy = Proxy

type State
  = Loadable SS.Orders

data Action
  = NoOp
  | ClearState
  | LoadOrders String

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
initialize = Just $ LoadOrders "v1alpha1/examples/orders.json"

render :: forall slots m. MonadAff m => State -> H.ComponentHTML Action slots m
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
    (a -> Array (H.ComponentHTML Action slots m)) ->
    Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> idle
    Loading -> loading
    Loaded dat -> rend dat
    Error err -> error err

  renderOrder :: SS.OrderForm -> H.ComponentHTML Action slots m
  renderOrder (SS.OrderForm o) =
    HH.tr_
      [ HH.td_ [ HH.text $ showID o.id ]
      , HH.td_ [ HH.text $ show o.status ]
      , HH.td_ [ HH.text purchaser ]
      , HH.td_ [ HH.text seller ]
      ]
    where
    Tuple purchaser seller = case o.customer of
      SS.NewCustomer
        { purchaser: SS.Purchaser { corporateName: p }
      , seller: SS.Seller { legalEntity: SS.LegalEntity { name: s } }
      } -> Tuple p s
      _ -> Tuple "?" "?"

  renderOrders :: SS.Orders -> Array (H.ComponentHTML Action slots m)
  renderOrders (SS.Orders os) =
    [ HH.h1_ [ HH.text "Orders" ]
    , HH.table_
        $ [ HH.tr_
              [ HH.th_ [ HH.text "ID" ]
              , HH.th_ [ HH.text "Status" ]
              , HH.th_ [ HH.text "Purchaser" ]
              , HH.th_ [ HH.text "Seller" ]
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

showMonetary :: Estimate (Additive Number) -> String
showMonetary = showEst <<< map (\(Additive n) -> toStringWith (fixed 2) n)
  where
  showEst = case _ of
    Exact s -> s
    Estimate s -> "~" <> s

loadOrders ::
  forall slots output m.
  MonadAff m =>
  String ->
  H.HalogenM State Action slots output m Unit
loadOrders url = do
  H.modify_ \_ -> Loading
  orders <- H.liftAff $ getJson url
  H.modify_ \_ -> orders

handleAction ::
  forall slots output m.
  MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ClearState -> H.put Idle
  LoadOrders url -> loadOrders url
