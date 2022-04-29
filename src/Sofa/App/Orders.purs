-- | Provides an order list component. Fetches orders from the Ordering backend.
module Sofa.App.Orders (Slot, proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm as OrderForm
import Sofa.App.OrderForm.SelectOrderStatus (statusColorClass)
import Sofa.App.Requests (getOrders)
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Route as Route
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (scrollToBottom)
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "orders"
proxy = Proxy

type Slots
  = ( orderForm :: OrderForm.Slot Unit
    )

type State
  = { orders :: Array SS.OrderForm -- ^ All loaded orders.
    , nextPageToken :: Loadable (Maybe String)
    }

data Action
  = NoOp
  | LoadNext (Maybe String)

component ::
  forall query input output m.
  MonadAff m =>
  CredentialStore m =>
  MonadAlert m =>
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
initialState _ =
  { orders: []
  , nextPageToken: Idle
  }

initialize :: Maybe Action
initialize = Just (LoadNext Nothing)

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ renderContent ]
  where
  renderOrder :: SS.OrderForm -> H.ComponentHTML Action Slots m
  renderOrder (SS.OrderForm o) =
    trow
      [ tcell [ maybe (HH.text "N/A") Widgets.dateWithTimeTooltip o.createTime ]
      , tcell [ HH.text buyer ]
      , tcell [ HH.text seller ]
      , tcell [ HH.text $ fromMaybe "" $ o.displayName ]
      , tcell
          [ HH.div
              [ HP.classes
                  [ Css.c "nectary-tag"
                  , Css.c "w-fit"
                  , statusColorClass o.status
                  ]
              ]
              [ HH.text $ SS.prettyOrderStatus o.status ]
          ]
      ]
    where
    rowClasses = [ "table-row", "hover:bg-gray-100", "odd:bg-snow-200" ]

    trow = case o.id of
      Nothing -> HH.div [ Css.classes rowClasses ]
      Just id -> HH.a [ Route.href (Route.Order id), Css.classes rowClasses ]

    tcell =
      HH.div
        [ Css.classes [ "table-cell", "p-5", "border-b", "border-snow-600" ]
        ]

    Tuple buyer seller =
      let
        SS.Buyer { corporateName: b } = o.buyer

        SS.Seller { registeredName: s } = o.seller
      in
        Tuple b s

  renderOrders :: H.ComponentHTML Action Slots m
  renderOrders =
    HH.div_
      [ table
          [ thead
              [ trow
                  [ thcell [ HH.text "Date" ]
                  , thcell [ HH.text "Customer" ]
                  , thcell [ HH.text "Legal entity" ]
                  , thcell [ HH.text "Name" ]
                  , thcell [ HH.text "Status" ]
                  ]
              ]
          , tbody $ map renderOrder state.orders
          ]
      , renderLoadMore
      ]
    where
    table =
      HH.div
        [ Css.classes
            [ "table"
            , "w-full"
            , "bg-white"
            , "rounded-md"
            ]
        ]

    thead =
      HH.div
        [ Css.classes
            [ "table-header-group"
            , "font-semibold"
            , "text-left"
            , "text-stormy-200"
            ]
        ]

    tbody = HH.div [ Css.class_ "table-row-group" ]

    trow = HH.div [ Css.class_ "table-row" ]

    thcell =
      HH.div
        [ Css.classes
            [ "table-cell"
            , "px-5"
            , "py-3"
            , "border-b"
            , "border-stormy-200"
            ]
        ]

  renderLoadMore :: H.ComponentHTML Action Slots m
  renderLoadMore =
    let
      btnClasses = [ "nectary-btn-secondary", "ring-0", "w-full", "mb-3", "rounded-t-none" ]
    in
      case state.nextPageToken of
        Loading ->
          HH.button
            [ Css.classes btnClasses, HP.disabled true ]
            [ Widgets.spinner [] ]
        Loaded mTok ->
          HH.button
            [ Css.classes btnClasses, HE.onClick \_ -> LoadNext mTok ]
            [ HH.text $ maybe "Load orders" (const "Load more") mTok ]
        _ -> HH.text ""

  renderSearchForm =
    HH.form [ Css.class_ "relative" ]
      [ Icon.search
          [ Icon.classes
              [ Css.c "absolute"
              , Css.c "inset-y-0"
              , Css.c "left-2.5"
              , Css.c "w-6"
              , Css.c "h-full"
              , Css.c "fill-stormy-300"
              ]
          , Icon.ariaHidden true
          ]
      , HH.input
          [ HP.type_ HP.InputSearch
          , Css.classes
              [ "nectary-input"
              , "w-96"
              , "pl-10"
              ]
          , HP.placeholder "Search (not yet functional)"
          ]
      ]

  renderNewOrderLink =
    HH.a
      [ Route.href Route.OrderForm
      , Css.class_ "nectary-btn-primary"
      ]
      [ HH.text "New order" ]

  renderContent =
    [ HH.div
        [ Css.classes
            [ "my-5"
            , "flex"
            , "flex-wrap"
            , "items-center"
            , "gap-4"
            ]
        ]
        [ HH.h1 [ Css.classes [ "grow", "my-0" ] ] [ HH.text "Orders" ]
        , renderSearchForm
        , renderNewOrderLink
        ]
    , renderOrders
    ]

handleAction ::
  forall slots output m.
  MonadAff m =>
  CredentialStore m =>
  MonadAlert m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  NoOp -> pure unit
  LoadNext nextPageToken -> do
    H.modify_ \st -> st { nextPageToken = Loading }
    result <- H.lift $ getOrders nextPageToken
    case result of
      Loaded { orders, nextPageToken: nextPageToken' } -> do
        H.modify_ \st ->
          st
            { orders = st.orders <> orders
            , nextPageToken = Loaded nextPageToken'
            }
        H.liftEffect scrollToBottom
      Error msg -> do
        H.modify_ _ { nextPageToken = Loaded nextPageToken }
        H.lift
          $ Alerts.push
          $ Alert.errorAlert "Failed to load orders" msg
      _ -> do
        H.modify_ _ { nextPageToken = Loaded nextPageToken }
