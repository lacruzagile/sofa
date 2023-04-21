-- | Provides an order list component. Fetches orders from the Ordering backend.
module Sofa.App.Orders (Slot, Input(..), OrderFilter(..), proxy, component) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.CopyOrderModal as CopyOrder
import Sofa.App.OrderForm as OrderForm
import Sofa.App.Requests (getOrders)
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
import Sofa.Component.Icon as Icon
import Sofa.Component.Spinner as Spinner
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Deployment (class MonadDeployment, isBuyerFixed)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Route as Route
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (scrollToBottom)
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))
import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.MouseEvent (MouseEvent, toEvent) as Event

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "orders"
proxy = Proxy

type Slots
  = ( orderForm :: OrderForm.Slot Unit
    , copyOrder :: CopyOrder.Slot Unit
    )

type Input
  = OrderFilter

data OrderFilter
  = ListAllAccessibleOrder
  | ListCustomerOrders { crmAccountId :: SS.CrmAccountId }

type State
  = { orders :: Array SS.OrderForm -- ^ All loaded orders.
    , nextPageToken :: Loadable (Maybe String)
    , orderFilter :: OrderFilter
    -- TODO: Make use of the order filter field once backend support is available.
    , showNewOrderButton :: Boolean
    }

data Action
  = LoadNext String
  | StopPropagation Event.MouseEvent

firstPageToken :: String
firstPageToken = ""

component ::
  forall query output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  H.Component query Input output m
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
initialState input =
  { orders: []
  , nextPageToken: Idle
  , orderFilter: input
  , showNewOrderButton: true
  }

initialize :: Maybe Action
initialize = Just (LoadNext firstPageToken)

render ::
  forall f m.
  MonadAff m =>
  MonadAlert m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action Slots m
render state = HH.section_ [ HH.article_ renderContent ]
  where
  renderOrder :: SS.OrderForm -> H.ComponentHTML Action Slots m
  renderOrder (SS.OrderForm o) =
    trow
      [ tcell [ maybe (HH.text "N/A") Widgets.dateWithTimeTooltip o.createTime ]
      -- , tcell [ HH.text buyer ]
      -- , tcell [ HH.text seller ]
      , tcell [ HH.text $ fromMaybe "" $ o.displayName ]
      , tcell
          [ HH.div
              [ HP.classes
                  [ Css.c "nectary-tag"
                  , Css.c "w-fit"
                  , Css.statusColorClass o.status
                  ]
              ]
              [ HH.text $ SS.prettyOrderStatus o.status ]
          ]
      -- , tcell 
      --       [
      --           HH.input [Css.classes [ "p-4 hover:bg-tropical-400" ], HE.onClick StopPropagation, HP.type_ HP.InputCheckbox, HP.id oid] 
      --           , HH.label [HP.for oid] [
      --             Icon.moreVert
      --               [ Icon.classes [ Css.c "w-5 hover:bg-tropical-400"]
      --               , Icon.ariaLabel "Action"
      --               ]
      --           ]
      --           , HH.ul [Css.classes [ "submenu" ]] [
      --               HH.li [] 
      --               [HH.a [HP.href "#"] [
      --                     HH.slot_ (Proxy :: Proxy "copyOrder") unit CopyOrder.component { orderName: (fromMaybe "" $ o.displayName), order: (SS.OrderForm o) }
      --                   ]
      --               ]
      --             ]
      --       ]
      
      ]
    where
    rowClasses = [ "table-row", "hover:bg-gray-100", "odd:bg-snow-200" ]

    trow = case o.id of
      Nothing -> HH.div [ Css.classes rowClasses ]
      Just id -> HH.a [ Route.href (Route.Order id), Css.classes rowClasses ]

    oid = case o.id of
      Just id -> show id
      Nothing -> ""

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
                  -- , thcell [ HH.text "Customer" ]
                  -- , thcell [ HH.text "Legal entity" ]
                  , thcell [ HH.text "Name" ]
                  , thcell [ HH.text "Status" ]
                  -- , thcell [ HH.text "Action" ]
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
            [ Spinner.render Spinner.defaults ]
        Loaded (Just tok) ->
          HH.button
            [ Css.classes btnClasses, HE.onClick \_ -> LoadNext tok ]
            [ HH.text
                $ if tok == firstPageToken then
                    "Load orders"
                  else
                    "Load more orders"
            ]
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
        -- Temporary commenting search bar
        -- , renderSearchForm
        ]
    , renderOrders
    ]

handleAction ::
  forall slots output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  LoadNext nextPageToken -> do
    fixedBuyer <- H.lift isBuyerFixed
    state <-
      H.modify \st ->
        st
          { nextPageToken = Loading
          , showNewOrderButton = true
          }
    let
      token
        | nextPageToken == firstPageToken = Nothing
        | otherwise = Just nextPageToken

      crmAccountId = case state.orderFilter of
        ListAllAccessibleOrder -> Nothing
        ListCustomerOrders { crmAccountId: id } -> Just id
    result <- H.lift $ getOrders token crmAccountId
    case result of
      Loaded { orders, nextPageToken: nextPageToken' } -> do
        H.modify_ \st ->
          st
            { orders = st.orders <> orders
            , nextPageToken = Loaded nextPageToken'
            }
        H.liftEffect scrollToBottom
      Error msg -> do
        H.modify_ _ { nextPageToken = Loaded (Just nextPageToken) }
        H.lift
          $ Alerts.push
          $ Alert.errorAlert "Failed to load orders" msg
      _ -> do
        H.modify_ _ { nextPageToken = Loaded (Just nextPageToken) }
  StopPropagation event -> H.liftEffect $ Event.stopPropagation $ Event.toEvent event
