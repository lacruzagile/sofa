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
import Sofa.App.Requests (getOrders)
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
    , error :: Maybe String
    , nextPageToken :: Loadable (Maybe String)
    }

data Action
  = NoOp
  | LoadNext (Maybe String)

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
initialState _ =
  { orders: []
  , error: Nothing
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
  renderError err =
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

  renderOrders :: H.ComponentHTML Action Slots m
  renderOrders =
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
          , tbody $ map renderOrder state.orders
          ]
      , renderLoadMore
      ]
    where
    table =
      HH.div
        [ HP.classes
            [ Css.c "table"
            , Css.c "w-full"
            , Css.c "bg-white"
            , Css.c "rounded-md"
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

  renderLoadMore :: H.ComponentHTML Action Slots m
  renderLoadMore = case state.nextPageToken of
    Loading ->
      HH.button
        [ HP.classes [ Css.c "sofa-btn-secondary", Css.c "w-full", Css.c "mb-3" ]
        , HP.disabled true
        ]
        [ Widgets.spinner [] ]
    Loaded (Just tok) ->
      HH.button
        ( [ HP.classes [ Css.c "sofa-btn-secondary", Css.c "w-full", Css.c "mb-3" ]
          , HE.onClick \_ -> LoadNext (Just tok)
          ]
        )
        [ HH.text "Load more" ]
    _ -> HH.text ""

  renderNewOrderLink =
    HH.a
      [ Route.href Route.OrderForm
      , HP.class_ (Css.c "sofa-btn-primary")
      ]
      [ HH.text "New order" ]

  renderContent =
    [ HH.div [ HP.classes [ Css.c "flex", Css.c "items-center" ] ]
        [ HH.h1 [ HP.class_ (Css.c "grow") ] [ HH.text "Orders" ]
        , renderNewOrderLink
        ]
    , renderOrders
    , maybe (HH.text "") renderError state.error
    ]

handleAction ::
  forall slots output m.
  MonadAff m =>
  CredentialStore m =>
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
      _ ->
        H.modify_ \st ->
          st
            { nextPageToken = Loaded nextPageToken
            , error =
              case result of
                Error err -> Just err
                _ -> Nothing
            }
