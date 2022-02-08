module App.Router where

import Prelude
import App.Home as Home
import App.NavbarItemUser as NavbarItemUser
import App.OrderForm as OrderForm
import App.Orders as Orders
import App.ProductCatalog as ProductCatalog
import Css as Css
import Data.Auth (class CredentialStore)
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Hash (matches)

startRouting :: forall output. H.HalogenIO Query output Aff -> Aff Unit
startRouting app =
  void
    $ liftEffect
    $ matches Route.routes \oldRoute newRoute ->
        when (oldRoute /= Just newRoute)
          $ launchAff_
          $ app.query
          $ H.mkTell
          $ GotoRoute newRoute

type State
  = { route :: Route
    }

type Slots
  = ( home :: Home.Slot Unit
    , productCatalog :: ProductCatalog.Slot Unit
    , orderForm :: OrderForm.Slot Unit
    , orders :: Orders.Slot Unit
    , navbarItemUser :: NavbarItemUser.Slot Unit
    )

data Query a
  = GotoRoute Route a

data Action
  = ToggleMenu

component ::
  forall input output m.
  MonadAff m =>
  CredentialStore m =>
  H.Component Query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleQuery = handleQuery
            }
    }

initialState :: forall input. input -> State
initialState _ = { route: Route.Home }

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div [ HP.classes [ Css.tw.mxAuto ], HP.style "max-width: 80rem" ]
    [ renderNavbar state.route
    , renderBody state
    ]

renderNavbar ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Route ->
  H.ComponentHTML Action Slots m
renderNavbar currentRoute =
  HH.nav [ HP.classes navbarClasses ]
    [ primaryItem Route.Home "SOFA"
    , navbarItem Route.Home "Home"
    , navbarItem Route.ProductCatalog "Product Catalog"
    , navbarItem Route.Orders "Orders"
    , navbarItem Route.OrderForm "Order Form"
    , expander
    , navbarItemUser
    ]
  where
  navbarClasses =
    [ Css.tw.mx5
    , Css.tw.px3
    , Css.tw.py3
    , Css.tw.flex
    , Css.tw.justifyBetween
    , Css.tw.shadowSm
    , Css.tw.bgWhite
    , Css.tw.itemsCenter
    ]

  logoClasses = [ Css.tw.text2Xl, Css.tw.smallCaps, Css.tw.mr5 ]

  -- Whether the given route is conceptually the same route as the current
  -- route.
  isCurrentRoute route = case currentRoute of
    Route.Order _ -> route == Route.OrderForm
    _ -> route == currentRoute

  navbarItemClasses route
    | isCurrentRoute route =
      [ Css.tw.px3
      , Css.tw.underline
      , Css.tw.underlineOffset8
      , Css.tw.decoration2
      , Css.tw.decorationSky500
      ]
    | otherwise =
      [ Css.tw.px3
      , Css.tw.hoverUnderline
      , Css.tw.underlineOffset8
      , Css.tw.decoration2
      , Css.tw.decorationSky500_30
      ]

  primaryItem route text =
    HH.a
      [ Route.href route, HP.classes logoClasses ]
      [ HH.text text ]

  navbarItem route text =
    HH.a
      [ Route.href route, HP.classes (navbarItemClasses route) ]
      [ HH.text text ]

  navbarItemUser = HH.slot_ NavbarItemUser.proxy unit NavbarItemUser.component absurd

  expander = HH.div [ HP.class_ Css.tw.grow ] []

renderBody ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State ->
  H.ComponentHTML Action Slots m
renderBody state =
  HH.main [ HP.class_ Css.tw.m5 ]
    [ case state.route of
        Route.Home -> slotHome
        Route.OrderForm -> slotOrderForm
        Route.Orders -> slotOrders
        (Route.Order id) -> slotOrder id
        Route.ProductCatalog -> slotProductCatalog
    ]
  where
  slotHome = HH.slot_ Home.proxy unit Home.component absurd

  slotOrderForm = HH.slot_ OrderForm.proxy unit OrderForm.component input
    where
    input = OrderForm.NewOrder

  slotOrders = HH.slot_ Orders.proxy unit Orders.component absurd

  slotOrder id = HH.slot_ OrderForm.proxy unit OrderForm.component input
    where
    input = OrderForm.ExistingOrderId id

  slotProductCatalog = HH.slot_ ProductCatalog.proxy unit ProductCatalog.component absurd

handleQuery ::
  forall action slots output m a.
  MonadEffect m =>
  Query a -> H.HalogenM State action slots output m (Maybe a)
handleQuery = case _ of
  GotoRoute route next -> do
    H.modify_ _ { route = route }
    pure (Just next)
