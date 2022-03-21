module Sofa.App.Router where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Hash (matches)
import Sofa.App.Home as Home
import Sofa.App.NavbarItemUser as NavbarItemUser
import Sofa.App.OrderForm as OrderForm
import Sofa.App.Orders as Orders
import Sofa.App.ProductCatalog as ProductCatalog
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Route (Route)
import Sofa.Data.Route as Route

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
  HH.div_
    [ renderNavbar
    , HH.div
        [ HP.classes
            [ Css.c "flex"
            , Css.c "flex-no-wrap"
            , Css.c "items-stretch"
            , Css.c "min-h-screen"
            , Css.c "pt-16"
            ]
        ]
        [ renderSideMenu state.route
        , renderBody state
        ]
    ]

renderSideMenu ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Route ->
  H.ComponentHTML Action Slots m
renderSideMenu currentRoute =
  HH.nav [ HP.classes navbarClasses ]
    [ HH.ul [ HP.classes [ Css.c "space-y-4" ] ]
        [ navbarItem Icon.package "Solutions  ⃰" []
        , navbarItem Icon.longMessage "Order forms"
            [ navbarSubItem Route.Home "Home"
            , navbarSubItem Route.ProductCatalog "Product catalog"
            , navbarSubItem Route.Orders "Orders"
            , navbarSubItem Route.OrderForm "Order form"
            ]
        , navbarItem Icon.piggybank "Billing  ⃰" []
        , navbarItem Icon.puzzle "Assets  ⃰" []
        , navbarItem Icon.settings "Project settings  ⃰" []
        , HH.div
            [ HP.classes [ Css.c "ml-5", Css.c "pt-10", Css.c "text-stormy-300" ] ]
            [ HH.text " ⃰ TODO" ]
        ]
    ]
  where
  navbarClasses =
    [ Css.c "w-64"
    , Css.c "pt-5"
    , Css.c "pl-3"
    , Css.c "flex-none"
    , Css.c "bg-snow-100"
    , Css.c "border-r"
    , Css.c "border-snow-600"
    ]

  -- Whether the given route is conceptually the same route as the current
  -- route.
  isCurrentRoute route = case currentRoute of
    Route.Order _ -> route == Route.OrderForm
    _ -> route == currentRoute

  navbarItem icon text children =
    HH.li_
      [ HH.div
          [ HP.classes
              ( [ Css.c "flex"
                , Css.c "items-center"
                , Css.c "font-semibold"
                ]
                  <> if A.null children then [] else [ Css.c "nectary-dropdown-icon" ]
              )
          ]
          [ icon
              [ Icon.classes
                  [ Css.c "h-10"
                  , Css.c "mr-2"
                  , Css.c "bg-snow-500"
                  , Css.c "rounded-md"
                  ]
              ]
          , HH.div_ [ HH.text text ]
          ]
      , HH.ul [ HP.classes [] ] children
      ]

  navbarSubItemClasses route
    | isCurrentRoute route =
      [ Css.c "inline-flex"
      , Css.c "items-center"
      , Css.c "w-full"
      , Css.c "pl-12"
      , Css.c "py-2"
      , Css.c "border-r-2"
      , Css.c "border-stormy-500"
      ]
    | otherwise =
      [ Css.c "inline-flex"
      , Css.c "items-center"
      , Css.c "w-full"
      , Css.c "pl-12"
      , Css.c "py-2"
      , Css.c "border-r"
      , Css.c "hover:border-r-2"
      , Css.c "border-stormy-100"
      ]

  navbarSubItem route text =
    HH.li_
      [ HH.a
          [ Route.href route, HP.classes (navbarSubItemClasses route) ]
          [ HH.text text ]
      ]

renderNavbar ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  H.ComponentHTML Action Slots m
renderNavbar =
  HH.nav [ HP.classes navbarClasses ]
    [ HH.div [ HP.classes navbarWrapperClasses ]
        [ primaryItem Route.Home
        , expander
        , navbarSubItemUser
        ]
    ]
  where
  navbarClasses =
    [ Css.c "absolute"
    , Css.c "inset-x-0"
    , Css.c "top-0"
    , Css.c "h-16"
    , Css.c "shadow-md"
    , Css.c "bg-white"
    ]

  navbarWrapperClasses =
    [ Css.c "h-full"
    , Css.c "px-5"
    , Css.c "flex"
    , Css.c "justify-between"
    , Css.c "items-center"
    ]

  primaryItem route =
    HH.a
      [ Route.href route, HP.class_ (Css.c "mr-5") ]
      [ Icon.sinchLogo [ Icon.classes [ Css.c "h-8" ] ] ]

  navbarSubItemUser = HH.slot_ NavbarItemUser.proxy unit NavbarItemUser.component absurd

  expander = HH.div [ HP.class_ (Css.c "grow") ] []

renderBody ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State ->
  H.ComponentHTML Action Slots m
renderBody state =
  HH.main [ HP.classes [ Css.c "w-11/12", Css.c "mx-5" ] ]
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
