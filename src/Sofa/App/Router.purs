module Sofa.App.Router where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Routing.Hash (matches)
import Sofa.App.Home as Home
import Sofa.App.NavbarItemUser as NavbarItemUser
import Sofa.App.OrderForm as OrderForm
import Sofa.App.Orders as Orders
import Sofa.App.ProductCatalog as ProductCatalog
import Sofa.Component.Alert (AlertType)
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
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
    , nectaryAlerts :: Alerts.Slot Unit
    )

data Query a
  = GotoRoute Route a

data Action
  = DoAlert AlertType

component ::
  forall input output m.
  MonadAff m =>
  CredentialStore m =>
  MonadAlert m =>
  H.Component Query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            }
    }

initialState :: forall input. input -> State
initialState _ = { route: Route.Home }

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  MonadAlert m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ renderNavbar
    , HH.div
        [ Css.classes
            [ "flex"
            , "flex-no-wrap"
            , "items-stretch"
            , "min-h-screen"
            , "pt-16"
            ]
        ]
        [ renderSideMenu state.route
        , renderBody state
        ]
    , HH.slot_ Alerts.proxy unit Alerts.component absurd
    ]

renderSideMenu ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Route ->
  H.ComponentHTML Action Slots m
renderSideMenu currentRoute =
  HH.nav [ Css.classes navbarClasses ]
    [ HH.ul [ Css.class_ "space-y-4" ]
        [ navbarItem Icon.package "Solutions  ⃰"
            [ navbarSubItem Route.ProductCatalog "Product catalog"
            ]
        , navbarItem Icon.longMessage "Order forms"
            [ navbarSubItem Route.Orders "Orders"
            , navbarSubItem Route.OrderForm "Order form"
            ]
        , navbarItem Icon.piggybank "Billing  ⃰" []
        , navbarItem Icon.puzzle "Assets  ⃰" []
        , navbarItem Icon.settings "Project settings  ⃰" []
        , navbarItem Icon.package "Alert test  ⃰  ⃰"
            $ let
                btn typ =
                  HH.li
                    [ Css.classes [ "w-full", "pl-4", "py-2" ]
                    ]
                    [ HH.button
                        [ Css.classes [ "nectary-btn-secondary", "h-6", "text-sm" ]
                        , HE.onClick \_ -> DoAlert typ
                        ]
                        [ HH.text (show typ) ]
                    ]
              in
                [ btn Alert.Informative
                , btn Alert.Success
                , btn Alert.Warning
                , btn Alert.Error
                ]
        , HH.div
            [ Css.classes [ "pt-10", "text-stormy-300" ] ]
            [ HH.text "  ⃰ TODO" ]
        , HH.div
            [ Css.classes [ "text-stormy-300" ] ]
            [ HH.text "  ⃰  ⃰ Ignore this" ]
        ]
    ]
  where
  navbarClasses =
    [ "w-64"
    , "pt-2"
    , "pl-4"
    , "flex-none"
    , "bg-snow-100"
    ]

  -- Whether the given route is conceptually the same route as the current
  -- route.
  isCurrentRoute route = case currentRoute of
    Route.Order _ -> route == Route.OrderForm
    _ -> route == currentRoute

  navbarItem icon text children =
    HH.li_
      [ HH.div
          [ Css.classes
              [ "flex"
              , "items-center"
              , "font-semibold"
              ]
          ]
          [ icon
              [ Icon.classes
                  [ Css.c "h-6"
                  , Css.c "mr-4"
                  ]
              , Icon.ariaHidden true
              ]
          , HH.div_ [ HH.text text ]
          ]
      , case children of
          [] -> HH.text ""
          _ -> HH.ul [ Css.classes [ "pt-4", "ml-6" ] ] children
      ]

  navbarSubItemClasses route =
    [ "inline-flex"
    , "items-center"
    , "w-full"
    , "pl-4"
    , "py-2"
    , "ring-0"
    , "ring-tropical-700"
    , "focus:ring-1"
    , "outline-none"
    ]
      <> if isCurrentRoute route then
          [ "border-l-2"
          , "border-tropical-500"
          , "text-tropical-500"
          , "font-semibold"
          ]
        else
          [ "border-l"
          , "border-stormy-300"
          , "outline-none"
          , "text-stormy-300"
          , "hover:text-tropical-500"
          ]

  navbarSubItem route text =
    HH.li_
      [ HH.a
          [ Route.href route, Css.classes (navbarSubItemClasses route) ]
          [ HH.text text ]
      ]

renderNavbar ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  H.ComponentHTML Action Slots m
renderNavbar =
  HH.nav [ Css.classes navbarClasses ]
    [ HH.div [ Css.classes navbarWrapperClasses ]
        [ primaryItem Route.Home
        , expander
        , navbarSubItemUser
        ]
    ]
  where
  navbarClasses =
    [ "absolute"
    , "inset-x-0"
    , "top-0"
    , "h-16"
    , "bg-white"
    ]

  navbarWrapperClasses =
    [ "h-full"
    , "flex"
    , "justify-between"
    , "items-center"
    ]

  primaryItem route =
    HH.a
      [ Route.href route
      , Css.classes [ "flex", "w-64", "h-full" ]
      ]
      [ Icon.sinchLogo
          [ Icon.classes [ Css.c "ml-6", Css.c "my-auto", Css.c "h-6" ]
          , Icon.ariaLabel "SOFA Home"
          ]
      ]

  navbarSubItemUser = HH.slot_ NavbarItemUser.proxy unit NavbarItemUser.component absurd

  expander = HH.div [ Css.class_ "grow" ] []

renderBody ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  MonadAlert m =>
  State ->
  H.ComponentHTML Action Slots m
renderBody state =
  HH.main [ Css.classes [ "w-11/12", "mx-5" ] ]
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

handleAction ::
  forall slots output m.
  MonadAlert m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  DoAlert typ -> do
    H.lift
      $ Alerts.push
      $ Alert.defaultAlert
          { type_ = typ
          , content = HH.span_ [ HH.text "Got an alert of type ", HH.text (show typ) ]
          }
