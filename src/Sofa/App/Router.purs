-- | Provides the main Sofa page component, including navigation bars and
-- | routing.
module Sofa.App.Router
  ( Query(..)
  , component
  , startRouting
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Routing.Hash (matches)
import Sofa.App.Home as Home
import Sofa.App.NavbarItemUser as NavbarItemUser
import Sofa.App.NavbarTile as NavbarTile
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
import Sofa.Data.Deployment (class MonadDeployment, Deployment(..), getDeployment)
import Sofa.Data.Route (Route)
import Sofa.Data.Route as Route
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (addClassToElement, removeClassToElement)
import Effect.Console as Console
import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.MouseEvent (MouseEvent, toEvent) as Event

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

type Input
  = Orders.OrderFilter

type State
  = { route :: Route
    , homeOrderFilter :: Orders.OrderFilter
    , showLogin :: Boolean
    }

type Slots
  = ( home :: Home.Slot Unit
    , productCatalog :: ProductCatalog.Slot Unit
    , orderForm :: OrderForm.Slot Unit
    , orderFormCrmAccountId :: OrderForm.Slot Unit
    , ordersCrmAccountId :: Orders.Slot Unit
    , orders :: Orders.Slot Unit
    , navbarItemUser :: NavbarItemUser.Slot Unit
    , navbarTile :: NavbarTile.Slot Unit
    , nectaryAlerts :: Alerts.Slot Unit
    )

data Query a
  = GotoRoute Route a

data Action
  = DoAlert AlertType
  | Initialize
  | Select String String Event.MouseEvent

component ::
  forall input output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  H.Component Query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , initialize = initialize
            }
    }

initialState :: Input -> State
initialState input = 
    { route: Route.Home, homeOrderFilter: input, showLogin: false }

initialize :: Maybe Action
initialize = Just Initialize

render ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ renderNavbar state
    , HH.div
        [ Css.classes
            [ "flex"
            , "flex-no-wrap"
            , "items-stretch"
            , "min-h-screen"
            , "pt-16"
            ]
        ]
        [ 
          -- renderSideMenu state.route
         renderBody state
        ]
    , HH.slot_ Alerts.proxy unit Alerts.component absurd
    ]

renderSideMenu ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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
        ]
    , HH.div
        [ Css.classes [ "pt-10", "text-stormy-300" ] ]
        [ HH.text "  ⃰ TODO" ]
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
  forall f m.
  MonadAff m =>
  MonadAlert m =>
  CredentialStore f m =>
  MonadDeployment m =>
  State ->
  H.ComponentHTML Action Slots m
renderNavbar state =
  HH.nav [ Css.classes navbarClasses ]
    [ HH.div [ Css.classes navbarWrapperClasses ]
        [ primaryItem Route.Home
        , expander
        , navBarButtons
        , tileMenu
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
    , "justify-center"
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

  tileMenu = HH.slot_ NavbarTile.proxy unit NavbarTile.component absurd

  navbarSubItemUser = do
    case state.showLogin of
      true -> HH.slot_ NavbarItemUser.proxy unit NavbarItemUser.component absurd
      false -> HH.text ""
    

  expander = HH.div [ Css.class_ "grow" ] []

  navBarButtons = do
    case state.homeOrderFilter of
      filter -> do 
          case filter of
              Orders.ListAllAccessibleOrder -> renderStandardNavigation
              Orders.ListCustomerOrders { crmAccountId: id } -> 
                HH.ul [ Css.classes [ "flex", "h-full", "justify-self-end" ] ] 
                [HH.li [Css.class_ "pr-8 py-4 pl-8 hover:bg-gray-100 font-semibold sofa-navbar-border sofa-navbar-selected", HE.onClick (Select "order-lists" "new-form") , HP.id "order-lists"]
                  [
                    HH.a 
                      [ Route.href (Route.OrdersCrmAccountId id)
                      , Css.class_ "flex space-x-24"
                      
                      ]
                      [ Icon.viewList
                          [ Icon.classes
                              [ Css.c "h-8"
                              , Css.c "mr-2"
                              ]
                          , Icon.ariaHidden true
                          ]
                      , HH.text "Order Lists"
                      ]
                  ]

                , HH.li [Css.class_ "pr-8 py-4 pl-8 hover:bg-gray-100 font-semibold sofa-navbar-border", HE.onClick (Select "new-form" "order-lists"), HP.id "new-form"]
                    [
                    HH.a 
                      [ Route.href (Route.OrderFormCrmAccountId id)
                      , Css.class_ "flex space-x-24"
                      
                      ]
                      [ Icon.createFolder
                          [ Icon.classes
                              [ Css.c "h-8"
                              , Css.c "mr-2"
                              ]
                          , Icon.ariaHidden true
                          ]
                      , HH.text "New Order Form"
                      ]
                  ] 
                ]
              _ -> renderStandardNavigation
    

renderStandardNavigation ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  H.ComponentHTML Action Slots m
renderStandardNavigation = 
  HH.ul [ Css.classes [ "flex", "h-full", "justify-self-end" ] ] 
  [HH.li [Css.class_ "pr-8 py-4 pl-8 hover:bg-gray-100 font-semibold sofa-navbar-border sofa-navbar-selected", HE.onClick (Select "order-lists" "new-form") , HP.id "order-lists"]
    [
      HH.a 
        [ Route.href (Route.Orders)
        , Css.class_ "flex space-x-24"
        ]
        [ Icon.viewList
            [ Icon.classes
                [ Css.c "h-8"
                , Css.c "mr-2"
                ]
            , Icon.ariaHidden true
            ]
        , HH.text "Order Lists"
        ]
    ]

  , HH.li [Css.class_ "pr-8 py-4 pl-8 hover:bg-gray-100 sofa-navbar-border font-semibold", HE.onClick (Select "new-form" "order-lists") , HP.id "new-form"]
      [
      HH.a 
        [ Route.href (Route.OrderForm)
        , Css.class_ "flex space-x-24"
        ]
        [ Icon.createFolder
            [ Icon.classes
                [ Css.c "h-8"
                , Css.c "mr-2"
                ]
            , Icon.ariaHidden true
            ]
        , HH.text "New Order Form"
        ]
    ] 
  ]

renderBody ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  MonadDeployment m =>
  State ->
  H.ComponentHTML Action Slots m
renderBody state =
  HH.main [ Css.classes [ "w-11/12", "mx-5" ] ]
    [ case state.route of
        Route.Home -> slotHomeOrder
        Route.OrderForm -> slotOrderForm
        (Route.OrderFormCrmAccountId crmAccountId) -> slotOrderFormCrmAccountId crmAccountId
        Route.Orders -> slotOrders
        (Route.OrdersCrmAccountId crmAccountId) -> slotOrdersCrmAccountId {crmAccountId}
        (Route.Order id) -> slotOrder id
        Route.ProductCatalog -> slotProductCatalog
    ]
  where
  slotHomeOrder = HH.slot_ Orders.proxy unit Orders.component input
    where
     input = case state.homeOrderFilter of
              filter -> do 
                case filter of
                    Orders.ListAllAccessibleOrder -> Orders.ListAllAccessibleOrder
                    Orders.ListCustomerOrders { crmAccountId: id } -> Orders.ListCustomerOrders { crmAccountId: id }
              _ -> Orders.ListAllAccessibleOrder

  slotOrderForm = HH.slot_ OrderForm.proxy unit OrderForm.component input
    where
    input = OrderForm.NewOrder

  slotOrders = HH.slot_ Orders.proxy unit Orders.component input
    where
    input = Orders.ListAllAccessibleOrder

  slotOrdersCrmAccountId crmAccountId = HH.slot_ Orders.proxy unit Orders.component input
    where
    input = Orders.ListCustomerOrders crmAccountId

  slotOrderFormCrmAccountId crmAccountId = HH.slot_ OrderForm.proxy unit OrderForm.component input
    where
    input = OrderForm.NewOrderCrmAccountId crmAccountId

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
  MonadAff m =>
  MonadDeployment m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  DoAlert typ -> do
    H.lift
      $ Alerts.push
      $ Alert.defaultAlert
          { type_ = typ
          , content = HH.span_ [ HH.text "Got an alert of type ", HH.text (show typ) ]
          }
  Initialize -> do
    deployment <- H.lift getDeployment
    case deployment of
      Standard ->
        H.modify_ \st ->
          st
            { showLogin = true
            }
      Salesforce _ ->
        H.modify_ \st ->
          st
            { showLogin = false
            }
  Select select unselect event -> do
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    H.liftEffect $ addClassToElement select "sofa-navbar-selected"
    H.liftEffect $ removeClassToElement unselect "sofa-navbar-selected"
    
