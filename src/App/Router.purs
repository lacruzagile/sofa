module App.Router where

import Prelude
import App.Button as Button
import App.Home as Home
import App.SolVis as SolVis
import Css as Css
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
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
  = { route :: Maybe Route
    }

type Slots
  = ( home :: Home.Slot Unit
    , button :: Button.Slot Unit
    , solvis :: SolVis.Slot Unit
    )

data Query a
  = GotoRoute Route a

data Action
  = ToggleMenu

component ::
  forall input output m.
  MonadEffect m =>
  MonadAff m =>
  H.Component Query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleQuery = handleQuery --, handleAction = handleAction
            }
    }

initialState :: forall input. input -> State
initialState _ = { route: Nothing }

render ::
  forall m.
  MonadEffect m =>
  MonadAff m =>
  State -> H.ComponentHTML Action Slots m
render state =
  navbar state
    $ case state.route of
        Nothing -> slotHome
        Just Route.Home -> slotHome
        Just Route.Button -> slotButton
        Just Route.SolVis -> slotSolVis
  where
  slotHome = HH.slot_ Home.proxy unit Home.component absurd

  slotButton = HH.slot_ Button.proxy unit Button.component absurd

  slotSolVis = HH.slot_ SolVis.proxy unit SolVis.component absurd

navbar ::
  forall slot. State -> HH.HTML slot Action -> HH.HTML slot Action
navbar state body =
  let
    navbarItemClasses route =
      [ Css.pseudo, Css.button ]
        <> if state.route == Just route then [ Css.active ] else []

    navbarItem route text =
      HH.a
        [ Route.href route, HP.classes (navbarItemClasses route) ]
        [ HH.text text ]
  in
    HH.div_
      [ HH.nav_
          [ HH.a
              [ Route.href Route.Home
              , HP.class_ Css.brand
              ]
              [ HH.text "Sinch Smart Spec" ]
          , HH.input
              [ HP.id "navmenu"
              , HP.type_ HP.InputCheckbox
              , HP.class_ Css.show
              ]
          , HH.label
              [ HP.for "navmenu"
              , HP.classes [ Css.burger, Css.pseudo, Css.button ]
              ]
              [ HH.text "menu" ]
          , HH.div [ HP.class_ Css.menu ]
              [ navbarItem Route.Home "Home"
              , navbarItem Route.Button "Button"
              , navbarItem Route.SolVis "SolVis"
              ]
          ]
      , HH.main [ HP.class_ (H.ClassName "content") ] [ body ]
      ]

handleQuery ::
  forall action slots output m a.
  MonadEffect m =>
  Query a -> H.HalogenM State action slots output m (Maybe a)
handleQuery = case _ of
  GotoRoute route next -> do
    liftEffect $ log $ "Route " <> show route
    H.modify_ _ { route = Just route }
    pure (Just next)

-- handleAction ::
--   forall slots output m.
--   MonadEffect m =>
--   Action -> H.HalogenM State Action slots output m Unit
-- handleAction = case _ of
--   ToggleMenu -> H.modify_ $ \s -> s { menuOpen = not s.menuOpen }
