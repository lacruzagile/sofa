-- | The user navigation bar item. When the user is not logged in then this
-- | shows a login button, which when clicked redirects the browser to the SSO
-- | login page. When logged in, the component shows a logout button.
module Sofa.App.NavbarTile (Slot, proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore, AuthEvent(..), credentialsAreReadOnly, getAuthInstance, getCredentials, getUser, logout, mkSsoAuthorizeUrl, toEmitter)
import Sofa.Data.Auth as Auth
import Sofa.Data.Deployment (detectDeployment, Deployment(..))
import Type.Proxy (Proxy(..))
import Web.HTML as Html
import Web.HTML.Location as HtmlLocation
import Web.HTML.Window as HtmlWindow

type Slot id
  = forall query output. H.Slot query output id

proxy :: Proxy "navbarTile"
proxy = Proxy

type State
  = { enable :: Boolean
    , menuOpen :: Boolean
    }

data Action
  = Initialize
  | SetMenuVisibility Boolean

component ::
  forall query input output f m.
  MonadAff m =>
  MonadAlert m =>
  CredentialStore f m =>
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
initialState =
  const
    { enable: false
    , menuOpen: false
    }

initialize :: Maybe Action
initialize = Just Initialize

render :: forall slots m. State -> H.ComponentHTML Action slots m
render = renderLoggedIn
  where
  renderUser text =
    HH.div
      [ Css.classes
          [ "flex"
          , "flex-col"
          , "place-content-center"
          , "items-center"
          , "h-full"
          , "px-5"
          ]
      ]
      [ Icon.user
          [ Icon.classes [ Css.c "w-8", Css.c "h-8" ]
          , Icon.ariaHidden true
          ]
      , HH.span [ Css.class_ "text-sm" ] [ HH.text text ]
      ]

  renderLoggedIn { enable, menuOpen }
    | not enable = HH.text ""
    | otherwise =
      HH.div
        [ Css.classes [ "relative", "mx-2" ]
        , HE.onMouseLeave \_ -> SetMenuVisibility false
        ]
        [ HH.div
            [ HE.onMouseEnter \_ -> SetMenuVisibility true ]
            [ Icon.tiles
                [ Icon.classes [ Css.c "w-5", Css.c "h-8" ]
                , Icon.ariaLabel "Menu"
                ]
            ]
        , renderUserMenu menuOpen
        ]

  renderUserMenu open
    | not open = HH.text ""
    | otherwise =
      HH.div
        [ Css.classes
            [ "absolute"
            , "w-40"
            , "right-0"
            , "flex"
            , "flex-col"
            , "bg-white"
            , "overflow-auto"
            , "border"
            , "rounded-sm"
            , "divide-y"
            , "z-10"
            , "shadow-md"
            ]
        ]
        [ HH.a
            [ Css.classes [ "p-3", "hover:bg-snow-500", "text-left" ]
            , HP.href "/admin-portal/home"
            ]
            [ HH.text "Go to dashboard" ]
        ]

handleAction ::
  forall output m.
  MonadAff m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    deployment <- H.liftEffect detectDeployment
    case deployment of
      Standard ->
        H.modify_ \st ->
          st
            { enable = true
            }
      Salesforce _ ->
        H.modify_ \st ->
          st
            { enable = false
            }
  SetMenuVisibility v ->
    H.modify_ \st ->
      st
        { menuOpen = v
        }