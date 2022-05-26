-- | The user navigation bar item. When the user is not logged in then this
-- | shows a login button, which when clicked redirects the browser to the SSO
-- | login page. When logged in, the component shows a logout button.
module Sofa.App.NavbarItemUser (Slot, proxy, component) where

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
import Type.Proxy (Proxy(..))
import Web.HTML as Html
import Web.HTML.Location as HtmlLocation
import Web.HTML.Window as HtmlWindow

type Slot id
  = forall query output. H.Slot query output id

proxy :: Proxy "navbarItemUser"
proxy = Proxy

data State
  = LoggedOut
  | LoggedIn
    { readOnly :: Boolean
    , user :: String
    }

data Action
  = Initialize
  | Login
  | Logout
  | AuthEv AuthEvent

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
initialState = const LoggedOut

initialize :: Maybe Action
initialize = Just Initialize

render :: forall slots m. State -> H.ComponentHTML Action slots m
render = case _ of
  LoggedOut -> renderLoggedOut
  LoggedIn s -> renderLoggedIn s
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

  renderLoggedOut =
    HH.button
      [ HE.onClick \_ -> Login ]
      [ renderUser "Login" ]

  renderLoggedIn { readOnly, user }
    | readOnly = renderUser user
    | otherwise =
      HH.button
        [ HP.title $ "Logged in as " <> user
        , HE.onClick $ \_ -> Logout
        ]
        [ renderUser "Logout" ]

handleAction ::
  forall output f m.
  MonadAff m =>
  MonadAlert m =>
  CredentialStore f m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize the authentication module.
    H.lift Auth.initialize
    -- Start listening to authentication events.
    authEventEmitter <- toEmitter <$> H.lift getAuthInstance
    _ <- H.subscribe (AuthEv <$> authEventEmitter)
    -- Load credentials from the credential store.
    readOnly <- H.lift credentialsAreReadOnly
    mCredentials <- H.lift getCredentials
    H.put
      $ maybe
          LoggedOut
          (\credentials -> LoggedIn { readOnly, user: getUser credentials })
          mCredentials
  Login ->
    -- Redirect to the SSO authorize page. Once authentication is complete the
    -- SSO server should redirect the browser back into SOFA and we'll pick up
    -- the authentication process again in `Initialize` above.
    H.liftEffect do
      authorizeUrl <- mkSsoAuthorizeUrl
      Html.window
        >>= HtmlWindow.location
        >>= HtmlLocation.assign authorizeUrl
  Logout -> do
    H.lift logout
    H.put LoggedOut
  AuthEv EvLogin -> pure unit
  AuthEv EvLogout -> do
    H.lift
      $ Alerts.push
      $ Alert.defaultAlert
          { content = HH.text "You were logged out."
          }
    st <- H.get
    case st of
      LoggedOut -> pure unit
      _ -> H.put LoggedOut
