-- | The user navigation bar item. When the user is not logged in then this
-- | shows a login button, which when clicked opens a login modal. When logged
-- | in, the component shows a logout button.
module Sofa.App.NavbarItemUser (Slot, proxy, component) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
import Sofa.Component.Icon as Icon
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore, AuthEvent(..), credentialsAreReadOnly, getAuthEventEmitter, getCredentials, getUser, login, logout, mkSsoAuthorizeUrl, toEmitter)
import Sofa.Data.Auth as Auth
import Sofa.Data.Loadable (Loadable(..), isLoading)
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.HTML as Html
import Web.HTML.Location as HtmlLocation
import Web.HTML.Window as HtmlWindow

type Slot id
  = forall query output. H.Slot query output id

proxy :: Proxy "navbarItemUser"
proxy = Proxy

data State
  = LoggedOut
  | LoggingIn
    { user :: Maybe String
    , pass :: Maybe String
    , result :: Loadable Unit
    }
  | LoggedIn
    { readOnly :: Boolean
    , user :: String
    }

data Action
  = Initialize
  | SetState State
  | Login Event.Event
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
  LoggingIn s -> renderLoggingIn s
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
      [ HE.onClick \_ ->
          SetState
            $ LoggingIn
                { user: Nothing
                , pass: Nothing
                , result: Idle
                }
      ]
      [ renderUser "Login" ]

  renderLoggedIn { readOnly, user }
    | readOnly = renderUser user
    | otherwise =
      HH.button
        [ HP.title $ "Logged in as " <> user
        , HE.onClick $ \_ -> Logout
        ]
        [ renderUser "Logout" ]

  renderLoggingIn st =
    Modal.render
      $ Modal.defaultInput
          { title = HH.text "Login"
          , content = renderContent
          }
    where
    textInputClasses =
      [ "nectary-input"
      , "invalid:border-stormy-200"
      , "w-full"
      ]

    errorClasses =
      [ "p-2"
      , "my-2"
      , "bg-red-100"
      , "border"
      , "border-red-400"
      , "text-raspberry-500"
      ]

    renderContent =
      HH.form
        [ Css.classes [ "w-96", "flex", "flex-col", "gap-y-3" ]
        , HE.onSubmit Login
        ]
        [ HH.p_
            [ HH.text "Login using your LDAP credentials. "
            , HH.text "Note, you need to have an Admin Portal account."
            ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "auth-user"
            , HP.required true
            , HP.placeholder "Username"
            , Css.classes textInputClasses
            , HP.value $ fromMaybe "" st.user
            , HE.onValueChange $ \v -> SetState $ LoggingIn $ st { user = Just v }
            ]
        , HH.input
            [ HP.type_ HP.InputPassword
            , HP.id "auth-pass"
            , HP.required true
            , HP.placeholder "Password"
            , Css.classes textInputClasses
            , HP.value $ fromMaybe "" st.pass
            , HE.onValueChange $ \v -> SetState $ LoggingIn $ st { pass = Just v }
            ]
        , case st.result of
            Error msg -> HH.div [ Css.classes errorClasses ] [ HH.text msg ]
            _ -> HH.text ""
        , HH.div [ Css.classes [ "flex", "gap-x-4" ] ]
            [ HH.div [ Css.class_ "grow" ] []
            , HH.button
                [ Css.class_ "nectary-btn-secondary"
                , HP.type_ HP.ButtonButton
                , HP.disabled $ isLoading st.result
                , HE.onClick \_ -> SetState LoggedOut
                ]
                [ HH.text "Cancel" ]
            , HH.button
                [ HP.type_ HP.ButtonSubmit
                , HP.disabled $ isLoading st.result
                , Css.class_ "nectary-btn-primary"
                ]
                [ HH.text "Login"
                , if isLoading st.result then
                    Widgets.spinner [ Css.c "ml-2", Css.c "align-text-bottom" ]
                  else
                    HH.text ""
                ]
            ]
        ]

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
    authEventEmitter <- toEmitter <$> H.lift getAuthEventEmitter
    _ <- H.subscribe (AuthEv <$> authEventEmitter)
    -- Load credentials from the credential store.
    readOnly <- H.lift credentialsAreReadOnly
    credentials <- H.lift getCredentials
    let
      st =
        maybe
          LoggedOut
          (\creds -> LoggedIn { readOnly, user: getUser creds })
          credentials
    H.put st
  SetState (LoggingIn _) ->
    H.liftEffect do
      authorizeUrl <- mkSsoAuthorizeUrl
      Html.window
        >>= HtmlWindow.location
        >>= HtmlLocation.assign authorizeUrl
  SetState st -> H.put st
  Login event -> do
    H.liftEffect $ Event.preventDefault event
    st <- H.get
    case st of
      LoggedOut -> H.put LoggedOut
      LoggingIn s@{ user: Just user, pass: Just pass } -> do
        H.put $ LoggingIn $ s { result = Loading }
        result <- H.lift $ login user pass
        case result of
          Left msg -> H.put $ LoggingIn $ s { result = Error msg }
          Right _ -> H.put $ LoggedIn { readOnly: false, user }
      LoggingIn s ->
        H.put
          $ LoggingIn
          $ s { result = Error "Need to enter username and password" }
      LoggedIn _ -> H.put LoggedOut
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
