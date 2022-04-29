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
import Sofa.Component.Icon as Icon
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore, Credentials(..), credentialsAreReadOnly, getCredentials, login, logout)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event

type Slot id
  = forall query output. H.Slot query output id

proxy :: Proxy "navbarItemUser"
proxy = Proxy

data State
  = LoggedOut
  | LoggingIn
    { user :: Maybe String
    , pass :: Maybe String
    , error :: Maybe String
    }
  | LoggedIn
    { readOnly :: Boolean
    , user :: String
    }

data Action
  = LoadCredentials -- ^ Load credentials from the credential store.
  | SetState State
  | Login Event.Event
  | Logout

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
initialState = const LoggedOut

initialize :: Maybe Action
initialize = Just LoadCredentials

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
      [ HE.onClick \_ -> SetState $ LoggingIn mempty ]
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
        [ HH.input
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
        , case st.error of
            Nothing -> HH.text ""
            Just msg -> HH.div [ Css.classes errorClasses ] [ HH.text msg ]
        , HH.div [ Css.classes [ "flex", "gap-x-4" ] ]
            [ HH.div [ Css.class_ "grow" ] []
            , HH.button
                [ Css.class_ "nectary-btn-secondary"
                , HP.type_ HP.ButtonButton
                , HE.onClick \_ -> SetState LoggedOut
                ]
                [ HH.text "Cancel" ]
            , HH.button
                [ HP.type_ HP.ButtonSubmit
                , Css.class_ "nectary-btn-primary"
                ]
                [ HH.text "Login" ]
            ]
        ]

handleAction ::
  forall output m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  LoadCredentials -> do
    readOnly <- H.lift credentialsAreReadOnly
    credentials <- H.lift getCredentials
    let
      st =
        maybe
          LoggedOut
          (\(Credentials { user }) -> LoggedIn { readOnly, user })
          credentials
    H.put st
  SetState st -> H.put st
  Login event -> do
    H.liftEffect $ Event.preventDefault event
    st <- H.get
    case st of
      LoggedOut -> H.put LoggedOut
      LoggingIn s@{ user: Just user, pass: Just pass } -> do
        result <- H.lift $ login user pass
        case result of
          Left msg -> H.put $ LoggingIn $ s { error = Just msg }
          Right _ -> H.put $ LoggedIn { readOnly: false, user }
      LoggingIn s -> H.put $ LoggingIn $ s { error = Just "Need to enter username and password" }
      LoggedIn _ -> H.put LoggedOut
  Logout -> do
    H.lift logout
    H.put LoggedOut
