module App.User (Slot, proxy, component) where

import Prelude
import Data.Auth (class CredentialStore, Credentials(..), credentialsAreReadOnly, getCredentials, login, logout)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event

type Slot id
  = forall query output. H.Slot query output id

proxy :: Proxy "user"
proxy = Proxy

data State
  = LoggedOut
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
initialState _ = LoggedOut mempty

initialize :: Maybe Action
initialize = Just LoadCredentials

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.section_
    [ HH.article_
        [ HH.h1_ [ HH.text "User" ]
        , case state of
            LoggedOut s -> renderLoggedOut s
            LoggedIn s -> renderLoggedIn s
        ]
    ]
  where
  renderLoggedOut st =
    HH.form [ HE.onSubmit Login ]
      [ HH.label [ HP.for "auth-user" ] [ HH.text "Username" ]
      , HH.input
          [ HP.placeholder "Username"
          , HP.id "auth-user"
          , HP.required true
          , HP.value $ fromMaybe "" st.user
          , HE.onValueChange $ \v -> SetState $ LoggedOut $ st { user = Just v }
          ]
      , HH.label [ HP.for "auth-pass" ] [ HH.text "Password" ]
      , HH.input
          [ HP.placeholder "Password"
          , HP.type_ HP.InputPassword
          , HP.id "auth-pass"
          , HP.required true
          , HP.value $ fromMaybe "" st.pass
          , HE.onValueChange $ \v -> SetState $ LoggedOut $ st { pass = Just v }
          ]
      , HH.div_ $ maybe [] (\m -> [ HH.text m ]) st.error
      , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Login" ]
      ]

  renderLoggedIn { readOnly, user } =
    if readOnly then
      HH.text $ "Logged in as " <> user
    else
      HH.button [ HE.onClick $ \_ -> Logout ]
        [ HH.text "Logout ", HH.text user ]

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
          (LoggedOut mempty)
          (\(Credentials { user }) -> LoggedIn { readOnly, user })
          credentials
    H.put st
  SetState st -> H.put st
  Login event -> do
    H.liftEffect $ Event.preventDefault event
    st <- H.get
    case st of
      LoggedIn _ ->
        H.put $ LoggedOut
          $ { user: Nothing
            , pass: Nothing
            , error: Just "Invalid state. Logged you out."
            }
      LoggedOut s@{ user: Just user, pass: Just pass } -> do
        result <- H.lift $ login user pass
        case result of
          Left msg -> H.put $ LoggedOut $ s { error = Just msg }
          Right _ -> H.put $ LoggedIn { readOnly: false, user }
      LoggedOut s -> H.put $ LoggedOut $ s { error = Just "Need to enter username and password" }
  Logout -> do
    H.lift logout
    H.put $ LoggedOut mempty
