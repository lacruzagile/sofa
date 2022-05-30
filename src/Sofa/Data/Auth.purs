-- | Types and functions useful for authentication.
module Sofa.Data.Auth
  ( AuthEvent(..)
  , AuthInstance
  , Credentials
  , class CredentialStore
  , clearCredentials
  , credentialsAreReadOnly
  , getAuthInstance
  , getAuthorizationHeader
  , getCredentials
  , getUser
  , handleSsoRedirect
  , initialize
  , logout
  , mkAuthInstance
  , mkCredentials
  , mkSsoAuthorizeUrl
  , mkSsoRedirectUri
  , setCredentials
  , toEmitter
  ) where

import Prelude
import Affjax as AX
import Affjax.RequestBody (formURLEncoded)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Monad.Fork.Class (class MonadFork, fork)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, jsonEmptyObject, printJsonDecodeError, (.:), (:=), (~>))
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.DateTime.Instant (fromDateTime, instant, toDateTime, unInstant)
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as S
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Data.UUID as UUID
import Effect (Effect)
import Effect.AVar as AVarEff
import Effect.Aff (delay)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now (nowDateTime)
import Halogen.Subscription as HS
import Jwt as Jwt
import Web.HTML as Html
import Web.HTML.Location as HtmlLocation
import Web.HTML.Window as HtmlWindow
import Web.Storage.Storage as HtmlStorage
import Web.URL as Url
import Web.URL.URLSearchParams as UrlParams

-- | Base URL for SSO authentication endpoints.
foreign import ssoBaseUrl :: Effect String

type Error
  = String

newtype Credentials
  = Credentials
  { accessToken :: String
  , refreshToken :: String
  , expiry :: DateTime
  , user :: String
  }

-- | The authentication instance. This, for example, contains the fetch lock
-- | variable that ensures that we have at most one token fetch in flight.
newtype AuthInstance
  = AuthInstance
  { id :: UUID
  , fetchLock :: AVar Unit
  , eventListener :: HS.Listener AuthEvent
  , eventEmitter :: HS.Emitter AuthEvent
  }

data AuthEvent
  = EvLogin
  | EvLogout

instance decodeCredentials :: DecodeJson Credentials where
  decodeJson json = do
    o <- decodeJson json
    accessToken <- o .: "accessToken"
    refreshToken <- o .: "refreshToken"
    expiry <- do
      x <- o .: "expiry"
      i <- maybe (Left $ TypeMismatch "invalid expiry") Right $ instant $ Milliseconds x
      pure $ toDateTime i
    user <- o .: "user"
    pure $ Credentials { accessToken, refreshToken, expiry, user }

instance encodeCredentials :: EncodeJson Credentials where
  encodeJson (Credentials creds) =
    ("accessToken" := creds.accessToken)
      ~> ("refreshToken" := creds.refreshToken)
      ~> ("expiry" := unwrap (unInstant (fromDateTime creds.expiry)))
      ~> ("user" := creds.user)
      ~> jsonEmptyObject

-- | A type class that enriches a monad with the ability to manage
-- | authentication credentials. The underlying monad must support forked
-- | sub-processes since we fork off a background process that performs token
-- | refreshes.
class
  (MonadEffect m, Functor f, MonadFork f m) <= CredentialStore f m | m -> f where
  -- | Whether this credential store supports the set and clear operations.
  credentialsAreReadOnly :: m Boolean
  getCredentials :: m (Maybe Credentials)
  setCredentials :: Credentials -> m Unit
  clearCredentials :: m Unit
  -- | Returns the unique authenticator instance. The authentication instance
  -- | should be the same throughout the execution of the application.
  getAuthInstance :: m AuthInstance

newtype TokenResponse
  = TokenResponse
  { accessToken :: String
  , refreshToken :: String
  , expiresIn :: Int
  , scope :: String
  , tokenType :: String
  }

instance decodeJsonTokenResponse :: DecodeJson TokenResponse where
  decodeJson json = do
    o <- decodeJson json
    accessToken <- o .: "access_token"
    refreshToken <- o .: "refresh_token"
    expiresIn <- o .: "expires_in"
    scope <- o .: "scope"
    tokenType <- o .: "token_type"
    pure
      $ TokenResponse
          { accessToken
          , refreshToken
          , expiresIn
          , scope
          , tokenType
          }

newtype SsoAccessToken
  = SsoAccessToken { user :: String }

instance decodeSsoAccessToken :: DecodeJson SsoAccessToken where
  decodeJson json = do
    o <- decodeJson json
    user <- o .: "user_name"
    pure $ SsoAccessToken { user }

-- | Make static credentials, for use when deployed in Salesforce.
mkCredentials :: String -> String -> Credentials
mkCredentials user accessToken =
  Credentials
    { accessToken
    , refreshToken: ""
    , expiry: top -- Never expire token.
    , user
    }

getUser :: Credentials -> String
getUser (Credentials { user }) = user

toEmitter :: AuthInstance -> HS.Emitter AuthEvent
toEmitter (AuthInstance { eventEmitter }) = eventEmitter

notifyEvent ::
  forall f m.
  CredentialStore f m =>
  AuthEvent -> m Unit
notifyEvent event = do
  AuthInstance { eventListener } <- getAuthInstance
  liftEffect $ HS.notify eventListener event

-- | Creates an authentication instance.
mkAuthInstance ∷ Effect AuthInstance
mkAuthInstance = do
  id <- genUUID
  fetchLock <- AVarEff.new unit
  { emitter: eventEmitter, listener: eventListener } <- HS.create
  pure $ AuthInstance { id, fetchLock, eventEmitter, eventListener }

-- | Base URL to use for the token service.
tokenBaseUrl :: String
tokenBaseUrl = ""

tokenUrl :: String
tokenUrl = tokenBaseUrl <> "/oauth/token"

emptyUrlParams :: UrlParams.URLSearchParams
emptyUrlParams = UrlParams.fromString ""

mkSsoRedirectUri :: Effect String
mkSsoRedirectUri = do
  location <- Html.window >>= HtmlWindow.location
  url <- Url.unsafeFromAbsolute <$> HtmlLocation.href location
  let
    -- The return path is the hash component of the URL.
    path = fromMaybe "" $ S.stripPrefix (Pattern "#") $ Url.hash url
  pure $ Url.toString
    $ Url.setSearch
        ( UrlParams.toString
            $ UrlParams.set "path" path
            $ emptyUrlParams
        )
    $ Url.setPathname "auth/sso"
    $ url

mkSsoAuthorizeUrl :: Effect String
mkSsoAuthorizeUrl = do
  redirectUri <- mkSsoRedirectUri
  baseUrl <- ssoBaseUrl
  pure
    $ Url.toString
    $ Url.setPathname "/oauth/authorize"
    $ Url.setSearch
        ( UrlParams.toString
            $ UrlParams.set "response_type" "code"
            $ UrlParams.set "client_id" "sofa"
            $ UrlParams.set "redirect_uri" redirectUri
            $ emptyUrlParams
        )
    $ Url.unsafeFromAbsolute baseUrl

-- | Finishes up SSO process if the current window location matches.
handleSsoRedirect ::
  forall f m. MonadAff m => CredentialStore f m => m Unit
handleSsoRedirect = do
  result <-
    liftEffect do
      location <- Html.window >>= HtmlWindow.location
      path <- HtmlLocation.pathname location
      case path of
        "/auth/sso" -> do
          url <- Url.unsafeFromAbsolute <$> HtmlLocation.href location
          let
            params = Url.searchParams url

            -- Also check for "state" parameter.
            targetPath = fromMaybe "/" $ UrlParams.get "path" params

            -- Recreate the exact redirect URL since it is needed when asking
            -- for the tokens. Note, `Url.toString` URL encodes any `/` symbols
            -- in the `path` parameter so we have to undo this.
            redirectUrl =
              Url.toString
                $ Url.setSearch (UrlParams.toString $ UrlParams.delete "code" params)
                $ url

            targetUrl =
              Url.toString
                $ Url.setPathname ""
                $ Url.setSearch ""
                $ Url.setHash targetPath
                $ url
          -- Handle the code parameter. Specifically, if there is one, return
          -- that for later fetch of authentication token.
          pure case UrlParams.get "code" params of
            Nothing -> Nothing
            Just authCode ->
              Just
                { authCode
                , location
                , redirectUrl
                , targetUrl
                }
        _ -> pure Nothing
  case result of
    Nothing -> pure unit
    Just { authCode, location, redirectUrl, targetUrl } -> do
      void
        $ fetchToken false
            [ Tuple "grant_type" (Just "authorization_code")
            , Tuple "code" (Just authCode)
            , Tuple "redirect_uri" (Just redirectUrl)
            ]
      -- Change location to the desired target path.
      liftEffect $ HtmlLocation.replace targetUrl location

fetchToken ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  Boolean -> Array (Tuple String (Maybe String)) -> m (Either String Credentials)
fetchToken schedule formFields = do
  AuthInstance authInstance <- getAuthInstance
  -- Try to take the lock, leaving the avar empty. Any fetch that occurs while
  -- the avar is empty will block until the winning fetch completes, then we can
  -- retrieve the credentials from the environment.
  mLock <- liftAff $ AVar.tryTake authInstance.fetchLock
  case mLock of
    Nothing -> do
      -- Wait for lock to release.
      void $ liftAff $ AVar.read authInstance.fetchLock
      liftEffect $ Console.log $ "Waited for lock"
      -- Read the credentials from the environment.
      maybe (Left "Credentials gone") Right <$> getCredentials
    Just _ -> do
      result <- runFetch
      -- Release the lock.
      liftAff $ AVar.put unit authInstance.fetchLock
      pure result
  where
  handleResponse = case _ of
    Left err -> throwError $ AX.printError err
    Right resp
      | statusOk resp.status ->
        withExceptT printJsonDecodeError
          $ except
          $ decodeJson resp.body
      | otherwise -> throwError "Authentication error"
    where
    statusOk (StatusCode n) = 200 <= n && n < 300

  handleAccessToken accessToken =
    withExceptT (\_ -> "Invalid access token")
      $ except
      $ Jwt.decodeWith decodeJson accessToken

  runFetch =
    runExceptT do
      mresponse <-
        liftAff
          $ AX.request
          $ AX.defaultRequest
              { url = tokenUrl
              , method = Left POST
              , timeout = Just (Milliseconds 60_000.0)
              , content = Just $ formURLEncoded $ FormURLEncoded formFields
              , responseFormat = ResponseFormat.json
              }
      TokenResponse tokenResp <- handleResponse mresponse
      SsoAccessToken accessTokenResp <- handleAccessToken tokenResp.accessToken
      now <- liftEffect nowDateTime
      let
        -- Calculate expiry timestamp. Note, the expiry is 30 seconds before the
        -- one claimed to allow for clock mismatches.
        offset = Seconds $ Int.toNumber tokenResp.expiresIn - 30.0

        expiry = fromMaybe now $ DateTime.adjust offset now

        creds =
          Credentials
            { accessToken: tokenResp.accessToken
            , refreshToken: tokenResp.refreshToken
            , expiry
            , user: accessTokenResp.user
            }
      lift do
        setCredentials creds
        when schedule $ scheduleRefresh
      pure creds

refresh ::
  forall f m.
  MonadAff m =>
  CredentialStore f m => String -> m (Either Error Credentials)
refresh refreshToken = do
  result <-
    fetchToken true
      [ Tuple "grant_type" (Just "refresh_token")
      , Tuple "refresh_token" (Just refreshToken)
      ]
  case result of
    Left _ -> logout
    Right _ -> pure unit
  pure result

logout :: forall f m. CredentialStore f m => m Unit
logout = do
  clearCredentials
  notifyEvent EvLogout

-- | Gets the credentials, renewing if expired.
getActiveCredentials ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  m (Either Error Credentials)
getActiveCredentials =
  runExceptT do
    mCreds <- lift getCredentials
    now <- liftEffect nowDateTime
    case mCreds of
      Just (Credentials creds)
        | creds.expiry > now -> pure (Credentials creds)
        | otherwise -> ExceptT $ refresh creds.refreshToken
      _ -> throwError "Not logged in"

getAuthorizationHeader ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  m (Either Error RequestHeader)
getAuthorizationHeader =
  runExceptT do
    Credentials creds <- ExceptT getActiveCredentials
    pure $ RequestHeader "Authorization" ("Bearer " <> creds.accessToken)

authInstanceStorageKey :: String
authInstanceStorageKey = "sofa-auth-instance"

-- | Initializes the authentication mechanism. Intended to be called once at
-- | application start. This will pick up any existing credentials and ensure
-- | they start getting refreshed.
initialize :: forall f m. MonadAff m ⇒ CredentialStore f m ⇒ m Unit
initialize = do
  -- If the current execution environment uses read-only credentials then
  -- initialization is a no-op.
  whenM (not <$> credentialsAreReadOnly) do
    -- Write the instance ID to session storage. This will help avoid duplicate
    -- refreshes when doing hot-reloading.
    AuthInstance { id } <- getAuthInstance
    liftEffect
      $ Html.window
      >>= HtmlWindow.sessionStorage
      >>= HtmlStorage.setItem authInstanceStorageKey (UUID.toString id)
    -- Schedule an access token refresh.
    scheduleRefresh

scheduleRefresh ::
  forall f m. MonadAff m ⇒ CredentialStore f m ⇒ m Unit
scheduleRefresh = do
  mCreds <- getCredentials
  case mCreds of
    Nothing -> pure unit
    Just (Credentials { expiry }) -> do
      now <- liftEffect nowDateTime
      let
        expiresIn = DateTime.diff expiry now
      void $ fork $ startRefreshJob expiresIn
  where
  startRefreshJob :: MonadAff m => CredentialStore f m => Milliseconds -> m Unit
  startRefreshJob expiresIn = do
    liftAff $ delay $ expiresIn
    -- If we are still the active instance then perform the actual refresh. We
    -- may not be the active, e.g., due to hot-reloading substituting the SOFA
    -- instance under our feet.
    whenM isActiveInstance do
      mCreds <- getCredentials
      case mCreds of
        Nothing -> pure unit
        Just (Credentials { refreshToken }) -> do
          refreshResult <- refresh refreshToken
          liftEffect
            $ Console.log
            $ case refreshResult of
                Left err -> "Error refreshing access token: " <> err
                Right _ -> "Access token refreshed"

  isActiveInstance = do
    storedId <-
      liftEffect
        $ Html.window
        >>= HtmlWindow.sessionStorage
        >>= HtmlStorage.getItem authInstanceStorageKey
    AuthInstance { id: ourId } <- getAuthInstance
    pure $ storedId == Just (UUID.toString ourId)
