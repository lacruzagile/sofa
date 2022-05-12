-- | Types and functions useful for authentication.
module Sofa.Data.Auth
  ( AuthEvent(..)
  , AuthEventEmitter
  , AuthInstance
  , Credentials(..)
  , class CredentialStore
  , clearCredentials
  , credentialsAreReadOnly
  , getAuthEventEmitter
  , getAuthInstance
  , getAuthorizationHeader
  , getCredentials
  , initialize
  , login
  , logout
  , mkAuthEventEmitter
  , mkAuthInstance
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
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Tuple (Tuple(..))
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

type Error
  = String

newtype Credentials
  = Credentials
  { accessToken :: String
  , refreshToken :: String
  , expiry :: DateTime
  , user :: String
  }

newtype AuthEventEmitter
  = AuthEventEmitter (HS.SubscribeIO AuthEvent)

-- | The authentication instance, containing the fetch lock variable that
-- | ensures that we have at most one token fetch in flight.
newtype AuthInstance
  = AuthInstance { fetchLock :: AVar Unit }

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

class
  (MonadEffect m, Functor f, MonadFork f m) <= CredentialStore f m | m -> f where
  -- | Whether this credential store supports the set and clear operations.
  credentialsAreReadOnly :: m Boolean
  getCredentials :: m (Maybe Credentials)
  setCredentials :: Credentials -> m Unit
  clearCredentials :: m Unit
  -- | Authentication event emitter.
  getAuthEventEmitter :: m AuthEventEmitter
  --  | Returns the unique authenticator instance. This function is idempotent.
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

mkAuthEventEmitter :: Effect AuthEventEmitter
mkAuthEventEmitter = AuthEventEmitter <$> HS.create

toEmitter :: AuthEventEmitter -> HS.Emitter AuthEvent
toEmitter (AuthEventEmitter s) = s.emitter

notifyEvent ::
  forall f m.
  CredentialStore f m =>
  AuthEvent -> m Unit
notifyEvent event = do
  AuthEventEmitter { listener } <- getAuthEventEmitter
  liftEffect $ HS.notify listener event

-- | Creates an authentication instance.
mkAuthInstance ∷ Effect AuthInstance
mkAuthInstance = do
  fetchLock <- AVarEff.new unit
  pure $ AuthInstance { fetchLock }

-- | Base URL to use for the token service.
tokenBaseUrl :: String
tokenBaseUrl = ""

tokenUrl :: String
tokenUrl = tokenBaseUrl <> "/oauth/token"

fetchToken ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  String -> Array (Tuple String (Maybe String)) -> m (Either String Credentials)
fetchToken user formFields = do
  AuthInstance authInstance <- getAuthInstance
  -- Try to take the lock, leaving the avar empty. Any fetch that occurs while
  -- the avar is empty will block until the winning fetch completes, then we can
  -- retrieve the credentials from the environment.
  mLock <- liftAff $ AVar.tryTake authInstance.fetchLock
  case mLock of
    Nothing -> do
      -- Wait for lock to release.
      liftAff $ AVar.read authInstance.fetchLock
      -- Read the credentials from the environment.
      maybe (Left "Credentials gone") Right <$> getCredentials
    Just _ -> do
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
              , user
              }
        lift $ setCredentials creds
        lift $ scheduleRefresh offset
        -- Release the lock.
        liftAff $ AVar.put unit authInstance.fetchLock
        pure creds
  where
  handleResponse = case _ of
    Left err -> throwError $ AX.printError err
    Right resp
      | statusOk resp.status ->
        withExceptT printJsonDecodeError
          $ except
          $ decodeJson resp.body
      | statusBadRequest resp.status -> throwError "Bad username or password, try again."
      | otherwise -> throwError "Generic error"
    where
    statusOk (StatusCode n) = 200 <= n && n < 300

    statusBadRequest (StatusCode n) = 400 == n

login ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  String -> String -> m (Either Error Credentials)
login user pass = do
  result <-
    fetchToken user
      [ Tuple "grant_type" (Just "password")
      , Tuple "username" (Just user)
      , Tuple "password" (Just pass)
      ]
  case result of
    Left _ -> pure unit
    Right _ -> notifyEvent EvLogin
  pure result

refresh ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  String -> String -> m (Either Error Credentials)
refresh user refreshToken = do
  result <-
    fetchToken user
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
    mcreds <- lift getCredentials
    now <- liftEffect nowDateTime
    case mcreds of
      Just (Credentials creds)
        | creds.expiry > now -> pure (Credentials creds)
        | otherwise -> ExceptT $ refresh creds.user creds.refreshToken
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

-- Initializes the authentication mechanism. Intended to be called on
-- application start. This will pick up any existing credentials and ensure they
-- start getting refreshed.
initialize :: forall f m. MonadAff m ⇒ CredentialStore f m ⇒ m Unit
initialize = scheduleRefresh mempty

scheduleRefresh ::
  forall f m. MonadAff m ⇒ CredentialStore f m ⇒ Seconds → m Unit
scheduleRefresh (Seconds expiresIn) = void $ fork startRefreshJob
  where
  startRefreshJob :: MonadAff m => CredentialStore f m => m Unit
  startRefreshJob = do
    when (expiresIn > 0.0) do
      liftAff $ delay $ Milliseconds (expiresIn * 1000.0)
    mcreds <- getCredentials
    case mcreds of
      Nothing -> pure unit
      Just (Credentials { user, refreshToken }) -> do
        refreshResult <- refresh user refreshToken
        liftEffect
          $ Console.log
          $ case refreshResult of
              Left err -> "Error refreshing access token: " <> err
              Right _ -> "Access token refreshed"
