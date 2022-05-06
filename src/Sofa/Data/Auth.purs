-- | Types and functions useful for authentication.
module Sofa.Data.Auth
  ( class CredentialStore
  , Credentials(..)
  , clearCredentials
  , getAuthorizationHeader
  , getCredentials
  , login
  , logout
  , setCredentials
  , credentialsAreReadOnly
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
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)

type Error
  = String

type AuthConfig
  = { tokenUrl :: String }

newtype Credentials
  = Credentials
  { accessToken :: String
  , refreshToken :: String
  , expiry :: DateTime
  , user :: String
  }

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
  Monad m <= CredentialStore m where
  -- | Whether this credential store supports the set and clear operations.
  credentialsAreReadOnly :: m Boolean
  getCredentials :: m (Maybe Credentials)
  setCredentials :: Credentials -> m Unit
  clearCredentials :: m Unit

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

-- | Base URL to use for the token service.
tokenBaseUrl :: String
tokenBaseUrl = ""

tokenUrl :: String
tokenUrl = tokenBaseUrl <> "/oauth/token"

fetchToken ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  String -> Array (Tuple String (Maybe String)) -> m (Either String Credentials)
fetchToken user formFields =
  runExceptT do
    mresponse <-
      liftAff
        $ AX.request
        $ AX.defaultRequest
            { url = tokenUrl
            , method = Left POST
            , timeout = Just (Milliseconds 20_000.0)
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
  forall m.
  MonadAff m =>
  CredentialStore m =>
  String -> String -> m (Either Error Credentials)
login user pass =
  fetchToken user
    [ Tuple "grant_type" (Just "password")
    , Tuple "username" (Just user)
    , Tuple "password" (Just pass)
    ]

refresh ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  String -> String -> m (Either Error Credentials)
refresh user refreshToken =
  fetchToken user
    [ Tuple "grant_type" (Just "refresh_token")
    , Tuple "refresh_token" (Just refreshToken)
    ]

logout :: forall m. CredentialStore m => m Unit
logout = clearCredentials

-- | Gets the credentials, renewing if expired.
getActiveCredentials ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
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
  forall m.
  MonadAff m =>
  CredentialStore m =>
  m (Either Error RequestHeader)
getAuthorizationHeader =
  runExceptT do
    Credentials creds <- ExceptT getActiveCredentials
    pure $ RequestHeader "Authorization" ("Bearer " <> creds.accessToken)
