-- | Types and functions useful for authentication.
module Data.Auth
  ( class CredentialStore
  , Credentials
  , clearCredentials
  , getAuthorizationHeader
  , getCredentials
  , login
  , logout
  , setCredentials
  ) where

import Prelude
import Affjax as AX
import Affjax.RequestBody (formURLEncoded)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.Except.Trans (lift)
import Data.Argonaut (class DecodeJson, decodeJson, printJsonDecodeError, (.:))
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Base64 as Base64
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)

type Error
  = String

type AuthConfig
  = { tokenUrl :: String }

type Credentials
  = { token :: String
    , expiry :: DateTime
    , user :: String
    , pass :: String
    }

class
  Monad m <= CredentialStore m where
  getCredentials :: m (Maybe Credentials)
  setCredentials :: Credentials -> m Unit
  clearCredentials :: m Unit

newtype TokenResponse
  = TokenResponse
  { accessToken :: String
  , expiresIn :: Int
  , scope :: String
  , tokenType :: String
  }

instance decodeJsonTokenResponse :: DecodeJson TokenResponse where
  decodeJson json = do
    o <- decodeJson json
    accessToken <- o .: "access_token"
    expiresIn <- o .: "expires_in"
    scope <- o .: "scope"
    tokenType <- o .: "token_type"
    pure $ TokenResponse { accessToken, expiresIn, scope, tokenType }

authConfig :: AuthConfig
authConfig = { tokenUrl: "/oauth2/token" }

basicAuth :: String -> String -> RequestHeader
basicAuth user pass =
  let
    encoded = Base64.encode $ user <> ":" <> pass
  in
    RequestHeader "Authorization" $ "Basic " <> encoded

login :: forall m. MonadAff m => String -> String -> CredentialStore m => m (Either Error Credentials)
login user pass =
  runExceptT do
    result <-
      liftAff $ AX.request
        $ AX.defaultRequest
            { url = authConfig.tokenUrl
            , method = Left POST
            , headers = [ basicAuth user pass ]
            , content = Just $ formURLEncoded $ FormURLEncoded [ Tuple "grant_type" (Just "client_credentials") ]
            , responseFormat = ResponseFormat.json
            }
    case result of
      Left err -> throwError $ AX.printError err
      Right response -> do
        TokenResponse tokenResp <- withExceptT printJsonDecodeError $ except $ decodeJson response.body
        now <- liftEffect nowDateTime
        let
          -- Calculate expiry timestamp. Note, the expiry is 30 seconds before the
          -- one claimed to allow for clock mismatches.
          offset = Seconds $ Int.toNumber tokenResp.expiresIn - 30.0

          expiry = fromMaybe now $ DateTime.adjust offset now

          creds = { token: tokenResp.accessToken, expiry, user, pass }
        lift $ setCredentials creds
        pure creds

logout :: forall m. CredentialStore m => m Unit
logout = clearCredentials

-- | Gets the credentials, renewing if expired.
getActiveCredentials :: forall m. MonadAff m => CredentialStore m => m (Either Error Credentials)
getActiveCredentials =
  runExceptT do
    mcreds <- lift getCredentials
    now <- liftEffect nowDateTime
    case mcreds of
      Just creds
        | creds.expiry > now -> pure creds
        | otherwise -> ExceptT $ login creds.user creds.pass
      _ -> throwError "Not logged in"

getAuthorizationHeader :: forall m. MonadAff m => CredentialStore m => m (Either Error RequestHeader)
getAuthorizationHeader =
  runExceptT do
    creds <- ExceptT getActiveCredentials
    pure $ RequestHeader "Authorization" ("Bearer " <> creds.token)
