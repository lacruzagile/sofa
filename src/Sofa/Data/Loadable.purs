module Sofa.Data.Loadable
  ( Loadable(..)
  , deleteR_
  , getJson
  , getRJson
  , isIdle
  , isLoaded
  , isLoading
  , patchRJson
  , postRJson
  , postRJson_
  , toMaybe
  ) where

import Prelude
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.HTTP.Method as HTTP
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Sofa.Data.Auth (class CredentialStore, getAuthorizationHeader)

-- | A representation of URL data that is loaded. The load can, e.g., fail with
-- | an error message.
data Loadable a
  = Idle
  | Loaded a
  | Loading
  | Error String

derive instance functorLoadable :: Functor Loadable

instance applyLoadable :: Apply Loadable where
  apply Idle _ = Idle
  apply (Loaded f) r = f <$> r
  apply Loading _ = Loading
  apply (Error err) _ = Error err

instance applicationLoadable :: Applicative Loadable where
  pure = Loaded

instance bindLoadable :: Bind Loadable where
  bind Idle _ = Idle
  bind (Loaded x) f = f x
  bind Loading _ = Loading
  bind (Error err) _ = Error err

-- | Whether the given loadable is in an idle state.
isIdle :: forall a. Loadable a -> Boolean
isIdle = case _ of
  Idle -> true
  _ -> false

-- | Whether the given loadable is in a loading state.
isLoading :: forall a. Loadable a -> Boolean
isLoading = case _ of
  Loading -> true
  _ -> false

-- | Whether the given loadable is in a loading state.
isLoaded :: forall a. Loadable a -> Boolean
isLoaded = case _ of
  Loaded _ -> true
  _ -> false

-- | Converts the given loadable into a maybe value. A `Loaded` value is
-- | converted to a `Just`, all other loadable values are converted to
-- | `Nothing`.
toMaybe :: forall a. Loadable a -> Maybe a
toMaybe = case _ of
  Loaded x -> Just x
  _ -> Nothing

handleResponse ::
  forall result body r.
  (body -> Loadable result) ->
  Either AX.Error { body :: body, status :: StatusCode | r } ->
  Loadable result
handleResponse handleBody = case _ of
  Left err -> Error $ AX.printError err
  Right resp
    | statusOk resp.status -> handleBody resp.body
    | statusNotFound resp.status -> Error "Not found"
    | otherwise -> Error "Generic error"
  where
  statusOk (StatusCode n) = 200 <= n && n < 300

  statusNotFound (StatusCode n) = n == 404

handleEmptyResponse :: Either AX.Error (AX.Response Unit) -> Loadable Unit
handleEmptyResponse = handleResponse \_ -> Loaded unit

handleJsonResponse ::
  forall a.
  DecodeJson a =>
  Either AX.Error (AX.Response Json) -> Loadable a
handleJsonResponse =
  handleResponse \body -> case decodeJson body of
    Left err -> Error $ printJsonDecodeError err
    Right value -> Loaded value

withAuthorizationHeader ::
  forall m a.
  MonadAff m =>
  CredentialStore m =>
  (RequestHeader -> m (Loadable a)) -> m (Loadable a)
withAuthorizationHeader fetcher = do
  creds <- getAuthorizationHeader
  case creds of
    Left err -> pure $ Error err
    Right authHdr -> fetcher authHdr

-- | Fetch JSON from an URL.
getJson :: forall a m. DecodeJson a => MonadAff m => String -> m (Loadable a)
getJson url =
  liftAff
    $ map handleJsonResponse
    $ AX.request
    $ AX.defaultRequest
        { url = url
        , method = Left HTTP.GET
        , timeout = Just (Milliseconds 10_000.0)
        , responseFormat = ResponseFormat.json
        }

-- | Fetch JSON from an URL.
getRJson ::
  forall a m.
  DecodeJson a =>
  MonadAff m =>
  CredentialStore m =>
  String -> m (Loadable a)
getRJson url =
  withAuthorizationHeader \authHdr -> do
    liftAff
      $ map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.GET
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          }

-- | Submit JSON using a PATCH request and parse the response.
patchRJson ::
  forall a b m.
  EncodeJson a =>
  DecodeJson b =>
  MonadAff m =>
  CredentialStore m =>
  String ->
  a ->
  m (Loadable b)
patchRJson url body =
  withAuthorizationHeader \authHdr ->
    liftAff
      $ map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.PATCH
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          , content = Just $ RequestBody.json $ encodeJson body
          }

-- | Submit JSON using a POST request and parse the response.
postRJson ::
  forall a b m.
  EncodeJson a =>
  DecodeJson b =>
  MonadAff m =>
  CredentialStore m =>
  String ->
  a ->
  m (Loadable b)
postRJson url body =
  withAuthorizationHeader \authHdr ->
    liftAff
      $ map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.POST
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          , content = Just $ RequestBody.json $ encodeJson body
          }

-- | Submit an empty POST request and parse the response.
postRJson_ ::
  forall a m.
  DecodeJson a =>
  MonadAff m =>
  CredentialStore m =>
  String ->
  m (Loadable a)
postRJson_ url =
  withAuthorizationHeader \authHdr ->
    liftAff
      $ map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.POST
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          }

-- | Submit a DELETE request.
deleteR_ ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  String ->
  m (Loadable Unit)
deleteR_ url =
  withAuthorizationHeader \authHdr ->
    liftAff
      $ map handleEmptyResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.DELETE
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          }
