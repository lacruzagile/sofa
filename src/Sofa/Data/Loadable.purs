module Sofa.Data.Loadable
  ( Loadable(..)
  , isLoading
  , isLoaded
  , toMaybe
  , deleteR_
  , getJson
  , getRJson
  , patchRJson
  , postRJson
  , postRJson_
  ) where

import Prelude
import Affjax (printError)
import Data.Argonaut (class DecodeJson, class EncodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (default, on)
import Effect.Aff.Class (class MonadAff, liftAff)
import Simple.Ajax (_affjaxError, _notFound, _parseError)
import Simple.Ajax as AJX
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

-- | Fetch JSON from an URL.
getJson :: forall a m. DecodeJson a => MonadAff m => String -> m (Loadable a)
getJson url = do
  res <- liftAff $ AJX.get url
  pure
    $ case res of
        Left error ->
          Error
            $ default "Generic error"
            # on _affjaxError printError
            # on _notFound (const "Not found")
            # on _parseError printJsonDecodeError
            $ error
        Right content -> Loaded content

-- | Fetch JSON from an URL.
getRJson ::
  forall a m.
  DecodeJson a =>
  MonadAff m =>
  CredentialStore m =>
  String -> m (Loadable a)
getRJson url = do
  creds <- getAuthorizationHeader
  case creds of
    Left err -> pure $ Error err
    Right authHdr -> do
      res <- liftAff $ AJX.getR { headers: [ authHdr ] } url
      pure
        $ case res of
            Left error ->
              Error
                $ default "Generic error"
                # on _affjaxError printError
                # on _notFound (const "Not found")
                # on _parseError printJsonDecodeError
                $ error
            Right content -> Loaded content

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
patchRJson url body = do
  creds <- getAuthorizationHeader
  case creds of
    Left err -> pure $ Error err
    Right authHdr -> do
      res <-
        liftAff
          $ AJX.patchR
              { headers: [ authHdr ] }
              url
              (Just body)
      pure
        $ case res of
            Left error ->
              Error
                $ default "Generic error"
                # on _affjaxError printError
                # on _notFound (const "Not found")
                # on _parseError printJsonDecodeError
                $ error
            Right content -> Loaded content

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
postRJson url body = do
  creds <- getAuthorizationHeader
  case creds of
    Left err -> pure $ Error err
    Right authHdr -> do
      res <-
        liftAff
          $ AJX.postR
              { headers: [ authHdr ] }
              url
              (Just body)
      pure
        $ case res of
            Left error ->
              Error
                $ default "Generic error"
                # on _affjaxError printError
                # on _notFound (const "Not found")
                # on _parseError printJsonDecodeError
                $ error
            Right content -> Loaded content

-- | Submit an empty POST request and parse the response.
postRJson_ ::
  forall a m.
  DecodeJson a =>
  MonadAff m =>
  CredentialStore m =>
  String ->
  m (Loadable a)
postRJson_ url = do
  creds <- getAuthorizationHeader
  case creds of
    Left err -> pure $ Error err
    Right authHdr -> do
      res <- liftAff $ AJX.postR { headers: [ authHdr ] } url (Nothing :: Maybe Int)
      pure
        $ case res of
            Left error ->
              Error
                $ default "Generic error"
                # on _affjaxError printError
                # on _notFound (const "Not found")
                # on _parseError printJsonDecodeError
                $ error
            Right content -> Loaded content

-- | Submit a DELETE request and parse the response.
deleteR_ ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  String ->
  m (Loadable Unit)
deleteR_ url = do
  creds <- getAuthorizationHeader
  case creds of
    Left err -> pure $ Error err
    Right authHdr -> do
      res <- liftAff $ AJX.deleteR_ { headers: [ authHdr ] } url
      pure
        $ case res of
            Left error ->
              Error
                $ default "Generic error"
                # on _affjaxError printError
                # on _notFound (const "Not found")
                $ error
            Right content -> Loaded content
