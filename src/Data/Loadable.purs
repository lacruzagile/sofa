module Data.Loadable (Loadable(..), getJson, getRJson, postRJson) where

import Prelude
import Affjax (printError)
import Affjax.RequestHeader (RequestHeader(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (default, on)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Simple.Ajax (_affjaxError, _notFound, _parseError)
import Simple.Ajax as AJX

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

-- | Fetch JSON from an URL.
getJson :: forall a m. DecodeJson a => MonadAff m => MonadEffect m => String -> m (Loadable a)
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
getRJson :: forall a m. DecodeJson a => MonadAff m => MonadEffect m => String -> String -> m (Loadable a)
getRJson token url = do
  res <- liftAff $ AJX.getR { headers: [ RequestHeader "Authorization" ("Bearer " <> token) ] } url
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
  MonadEffect m =>
  String ->
  String ->
  a ->
  m (Loadable b)
postRJson token url body = do
  res <-
    liftAff
      $ AJX.postR
          { headers: [ RequestHeader "Authorization" ("Bearer " <> token) ] }
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
