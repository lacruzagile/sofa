module App
  ( AppM
  , runAppM
  ) where

import Prelude
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut (decodeJson, encodeJson, jsonParser, stringify)
import Data.Auth (class CredentialStore)
import Data.Deployment (Deployment)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Web.HTML as Html
import Web.HTML.Window as HtmlWindow
import Web.Storage.Storage as LS

type Env
  = { deployment :: Deployment
    }

newtype AppM a
  = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

instance credentialStoreAppM :: CredentialStore AppM where
  getCredentials =
    AppM
      $ ReaderT \_ ->
          liftEffect
            $ do
                w <- Html.window
                s <- HtmlWindow.sessionStorage w
                mJsonStr <- LS.getItem "sofa-cred" s
                pure
                  $ do
                      jsonStr <- mJsonStr
                      json <- toMaybe (jsonParser jsonStr)
                      toMaybe (decodeJson json)
    where
    toMaybe :: forall a b. Either a b -> Maybe b
    toMaybe = either (const Nothing) Just
  setCredentials creds =
    AppM
      $ ReaderT \_ ->
          liftEffect
            $ do
                w <- Html.window
                s <- HtmlWindow.sessionStorage w
                let
                  jsonStr = stringify (encodeJson creds)
                log $ "Storing " <> jsonStr
                LS.setItem "sofa-cred" jsonStr s
  clearCredentials =
    AppM
      $ ReaderT \_ ->
          liftEffect
            $ do
                w <- Html.window
                s <- HtmlWindow.sessionStorage w
                LS.removeItem "sofa-cred" s

-- | Runs the `AppM` monad, takes the initial credentials as input.
runAppM :: forall a. Deployment -> AppM a -> Aff a
runAppM deployment (AppM m) = runReaderT m { deployment }
