module Sofa.App
  ( AppM
  , runAppM
  ) where

import Prelude
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut (decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Sofa.Data.Auth (class CredentialStore, Credentials(..))
import Sofa.Data.Deployment (Deployment)
import Sofa.Data.Deployment as Deployment
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
  credentialsAreReadOnly =
    AppM
      $ ReaderT
      $ \{ deployment } ->
          pure
            $ case deployment of
                Deployment.Standard -> false
                Deployment.Salesforce _ -> true
  getCredentials =
    AppM
      $ ReaderT
      $ \{ deployment } -> case deployment of
          Deployment.Standard -> handleStandard
          Deployment.Salesforce sf -> handleSalesforce sf
    where
    handleStandard =
      liftEffect
        $ do
            w <- Html.window
            s <- HtmlWindow.sessionStorage w
            mJsonStr <- LS.getItem "sofa-cred" s
            pure
              $ do
                  jsonStr <- mJsonStr
                  json <- hush (jsonParser jsonStr)
                  hush (decodeJson json)

    handleSalesforce sf =
      pure $ Just
        $ Credentials
            { token: sf.accessToken
            , expiry: top -- Never expire token.
            , user: sf.userEmail
            , pass: ""
            }
  setCredentials creds =
    AppM
      $ ReaderT
      $ \{ deployment } -> case deployment of
          Deployment.Standard -> handleStandard
          Deployment.Salesforce _ -> pure unit
    where
    handleStandard =
      liftEffect
        $ do
            w <- Html.window
            s <- HtmlWindow.sessionStorage w
            let
              jsonStr = stringify (encodeJson creds)
            LS.setItem "sofa-cred" jsonStr s
  clearCredentials =
    AppM
      $ ReaderT
      $ \{ deployment } -> case deployment of
          Deployment.Standard -> handleStandard
          Deployment.Salesforce _ -> pure unit
    where
    handleStandard =
      liftEffect
        $ do
            w <- Html.window
            s <- HtmlWindow.sessionStorage w
            LS.removeItem "sofa-cred" s

-- | Runs the `AppM` monad, takes the initial credentials as input.
runAppM :: forall a. Deployment -> AppM a -> Aff a
runAppM deployment (AppM m) = runReaderT m { deployment }
