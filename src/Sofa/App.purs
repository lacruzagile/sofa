module Sofa.App
  ( Env
  , AppM
  , runAppM
  ) where

import Prelude
import Control.Monad.Fork.Class (class MonadFork)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Argonaut (decodeJson, encodeJson, jsonParser, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Sofa.Component.Alerts (class MonadAlert, AlertSink)
import Sofa.Data.Auth (class CredentialStore, AuthInstance, mkCredentials)
import Sofa.Data.Deployment (class MonadDeployment, Deployment)
import Sofa.Data.Deployment as Deployment
import Web.HTML as Html
import Web.HTML.Window as HtmlWindow
import Web.Storage.Storage as LS

-- | The SOFA runtime environment.
-- |
-- | - `deployment` – indicates the current deployment environment
-- | - `alertSink` – the sink for global alerts
-- | - `authInstance` – the authentication state
type Env
  = { deployment :: Deployment
    , alertSink :: AlertSink
    , authInstance :: AuthInstance
    }

-- | The SOFA application base monad, it is essentially the `Aff` monad combined
-- | with a reader of `Env`.
newtype AppM a
  = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadForkAppM :: MonadFork Fiber AppM

instance monadAlertAppM :: MonadAlert AppM where
  getAlertSink = do
    AppM
      $ ReaderT
      $ \{ alertSink } -> pure alertSink

instance credentialStoreAppM :: CredentialStore Fiber AppM where
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
            mJsonStr <- LS.getItem authCredentialsStorageKey s
            pure
              $ do
                  jsonStr <- mJsonStr
                  json <- hush (jsonParser jsonStr)
                  hush (decodeJson json)

    handleSalesforce sf = pure $ Just $ mkCredentials sf.userEmail sf.accessToken
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
            LS.setItem authCredentialsStorageKey jsonStr s
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
            LS.removeItem authCredentialsStorageKey s
  getAuthInstance = AppM $ ReaderT \env -> pure env.authInstance

instance deploymentAppM :: MonadDeployment AppM where
  getDeployment = AppM $ ReaderT \env -> pure env.deployment

authCredentialsStorageKey :: String
authCredentialsStorageKey = "sofa-auth-credentials"

-- | Runs the `AppM` monad, takes an environment as input.
runAppM :: forall a. Env -> AppM a -> Aff a
runAppM env (AppM m) = runReaderT m env
