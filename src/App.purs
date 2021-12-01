module App
  ( AppM
  , runAppM
  ) where

import Prelude
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Auth (class CredentialStore, Credentials)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Env
  = { credentials :: Ref (Maybe Credentials)
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
      $ ReaderT \r -> liftEffect $ Ref.read r.credentials
  setCredentials creds =
    AppM
      $ ReaderT \r -> liftEffect $ Ref.write (Just creds) r.credentials
  clearCredentials =
    AppM
      $ ReaderT \r -> liftEffect $ Ref.write Nothing r.credentials

-- | Runs the `AppM` monad, takes the initial credentials as input.
runAppM :: forall a. Ref (Maybe Credentials) -> AppM a -> Aff a
runAppM credentials (AppM m) = runReaderT m { credentials }