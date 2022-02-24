module App.SchemaDataSource
  ( DataSourceEnumResult
  , getDataSourceEnum
  ) where

import Prelude
import App.Requests as Requests
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.SmartSpec (BillingAccountId(..), Commercial(..), ConfigValue, SchemaDataSourceEnum(..))
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import JSURI (encodeURIComponent)

-- | The 'replaceAll' method taking a function as replacer.
foreign import replaceAllFun :: String -> (Unit -> String) -> String -> String

type DataSourceEnumResult
  = Loadable (Array (Tuple String ConfigValue))

encUri ∷ String -> Maybe String
encUri = encodeURIComponent

-- | Function that provides the data indicated by a enumeration schema data
-- | source. This function is intended to be given to the different schema
-- | widgets in a partially applied format.
getDataSourceEnum ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  { getCommercial :: Unit -> Maybe Commercial } ->
  SchemaDataSourceEnum ->
  Maybe String ->
  m (Loadable (Array (Tuple String ConfigValue)))
getDataSourceEnum vars dataSource input = case dataSource of
  SdsEnumMap { entries } ->
    let
      available = Map.toUnfoldable entries :: Array (Tuple String ConfigValue)
    in
      pure $ Loaded available
  SdsEnumHttpGet { url: urlTemplate, authenticate } ->
    let
      varCommercial _ =
        fromMaybe "" do
          Commercial commercial <- vars.getCommercial unit
          BillingAccountId baid <- commercial.billingAccountId
          encUri baid

      varInput _ = fromMaybe "" $ encUri =<< input

      url =
        replaceAllFun "${input}" varInput
          $ replaceAllFun "${commercial.billingAccountId}" varCommercial
          $ urlTemplate
    in
      Requests.getDataSourceEnum url authenticate
