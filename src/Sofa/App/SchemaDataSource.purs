module Sofa.App.SchemaDataSource
  ( DataSourceEnumResult
  , DataSourceVars
  , getDataSourceEnum
  ) where

import Prelude
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import JSURI (encodeURIComponent)
import Sofa.App.Requests as Requests
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.SmartSpec (BillingAccountId(..), Commercial(..), Buyer(..), CrmAccountId(..), ConfigValue, SchemaDataSourceEnum(..))

-- | The 'replaceAll' method taking an effect as replacer.
foreign import replaceAllFun :: String -> (Unit -> String) -> String -> String

type DataSourceVars
  = { getCommercial :: Unit -> Maybe Commercial, 
      getBuyer :: Unit -> Maybe Buyer }

type DataSourceEnumResult
  = Loadable (Array (Tuple String ConfigValue))

encUri ∷ String -> Maybe String
encUri = encodeURIComponent

-- | Function that provides the data indicated by a enumeration schema data
-- | source. This function is intended to be given to the different schema
-- | widgets in a partially applied format.
getDataSourceEnum ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  DataSourceVars ->
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
      varCommercial = do
        mCommercial <- vars.getCommercial
        pure
          $ fromMaybe "" do
              Commercial commercial <- mCommercial
              BillingAccountId baid <- commercial.billingAccountId
              encUri baid

    
      varBuyer = do
        mBuyer <- vars.getBuyer
        pure
          $ fromMaybe "" do
              Buyer buyer <- mBuyer
              CrmAccountId caid <- buyer.crmAccountId
              encUri caid

      varInput = pure $ fromMaybe "" $ encUri =<< input

      applyVars =
        replaceAllFun "${input}" varInput
          <<< replaceAllFun "${commercial.billingAccountId}" varCommercial
          <<< replaceAllFun "${buyer.crmAccountId}" varBuyer

      url = applyVars urlTemplate
    in
      Requests.getDataSourceEnum url authenticate
