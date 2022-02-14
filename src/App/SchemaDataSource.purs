module App.SchemaDataSource
  ( DataSourceEnumResult
  , getDataSourceEnum
  ) where

import Prelude
import App.Requests as Requests
import Data.Loadable (Loadable(..))
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.SmartSpec (Commercial(..), ConfigValue, SchemaDataSourceEnum(..))
import Data.String as S
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import JSURI (encodeURIComponent)

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
  { commercial :: Commercial } ->
  SchemaDataSourceEnum ->
  Maybe String ->
  m (Loadable (Array (Tuple String ConfigValue)))
getDataSourceEnum vars dataSource input = case dataSource of
  SdsEnumMap { entries } ->
    let
      available = Map.toUnfoldable entries :: Array (Tuple String ConfigValue)
    in
      pure $ Loaded available
  SdsEnumHttpGet { url: urlTemplate } -> do
    let
      { commercial: Commercial commercial } = vars

      url =
        S.replaceAll
          (S.Pattern "${input}")
          (S.Replacement (fromMaybe "" (encUri =<< input)))
          $ S.replaceAll
              (S.Pattern "${commercial.billingAccountId}")
              (S.Replacement (fromMaybe "" (encUri =<< map unwrap commercial.billingAccountId)))
          $ urlTemplate
    Requests.getDataSourceEnum url
