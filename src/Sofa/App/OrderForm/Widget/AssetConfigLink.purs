module Sofa.App.OrderForm.Widget.AssetConfigLink (SkuConfigs, Slot, Output(..), proxy, component) where

import Prelude
import Control.Alternative (guard)
import Data.Array as A
import Data.Either (either)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex as Re
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Sofa.Component.Select as Select
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetAssetConfigLink"
proxy = Proxy

type Slots
  = ( selectConfig :: Select.Slot Unit SS.OrderLineConfigId
    )

selectProxy :: Proxy "selectConfig"
selectProxy = Proxy

type Input
  = { value :: Maybe SS.ConfigValue
    , skuPattern :: String -- ^ Regex of eligible SKUs.
    , configs :: Array SkuConfigs
    -- ^ Array of configurations associated with different SKUs.
    }

type SkuConfigs
  = { sku :: SS.SkuCode
    , configs ::
        Array
          { label :: String
          , config :: SS.OrderLineConfig
          }
    }

type Output
  = Maybe SS.ConfigValue

type State
  = { selectedId :: Maybe SS.OrderLineConfigId
    , skuPattern :: String
    , value :: Maybe SS.ConfigValue
    , options :: Array { configId :: SS.OrderLineConfigId, label :: String }
    }

data Action
  = Receive Input
  | Select SS.OrderLineConfigId -- ^ When the users selects a configuration.

component ::
  forall query f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
    }

initialState :: Input -> State
initialState input =
  { selectedId:
      case input.value of
        Just (SS.CvObject v) -> case Map.lookup "configId" v of
          Just (SS.CvString s) -> Just $ SS.OrderLineConfigId s
          _ -> Nothing
        _ -> Nothing
  , skuPattern: input.skuPattern
  , value: input.value
  , options:
      do
        re <- either (const []) A.singleton $ Re.regex input.skuPattern mempty
        configs <- input.configs
        guard $ Re.test re (show configs.sku)
        { label, config: SS.OrderLineConfig { id } } <- configs.configs
        maybe [] (\configId -> [ { configId, label } ]) id
  }

render ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action Slots m
render st =
  HH.slot
    selectProxy
    unit
    Select.component
    ( Select.defaultInput
        { selected = st.selectedId
        , values = map (\o -> Tuple (HH.text o.label) o.configId) st.options
        , noSelectionText = "Please choose a configuration"
        }
    )
    Select

handleAction ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Receive input -> do
    let
      newState = initialState input
    oldOptions <- H.gets _.options
    when (newState.options /= oldOptions) do
      H.put newState
      -- Also inform the select component about the new options.
      H.tell selectProxy unit
        $ Select.SetValues
        $ map (\o -> Tuple (HH.text o.label) o.configId)
        $ newState.options
  Select selectedId -> do
    st' <- H.modify \st -> st { selectedId = Just selectedId }
    -- Let the parent component know about the new selection.
    H.raise do
      SS.OrderLineConfigId configId <- st'.selectedId
      value <- st'.value
      case value of
        SS.CvObject x ->
          Just
            $ SS.CvObject
            $ Map.insert "configId" (SS.CvString configId) x
        _ -> Nothing
