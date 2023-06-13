module Sofa.App.OrderForm.Widget.AssetConfigLink (SkuConfigs, Slot, Output(..), proxy, component) where

import Prelude
import Control.Alternative (guard)
import Data.Array as A
import Data.Either (either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String.Regex as Re
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Sofa.Component.Select as Select
import Sofa.Css as Css
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
    , readOnly :: Boolean
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
    , readOnly :: Boolean
    }

data Action
  = Initialize
  | Receive Input
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
            , initialize = initialize
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
  , readOnly: input.readOnly
  }

initialize :: Maybe Action
initialize = Just Initialize

render ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action Slots m
render st
  | st.readOnly =
    HH.div
      [ Css.classes
          [ "w-90pur"
          , "min-w-96"
          , "h-12"
          , "px-3"
          , "my-0.5"
          , "flex"
          , "items-center"
          , "rounded"
          , "bg-snow-100"
          ]
      ]
      [ HH.text
          $ fromMaybe "" do
              selectedId <- st.selectedId
              { label } <- A.find (\{ configId } -> configId == selectedId) st.options
              pure label
      ]
  | otherwise =
    HH.slot
      selectProxy
      unit
      Select.component
      ( Select.defaultInput
          { selected = st.selectedId
          , values = map (\o -> Tuple (HH.div [HP.title $ o.label] [HH.text $ o.label]) o.configId) st.options
          , noSelectionText = "Please choose a configuration"
          , wrapperClasses = [ Css.c "inline-block", Css.c "w-90pur", Css.c "min-w-96" ]
          }
      )
      Select

mkOutput :: State -> Maybe SS.ConfigValue
mkOutput state = do
  SS.OrderLineConfigId configId <- state.selectedId
  value <- state.value
  case value of
    SS.CvObject x ->
      Just
        $ SS.CvObject
        $ Map.insert "configId" (SS.CvString configId) x
    _ -> Nothing

-- | Checks the given state and if there is no existing selection and there is
-- | only one option then switch the selection to that one option.
selectOnlyOption ::
  forall m.
  MonadAff m =>
  State -> H.HalogenM State Action Slots Output m Unit
selectOnlyOption state = do
  let
    -- If only one option is available then grab its ID.
    onlyOptionId = case state.options of
      [ { configId } ] -> Just configId
      _ -> Nothing
  -- If there is no selection from the input _and_ there is only one option,
  -- then automatically select that option.
  when (isNothing state.selectedId && isJust onlyOptionId) do
    let
      newState = state { selectedId = onlyOptionId }
    H.put newState
    -- Inform the select component about the new selection.
    H.tell selectProxy unit $ Select.SetSelected newState.selectedId
    -- Let the parent know about the selection.
    H.raise $ mkOutput newState

handleAction ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    selectOnlyOption state
  Receive input -> do
    let
      newState = initialState input
    { selectedId: oldSelectedId, options: oldOptions } <- H.get
    when (newState.options /= oldOptions || isNothing oldSelectedId) do
      H.put newState
      -- Also inform the select component about the new options and selection.
      H.tell selectProxy unit
        $ Select.SetValues
        $ map (\o -> Tuple (HH.text o.label) o.configId)
        $ newState.options
      selectOnlyOption newState
  Select selectedId -> do
    newState <- H.modify _ { selectedId = Just selectedId }
    -- Let the parent component know about the new selection.
    H.raise $ mkOutput newState
