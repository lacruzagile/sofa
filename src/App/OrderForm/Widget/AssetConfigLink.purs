module App.OrderForm.Widget.AssetConfigLink (Slot, Output(..), proxy, component) where

import Prelude
import Control.Alternative (guard)
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Either (either)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
import Data.String.Regex as Re
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetAssetConfigLink"
proxy = Proxy

type Input
  = { value :: Maybe SS.ConfigValue
    , skuPattern :: String -- ^ Regex of eligible SKUs.
    , configs :: Array (Tuple SS.SkuCode (Array SS.OrderLineConfig))
    -- ^ Array of sku / config elements.
    }

type Output
  = Maybe SS.ConfigValue

type State
  = { selectedId :: Maybe String
    , skuPattern :: String
    , value :: Maybe SS.ConfigValue
    , configIds :: Array String
    }

data Action
  = Receive Input
  | Select Int

component ::
  forall query m.
  MonadAff m => CredentialStore m => H.Component query Input Output m
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
          Just (SS.CvString s) -> Just s
          _ -> Nothing
        _ -> Nothing
  , skuPattern: input.skuPattern
  , value: input.value
  , configIds:
      do
        re <- either (const []) A.singleton $ Re.regex input.skuPattern mempty
        Tuple (SS.SkuCode sku) configs <- input.configs
        guard $ Re.test re sku
        SS.OrderLineConfig { id } <- configs
        maybe [] pure id
  }

render ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action slots m
render st =
  HH.select
    [ HE.onSelectedIndexChange Select ]
    $ [ HH.option
          [ HP.selected $ st.selectedId == Nothing
          , HP.disabled true
          ]
          [ HH.text $ "Please choose a configuration" ]
      ]
    <> map renderItem st.configIds
  where
  renderItem configId =
    HH.option
      [ HP.selected $ st.selectedId == Just configId ]
      [ HH.text configId ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM (State) Action slots Output m Unit
handleAction = case _ of
  Receive input -> do
    let
      newState = initialState input
    oldConfigIds <- H.gets _.configIds
    if newState.configIds == oldConfigIds then
      pure unit
    else
      H.put newState
  Select idx -> do
    st' <- H.modify \st -> st { selectedId = st.configIds !! (idx - 1) }
    -- Let the parent component know about the new selection.
    H.raise do
      configId <- st'.selectedId
      value <- st'.value
      case value of
        SS.CvObject x ->
          Just
            $ SS.CvObject
            $ Map.insert "configId" (SS.CvString configId) x
        _ -> Nothing
