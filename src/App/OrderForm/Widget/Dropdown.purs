module App.OrderForm.Widget.Dropdown (Slot, Output(..), proxy, component) where

import Prelude
import App.SchemaDataSource (DataSourceEnumResult)
import Css as Css
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Maybe (Maybe(..))
import Data.SmartSpec as SS
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetDropdown"
proxy = Proxy

type Input m
  = { value :: Maybe SS.ConfigValue
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    }

type Output
  = Maybe SS.ConfigValue

type State m
  = { selected :: Maybe SS.ConfigValue
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    }

data Action
  = Initialize
  | Select Int

component ::
  forall query m.
  MonadAff m => CredentialStore m => H.Component query (Input m) Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

initialState :: forall m. Input m -> State m
initialState input =
  { selected: input.value
  , available: Idle
  , getEnumData: input.getEnumData
  }

render ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  State m -> H.ComponentHTML Action slots m
render st = case st.available of
  Idle -> HH.div [ HP.classes infoClasses ] [ HH.text "Data not loaded …" ]
  Loading -> HH.div [ HP.classes loadingClasses ] [ HH.text "Loading data …" ]
  Error msg -> HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ]
  Loaded [] -> HH.div [ HP.classes infoClasses ] [ HH.text "No data available …" ]
  Loaded available ->
    HH.select
      [ HE.onSelectedIndexChange Select ]
      $ [ HH.option
            [ HP.selected $ st.selected == Nothing
            , HP.disabled true
            ]
            [ HH.text $ "Please choose an option" ]
        ]
      <> map renderItem available
  where
  containerClasses = []

  infoClasses = containerClasses <> [ Css.c "p-2" ]

  loadingClasses = infoClasses <> [ Css.c "animate-pulse" ]

  renderItem (Tuple key v) =
    HH.option
      [ HP.selected $ st.selected == Just v ]
      [ HH.text key ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM (State m) Action slots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.modify \st -> st { available = Loading }
    lAvailable <- H.lift $ state.getEnumData Nothing
    H.modify_ \st ->
      st
        { selected =
          do
            inputValue <- st.selected
            available <- Loadable.toMaybe lAvailable
            snd <$> A.find (\(Tuple _ v) -> v == inputValue) available
        , available = lAvailable
        }
  Select idx -> do
    st' <-
      H.modify \st ->
        st
          { selected =
            snd
              <$> do
                  available <- Loadable.toMaybe st.available
                  available !! (idx - 1)
          }
    -- Let the parent component know about the new selection.
    H.raise st'.selected
