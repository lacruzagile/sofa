module App.OrderForm.Widget.Checkbox (Slot, Output(..), proxy, component) where

import Prelude
import App.Requests (getDataSourceEnum)
import Css as Css
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.SmartSpec as SS
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetCheckbox"
proxy = Proxy

type Input
  = { value :: Array SS.ConfigValue
    , dataSource :: SS.SchemaDataSourceEnum
    }

type Output
  = Array SS.ConfigValue

type State
  = { selected :: Set SS.ConfigValue
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , dataSource :: SS.SchemaDataSourceEnum
    }

data Action
  = Initialize
  | Check SS.ConfigValue Boolean

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
            , initialize = Just Initialize
            }
    }

initialState :: Input -> State
initialState input =
  { selected: Set.fromFoldable input.value
  , available: Idle
  , dataSource: input.dataSource
  }

render ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action slots m
render st = case st.available of
  Idle -> HH.div [ HP.classes infoClasses ] [ HH.text "Data not loaded …" ]
  Loading -> HH.div [ HP.classes loadingClasses ] [ HH.text "Loading data …" ]
  Error msg -> HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ]
  Loaded [] -> HH.div [ HP.classes infoClasses ] [ HH.text "No data available …" ]
  Loaded available ->
    HH.fieldset [ HP.classes [ Css.c "flex", Css.c "flex-col" ] ]
      $ map renderItem available
  where
  containerClasses = []

  infoClasses = containerClasses <> [ Css.c "p-2" ]

  loadingClasses = infoClasses <> [ Css.c "animate-pulse" ]

  renderItem (Tuple key v) =
    HH.label_
      [ HH.input
          [ HP.type_ InputCheckbox
          , HP.checked $ Set.member v st.selected
          , HE.onChecked $ Check v
          ]
      , HH.span [ HP.class_ (Css.c "ml-2") ] [ HH.text key ]
      ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    case state.dataSource of
      SS.SdsEnumMap { entries } ->
        let
          available = Map.toUnfoldable entries :: Array (Tuple String SS.ConfigValue)
        in
          H.modify_ \st -> st { available = Loaded available }
      SS.SdsEnumHttpGet { url } -> do
        H.modify_ \st -> st { available = Loading }
        result <- H.lift $ getDataSourceEnum url
        H.modify_ \st -> st { available = result }
  Check value checked -> do
    st' <-
      H.modify \st ->
        st
          { selected =
            if checked then
              Set.insert value st.selected
            else
              Set.delete value st.selected
          }
    -- Let the parent component know about the new selection.
    H.raise $ A.fromFoldable st'.selected
