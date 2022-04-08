module Sofa.App.OrderForm.Widget.Checkbox (Slot, Output(..), proxy, component) where

import Prelude
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.SchemaDataSource (DataSourceEnumResult)
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetCheckbox"
proxy = Proxy

type Input m
  = { value :: Array SS.ConfigValue
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    }

type Output
  = Array SS.ConfigValue

type State m
  = { selected :: Set SS.ConfigValue
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    }

data Action
  = Initialize
  | Check SS.ConfigValue Boolean

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

initialState ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Input m -> State m
initialState input =
  { selected: Set.fromFoldable input.value
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
    HH.fieldset [ HP.classes [ Css.c "flex", Css.c "flex-col", Css.c "gap-y-2" ] ]
      $ map renderItem available
  where
  containerClasses = []

  infoClasses = containerClasses <> [ Css.c "p-2" ]

  loadingClasses = infoClasses <> [ Css.c "animate-pulse" ]

  renderItem (Tuple key v) =
    HH.label_
      [ HH.input
          [ HP.type_ InputCheckbox
          , HP.class_ (Css.c "nectary-input-checkbox")
          , HP.checked $ Set.member v st.selected
          , HE.onChecked $ Check v
          ]
      , HH.span [ HP.class_ (Css.c "ml-2") ] [ HH.text key ]
      ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM (State m) Action slots Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.modify \st -> st { available = Loading }
    lAvailable <- H.lift $ state.getEnumData Nothing
    H.modify_ \st -> st { available = lAvailable }
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
