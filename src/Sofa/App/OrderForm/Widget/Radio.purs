module Sofa.App.OrderForm.Widget.Radio (Slot, Output(..), proxy, component) where

import Prelude
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.SchemaDataSource (DataSourceEnumResult)
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetRadio"
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
  | Select SS.ConfigValue

component ::
  forall query f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query (Input m) Output m
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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  Input m -> State m
initialState input =
  { selected: input.value
  , available: Idle
  , getEnumData: input.getEnumData
  }

render ::
  forall slots f m.
  MonadAff m =>
  CredentialStore f m =>
  State m -> H.ComponentHTML Action slots m
render st = case st.available of
  Idle -> HH.div [ HP.classes infoClasses ] [ HH.text "Data not loaded …" ]
  Loading -> HH.div [ HP.classes loadingClasses ] [ HH.text "Loading data …" ]
  Error msg -> HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ]
  Loaded [] -> HH.div [ HP.classes infoClasses ] [ HH.text "No data available …" ]
  Loaded available ->
    HH.fieldset [ Css.classes [ "flex", "flex-col" ] ]
      $ map renderItem available
  where
  infoClasses = [ Css.c "p-2" ]

  loadingClasses = infoClasses <> [ Css.c "animate-pulse" ]

  renderItem (Tuple key v) =
    HH.label_
      [ HH.input
          [ HP.type_ InputRadio
          , Css.class_ "nectary-input-radio"
          , HP.checked $ st.selected == Just v
          , HE.onChange \_ -> Select v
          ]
      , HH.span [ Css.class_ "ml-2" ] [ HH.text key ]
      ]

handleAction ::
  forall slots f m.
  MonadAff m =>
  CredentialStore f m =>
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
  Select value -> do
    st' <- H.modify \st -> st { selected = Just value }
    -- Let the parent component know about the new selection.
    H.raise st'.selected
