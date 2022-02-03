module App.OrderForm.Widget.Radio (Slot, Output(..), proxy, component) where

import Prelude
import App.Requests (getDataSourceEnum)
import Css as Css
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Map as Map
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

proxy :: Proxy "widgetRadio"
proxy = Proxy

type Input
  = { value :: Maybe SS.ConfigValue
    , dataSource :: SS.SchemaDataSourceEnum
    }

type Output
  = Maybe SS.ConfigValue

type State
  = { selected :: Maybe SS.ConfigValue
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , dataSource :: SS.SchemaDataSourceEnum
    }

data Action
  = Initialize
  | Select SS.ConfigValue

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
  { selected: input.value
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
    HH.fieldset [ HP.classes [ Css.tw.flex, Css.tw.flexCol ] ]
      $ map renderItem available
  where
  containerClasses = []

  infoClasses = containerClasses <> [ Css.tw.p2 ]

  loadingClasses = infoClasses <> [ Css.tw.animatePulse ]

  renderItem (Tuple key v) =
    HH.label_
      [ HH.input
          [ HP.type_ InputRadio
          , HP.checked $ st.selected == Just v
          , HE.onChange \_ -> Select v
          ]
      , HH.span [ HP.class_ Css.tw.ml2 ] [ HH.text key ]
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
          H.modify_ \st ->
            st
              { selected =
                do
                  inputValue <- st.selected
                  snd <$> A.find (\(Tuple _ v) -> v == inputValue) available
              , available = Loaded available
              }
      SS.SdsEnumHttpGet { url } -> do
        H.modify_ \st -> st { available = Loading }
        result <- H.lift $ getDataSourceEnum url
        H.modify_ \st ->
          st
            { selected =
              do
                inputValue <- st.selected
                available <- Loadable.toMaybe result
                snd <$> A.find (\(Tuple _ v) -> v == inputValue) available
            , available = result
            }
  Select value -> do
    st' <- H.modify \st -> st { selected = Just value }
    -- Let the parent component know about the new selection.
    H.raise st'.selected