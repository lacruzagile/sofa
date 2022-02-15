module App.OrderForm.Widget.Typeahead (Slot, Output(..), proxy, component) where

import Prelude
import App.SchemaDataSource (DataSourceEnumResult)
import Css as Css
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Int as Int
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (selectInputText, setInputText)
import Select as Sel
import Select.Setters as SelSet
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetTypeahead"
proxy = Proxy

type Input m
  = { value :: Maybe SS.ConfigValue
    , minInputLength :: Int
    , debounceMs :: Int
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    }

type Output
  = Maybe SS.ConfigValue

type State m
  = ( selected :: Maybe (Tuple String SS.ConfigValue)
    , filtered :: Loadable (Array (Tuple String SS.ConfigValue))
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , minInputLength :: Int
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    )

data Action
  = Initialize
  | InputFocused

type Action'
  = Sel.Action Action

component ::
  forall query m.
  MonadAff m => CredentialStore m => H.Component query (Input m) Output m
component =
  H.mkComponent
    { initialState: identity
    , render: \st -> HH.slot selectLabel unit selectComponent st identity
    , eval: H.mkEval H.defaultEval { handleAction = H.raise }
    }
  where
  selectLabel = Proxy :: Proxy "select"

  selectComponent :: H.Component (Sel.Query query ()) (Input m) Output m
  selectComponent =
    Sel.component mapInput
      $ Sel.defaultSpec
          { initialize = Just Initialize
          , handleAction = handleAction
          , handleEvent = handleEvent
          , render = render
          }

  mapInput :: (Input m) -> Sel.Input (State m)
  mapInput input =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds $ Int.toNumber input.debounceMs)
    , search: Nothing
    , getItemCount: getDataItemCount
    , selected: (\v -> Tuple "" v) <$> input.value
    , filtered: Idle
    , available: Idle
    , minInputLength: input.minInputLength
    , getEnumData: input.getEnumData
    }

  getDataItemCount st = maybe 0 A.length $ Loadable.toMaybe $ st.filtered

  handleAction = case _ of
    Initialize -> do
      state <-
        H.modify \st ->
          st { filtered = Loading, available = Loading }
      lAvailable <- H.lift $ state.getEnumData Nothing
      state' <-
        H.modify \st ->
          st
            { selected =
              do
                Tuple _ inputValue <- st.selected
                available <- Loadable.toMaybe lAvailable
                A.find (\(Tuple _ v) -> v == inputValue) available
            , filtered = lAvailable
            , available = lAvailable
            }
      -- Set the input element to the full selection key.
      case state'.selected of
        Nothing -> pure unit
        Just (Tuple key _) -> setInputText "select-input" key
    InputFocused -> selectInputText "select-input"

  handleEvent = case _ of
    Sel.Searched str -> do
      state <- H.get
      when (S.length str >= state.minInputLength)
        $ H.modify_ \st -> st { filtered = filterAvailable str st.available }
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { search = ""
            , visibility = Sel.Off
            , selected =
              do
                filtered <- Loadable.toMaybe st.filtered
                filtered !! idx
            , filtered = st.available
            }
      -- Set the input element to the full selection key.
      case st'.selected of
        Just (Tuple key _) -> setInputText "select-input" key
        Nothing -> pure unit
      -- Let the parent component know about the new selection.
      H.raise $ map snd $ st'.selected
    _ -> pure unit

  render :: Sel.State (State m) -> H.ComponentHTML Action' () m
  render st = HH.div [ HP.class_ (Css.c "inline-block") ] [ renderInput, renderResults ]
    where
    renderInput :: H.ComponentHTML Action' () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.type_ HP.InputText
            , HP.classes [ Css.c "border" ]
            , HP.placeholder "Type to search value…"
            , HE.onFocus \_ -> Sel.Action InputFocused
            ]

    containerClasses =
      [ Css.c "absolute"
      , Css.c "flex"
      , Css.c "flex-col"
      , Css.c "bg-white"
      , Css.c "w-72"
      , Css.c "max-h-72"
      , Css.c "overflow-auto"
      , Css.c "border"
      , Css.c "rounded-md"
      ]

    infoClasses = containerClasses <> [ Css.c "p-2" ]

    loadingClasses = infoClasses <> [ Css.c "animate-pulse" ]

    renderResults :: H.ComponentHTML Action' () m
    renderResults
      | st.visibility == Sel.Off = HH.text ""
      | otherwise = case st.filtered of
        Idle -> HH.div [ HP.classes infoClasses ] [ HH.text "No active search …" ]
        Loading -> HH.div [ HP.classes loadingClasses ] [ HH.text "Loading search results …" ]
        Error msg -> HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ]
        Loaded [] -> HH.div [ HP.classes infoClasses ] [ HH.text "No matching value …" ]
        Loaded filtered ->
          HH.div (SelSet.setContainerProps [ HP.classes containerClasses ])
            $ A.mapWithIndex renderItem filtered

    renderItem idx (Tuple key _) =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes
                $ if st.highlightedIndex == Just idx then
                    selectedClasses
                  else
                    itemClasses
            ]
        )
        [ HH.text key ]
      where
      itemClasses = [ Css.c "p-2" ]

      selectedClasses = [ Css.c "p-2", Css.c "bg-snow-500" ]

filterAvailable ::
  forall f.
  Functor f =>
  String ->
  f (Array (Tuple String SS.ConfigValue)) ->
  f (Array (Tuple String SS.ConfigValue))
filterAvailable needle available =
  let
    pat = S.Pattern $ S.toLower needle

    containsNc = S.contains pat <<< S.toLower

    match (Tuple key _) = containsNc key
  in
    A.filter match <$> available
