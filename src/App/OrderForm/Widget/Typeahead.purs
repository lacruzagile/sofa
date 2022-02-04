module App.OrderForm.Widget.Typeahead (Slot, Output(..), proxy, component) where

import Prelude
import App.Requests (getDataSourceEnum)
import Css as Css
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Int as Int
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Map as Map
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

type Input
  = { value :: Maybe SS.ConfigValue
    , minInputLength :: Int
    , debounceMs :: Int
    , dataSource :: SS.SchemaDataSourceEnum
    }

type Output
  = Maybe SS.ConfigValue

type State
  = ( selected :: Maybe (Tuple String SS.ConfigValue)
    , filtered :: Loadable (Array (Tuple String SS.ConfigValue))
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , minInputLength :: Int
    , dataSource :: SS.SchemaDataSourceEnum
    )

data Action
  = Initialize
  | InputFocused

type Action'
  = Sel.Action Action

component ::
  forall query m.
  MonadAff m => CredentialStore m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render: \st -> HH.slot selectLabel unit selectComponent st identity
    , eval: H.mkEval H.defaultEval { handleAction = H.raise }
    }
  where
  selectLabel = Proxy :: Proxy "select"

  selectComponent :: H.Component (Sel.Query query ()) Input Output m
  selectComponent =
    Sel.component mapInput
      $ Sel.defaultSpec
          { initialize = Just Initialize
          , handleAction = handleAction
          , handleEvent = handleEvent
          , render = render
          }

  mapInput :: Input -> Sel.Input State
  mapInput input =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds $ Int.toNumber input.debounceMs)
    , search: Nothing
    , getItemCount: getDataItemCount
    , selected: (\v -> Tuple "" v) <$> input.value
    , filtered: Idle
    , available: Idle
    , minInputLength: input.minInputLength
    , dataSource: input.dataSource
    }

  getDataItemCount st = maybe 0 A.length $ Loadable.toMaybe $ st.filtered

  handleAction = case _ of
    Initialize -> do
      state <-
        H.modify \st ->
          st { filtered = Loading, available = Loading }
      state' <- case state.dataSource of
        SS.SdsEnumMap { entries } ->
          let
            available = Map.toUnfoldable entries :: Array (Tuple String SS.ConfigValue)
          in
            H.modify \st ->
              st
                { selected =
                  do
                    Tuple _ inputValue <- st.selected
                    A.find (\(Tuple _ v) -> v == inputValue) available
                , filtered = Loaded available
                , available = Loaded available
                }
        SS.SdsEnumHttpGet { url } -> do
          result <- H.lift $ getDataSourceEnum url
          H.modify \st ->
            st
              { selected =
                do
                  Tuple _ inputValue <- st.selected
                  available <- Loadable.toMaybe result
                  A.find (\(Tuple _ v) -> v == inputValue) available
              , filtered = result
              , available = result
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

  render :: Sel.State State -> H.ComponentHTML Action' () m
  render st = HH.div [ HP.class_ Css.tw.inlineBlock ] [ renderInput, renderResults ]
    where
    renderInput :: H.ComponentHTML Action' () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.classes [ Css.tw.border ]
            , HP.placeholder "Type to search value…"
            , HE.onFocus \_ -> Sel.Action InputFocused
            ]

    containerClasses =
      [ Css.tw.absolute
      , Css.tw.mt1
      , Css.tw.flex
      , Css.tw.flexCol
      , Css.tw.bgWhite
      , Css.tw.w72
      , Css.tw.maxH72
      , Css.tw.overflowAuto
      , Css.tw.border
      , Css.tw.roundedMd
      ]

    infoClasses = containerClasses <> [ Css.tw.p2 ]

    loadingClasses = infoClasses <> [ Css.tw.animatePulse ]

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
      itemClasses = [ Css.tw.p2 ]

      selectedClasses = [ Css.tw.p2, Css.tw.bgSky100 ]

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
