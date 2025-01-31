module Sofa.App.OrderForm.Widget.Typeahead (Slot, Output(..), proxy, component) where

import Prelude

import Data.Array ((!!))
import Data.Array as A
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Sel
import Sofa.App.SchemaDataSource (DataSourceEnumResult)
import Sofa.Component.Typeahead as Typeahead
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (selectInputText, setInputText)
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
    , readOnly :: Boolean
    , required :: Boolean
    }

type Output
  = Maybe SS.ConfigValue

type State m
  = ( selected :: Maybe (Tuple String SS.ConfigValue)
    , filtered :: Loadable (Array (Tuple String SS.ConfigValue))
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , minInputLength :: Int
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    , readOnly :: Boolean
    , required :: Boolean
    )

data Action
  = Initialize
  | InputFocused

type Action'
  = Sel.Action Action

component ::
  forall query f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query (Input m) Output m
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
    , readOnly: input.readOnly
    , required: input.required
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
  render st
    | st.readOnly =
      HH.div
        [ Css.classes
            [ "w-90pur"
            ,"min-w-96"
            , "h-12"
            , "px-3"
            , "py-2"
            , "my-0.5"
            , "rounded"
            , "bg-snow-100"
            , "flex"
            , "items-center"
            ]
          , HP.title 
              $ fromMaybe "" do
                Tuple label _ <- st.selected
                pure label
        ]
        [ HH.text 
            $ fromMaybe "" do
                Tuple label _ <- st.selected
                pure label
        ]
    | otherwise =
      Typeahead.render
        $ (Typeahead.initRenderState st)
            { selected = map fst st.selected
            , selectedIndex =
              do
                Tuple _ selVal <- st.selected
                vals <- Loadable.toMaybe st.filtered
                A.findIndex (\(Tuple _ val) -> val == selVal) vals
            , values =
              case st.filtered of
                Loaded filtered ->
                  let
                    renderItem (Tuple key _) = HH.div [HP.title key] [HH.text key]
                  in
                    renderItem <$> filtered
                _ -> []
            , noSelectionText = "Type to search value …"
            , loading = Loadable.isLoading st.filtered
            , wrapperClasses = [ Css.c "inline-block", Css.c "w-90pur", Css.c "min-w-96" ]
            , onInputFocus = Just $ \_ -> InputFocused
            , required = st.required
            }

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
