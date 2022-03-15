module App.OrderForm.Widget.Dropdown (Slot, Output(..), proxy, component) where

import Prelude
import App.SchemaDataSource (DataSourceEnumResult)
import Css as Css
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HtmlUtils (setInputText)
import Select as Sel
import Select.Setters as SelSet
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
  = ( selected :: Maybe (Tuple String SS.ConfigValue)
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    )

data Action
  = Initialize

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
    { inputType: Sel.Toggle
    , debounceTime: Nothing
    , search: Nothing
    , getItemCount: getDataItemCount
    , selected: (\v -> Tuple "" v) <$> input.value
    , available: Idle
    , getEnumData: input.getEnumData
    }

  getDataItemCount st = maybe 0 A.length $ Loadable.toMaybe $ st.available

  handleAction = case _ of
    Initialize -> do
      state <-
        H.modify \st ->
          st { available = Loading }
      lAvailable <- H.lift $ state.getEnumData Nothing
      state' <-
        H.modify \st ->
          st
            { selected =
              do
                Tuple _ inputValue <- st.selected
                available <- Loadable.toMaybe lAvailable
                A.find (\(Tuple _ v) -> v == inputValue) available
            , available = lAvailable
            }
      -- Set the input element to the full selection key.
      case state'.selected of
        Nothing -> pure unit
        Just (Tuple key _) -> setInputText "select-input" key

  handleEvent = case _ of
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { visibility = Sel.Off
            , selected =
              do
                available <- Loadable.toMaybe st.available
                available !! idx
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
      HH.button
        ( SelSet.setToggleProps
            [ HP.classes [ Css.c "nectary-input", Css.c "nectary-dropdown-icon" ] ]
        )
        [ maybe
            (HH.span [ HP.class_ (Css.c "text-stormy-300") ] [ HH.text "Please choose" ])
            (HH.text <<< fst)
            st.selected
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
      | otherwise = case st.available of
        Idle -> HH.div [ HP.classes infoClasses ] [ HH.text "Waiting for load …" ]
        Loading -> HH.div [ HP.classes loadingClasses ] [ HH.text "Loading values …" ]
        Error msg -> HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ]
        Loaded [] -> HH.div [ HP.classes infoClasses ] [ HH.text "No matching value …" ]
        Loaded available ->
          HH.div (SelSet.setContainerProps [ HP.classes containerClasses ])
            $ A.mapWithIndex renderItem available

    renderItem :: Int -> Tuple String SS.ConfigValue -> H.ComponentHTML Action' () m
    renderItem idx (Tuple key _) =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes $ itemClasses <> selectedClasses <> highlightClasses
            ]
        )
        [ HH.text key ]
      where
      itemClasses = [ Css.c "p-2", Css.c "pr-8" ]

      highlightClasses
        | st.highlightedIndex == Just idx = [ Css.c "bg-snow-500" ]
        | otherwise = []

      selectedClasses
        | Just key == map fst st.selected = [ Css.c "nectary-icon-check" ]
        | otherwise = []
