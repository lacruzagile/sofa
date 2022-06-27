module Sofa.App.OrderForm.Widget.Dropdown (Slot, Output(..), proxy, component) where

import Prelude
import Control.Alternative ((<|>))
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Tuple (Tuple(..), fst)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Select as Sel
import Sofa.App.SchemaDataSource (DataSourceEnumResult)
import Sofa.Component.Select as Select
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (setInputText)
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetDropdown"
proxy = Proxy

type Input m
  = { value :: Maybe SS.ConfigValue
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    , readOnly :: Boolean
    }

type Output
  = Maybe SS.ConfigValue

type State m
  = ( selectedIndex :: Maybe Int
    , selectedValue :: Maybe SS.ConfigValue
    , available :: Loadable (Array (Tuple String SS.ConfigValue))
    , getEnumData :: Maybe String -> m DataSourceEnumResult
    , readOnly :: Boolean
    )

data Action
  = Initialize

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
          , render = renderSelect
          }

  mapInput :: (Input m) -> Sel.Input (State m)
  mapInput input =
    { inputType: Sel.Toggle
    , debounceTime: Nothing
    , search: Nothing
    , getItemCount: getDataItemCount
    , selectedIndex: Nothing
    , selectedValue: input.value
    , available: Idle
    , getEnumData: input.getEnumData
    , readOnly: input.readOnly
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
            { selectedIndex =
              let
                selectSelected = do
                  inputValue <- st.selectedValue
                  available <- Loadable.toMaybe lAvailable
                  A.findIndex (\(Tuple _ v) -> v == inputValue) available

                selectOnly = do
                  available <- Loadable.toMaybe lAvailable
                  if A.length available == 1 then Just 0 else Nothing
              in
                selectSelected <|> selectOnly
            , available = lAvailable
            }
      -- Set the input element to the full selection key.
      maybe' pure (setInputText "select-input") do
        idx <- state'.selectedIndex
        available <- Loadable.toMaybe state'.available
        Tuple key _ <- available !! idx
        pure key

  handleEvent = case _ of
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { visibility = Sel.Off
            , selectedIndex = Just idx
            , selectedValue =
              do
                available <- Loadable.toMaybe st.available
                Tuple _ value <- available !! idx
                pure value
            }
      -- Set the input element to the full selection key.
      maybe' pure (setInputText "select-input") do
        available <- Loadable.toMaybe st'.available
        Tuple key _ <- available !! idx
        pure key
      -- Let the parent component know about the new selection.
      H.raise st'.selectedValue
    _ -> pure unit

  renderSelect :: Sel.State (State m) -> H.ComponentHTML Action' () m
  renderSelect st
    | st.readOnly =
      HH.div
        [ Css.classes
            [ "w-96"
            , "h-12"
            , "px-3"
            , "py-2"
            , "my-0.5"
            , "rounded"
            , "bg-snow-100"
            , "flex"
            , "items-center"
            ]
        ]
        [ HH.text
            $ fromMaybe "" do
                selectedIndex <- st.selectedIndex
                available <- Loadable.toMaybe st.available
                Tuple label _ <- A.index available selectedIndex
                pure label
        ]
    | otherwise =
      Select.render
        $ (Select.initRenderState st)
            { selectedIndex = st.selectedIndex
            , values =
              case Loadable.toMaybe st.available of
                Nothing -> []
                Just available -> map (HH.text <<< fst) available
            , loading = Loadable.isLoading st.available
            , wrapperClasses = [ Css.c "w-96" ]
            }
