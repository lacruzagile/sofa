module App.OrderForm.SelectLegalEntity (Slot, Output(..), proxy, component) where

import Prelude
import App.Requests (getLegalEntities)
import Component.Typeahead as Typeahead
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import HtmlUtils (focusElementByRef)
import Select as Sel
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLInputElement as HTMLInputElement

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "selectLegalEntity"
proxy = Proxy

type Output
  = Maybe SS.LegalEntity

type State
  = ( selected :: Maybe SS.LegalEntity
    , filtered :: Loadable (Array SS.LegalEntity)
    , available :: Loadable (Array SS.LegalEntity)
    )

data Action
  = Initialize

component ::
  forall query input m.
  MonadAff m => CredentialStore m => H.Component query input Output m
component =
  H.mkComponent
    { initialState: identity
    , render: \st -> HH.slot selectLabel unit selectComponent st identity
    , eval: H.mkEval H.defaultEval { handleAction = H.raise }
    }
  where
  selectLabel = Proxy :: Proxy "select"

selectComponent ::
  forall query input m.
  MonadAff m =>
  CredentialStore m =>
  H.Component (Sel.Query query ()) input Output m
selectComponent =
  Sel.component (const input)
    $ Sel.defaultSpec
        { handleAction = handleAction
        , handleEvent = handleEvent
        , render = render
        , initialize = Just Initialize
        }
  where
  input :: Sel.Input State
  input =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds 50.0)
    , search: Nothing
    , getItemCount: maybe 0 A.length <<< Loadable.toMaybe <<< _.filtered
    , selected: Nothing
    , filtered: Idle
    , available: Idle
    }

  handleAction :: Action -> H.HalogenM (Sel.State State) (Sel.Action Action) () Output m Unit
  handleAction = case _ of
    Initialize -> do
      -- Focus the search box to allow immediate typing.
      focusElementByRef (H.RefLabel "select-input")

  handleEvent :: Sel.Event -> H.HalogenM (Sel.State State) (Sel.Action Action) () Output m Unit
  handleEvent = case _ of
    Sel.Searched _ -> do
      state <- H.get
      mAvailable <- case state.available of
        Loaded _ -> pure $ Just state.available
        Loading -> pure $ Nothing
        _ -> do
          H.modify_ $ \st -> st { available = Loading, filtered = Loading }
          H.lift $ Just <$> getLegalEntities
      case mAvailable of
        Nothing -> pure unit
        Just available ->
          H.modify_ \st ->
            st
              { available = available
              -- Update the array of filtered matches. Note, we don't filter
              -- using the string passed in `Sel.Searched` since it may be out
              -- of date at the time `getLegalEntities` finishes.
              , filtered =
                let
                  pat = S.Pattern $ S.toLower st.search

                  match (SS.LegalEntity le) = S.contains pat (S.toLower le.registeredName)
                in
                  A.filter match <$> available
              }
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { search = ""
            , selected =
              do
                filtered <- Loadable.toMaybe st.filtered
                filtered !! idx
            , filtered = st.available
            , visibility = Sel.Off
            }
      -- Clear the input element.
      inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
      for_ inputElement
        $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.setValue "")
        <<< HTMLInputElement.fromHTMLElement
      -- Let the parent component know about the new selection.
      H.raise st'.selected
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML _ () m
  render st =
    Typeahead.render
      $ (Typeahead.initState st)
          { selected = map (\(SS.LegalEntity { registeredName }) -> registeredName) st.selected
          , selectedIndex =
            do
              SS.LegalEntity { registeredName: selName } <- st.selected
              vals <- Loadable.toMaybe st.filtered
              A.findIndex (\(SS.LegalEntity { registeredName: name }) -> name == selName) vals
          , values =
            case st.filtered of
              Loaded filtered ->
                let
                  renderItem (SS.LegalEntity { registeredName }) = HH.text registeredName
                in
                  renderItem <$> filtered
              _ -> []
          , noSelectionText = "Type to search legal entity  …"
          , loading = Loadable.isLoading st.filtered
          }
