module Sofa.App.OrderForm.SelectLegalEntity (Slot, Output(..), proxy, component) where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Select as Sel
import Sofa.App.Requests (getLegalEntities)
import Sofa.Component.Typeahead as Typeahead
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (focusElementByRef)
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
      H.modify_ _ { selected = Nothing, available = Loading, filtered = Loading }
      result <- H.lift $ getLegalEntities
      H.modify_ _ { available = result, filtered = result }

  handleEvent :: Sel.Event -> H.HalogenM (Sel.State State) (Sel.Action Action) () Output m Unit
  handleEvent = case _ of
    Sel.Searched str -> do
      H.modify_ \st ->
        st
          { filtered =
            let
              pat = S.Pattern $ S.toLower str

              match (SS.LegalEntity le) = S.contains pat (S.toLower le.registeredName)
            in
              A.filter match <$> st.available
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
      $ (Typeahead.initRenderState st)
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
          , wrapperClasses = [ Css.c "min-w-96" ]
          , noSelectionText = "Type to search legal entity  …"
          , loading = Loadable.isLoading st.filtered
          }
