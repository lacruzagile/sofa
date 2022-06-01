module Sofa.App.OrderForm.SelectBuyer (Slot, Output(..), proxy, component) where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Select as Sel
import Sofa.App.Requests (getBuyer, getBuyers)
import Sofa.Component.Spinner as Spinner
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

proxy :: Proxy "selectBuyer"
proxy = Proxy

type Output
  = Loadable SS.Buyer

type State
  = ( selected :: Maybe SS.Buyer -- ^ The chosen buyer, if any.
    , selectedFull :: Loadable SS.Buyer -- ^ The chosen buyer in full, if any.
    , available :: Loadable (Array SS.Buyer) -- ^ The available buyers.
    )

data Action
  = Initialize

type Action'
  = Sel.Action Action

component ::
  forall query input f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render: const $ HH.slot selectLabel unit selectComponent unit identity
    , eval: H.mkEval H.defaultEval { handleAction = H.raise }
    }
  where
  selectLabel = Proxy :: Proxy "select"

selectComponent ::
  forall query input f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component (Sel.Query query ()) input Output m
selectComponent =
  Sel.component (const input)
    $ Sel.defaultSpec
        { initialize = Just Initialize
        , handleAction = handleAction
        , handleEvent = handleEvent
        , render = render
        }
  where
  input :: Sel.Input State
  input =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds 500.0)
    , search: Nothing
    , getItemCount: maybe 0 A.length <<< Loadable.toMaybe <<< _.available
    , selected: Nothing
    , selectedFull: Idle
    , available: Idle
    }

  handleAction = case _ of
    Initialize -> focusElementByRef (H.RefLabel "select-input")

  handleEvent = case _ of
    Sel.Searched str ->
      when (S.length str >= 3)
        $ do
            H.modify_ _ { available = Loading }
            result <- H.lift $ getBuyers str
            H.modify_ _ { available = result }
            -- If the result is an error then we also propagate this to the
            -- parent.
            case result of
              Error msg -> H.raise $ Error msg
              _ -> pure unit
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { search = ""
            , selected = (_ !! idx) =<< Loadable.toMaybe st.available
            , selectedFull = Loading
            , visibility = Sel.Off
            }
      -- Clear the input element.
      inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
      for_ inputElement
        $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.setValue "")
        <<< HTMLInputElement.fromHTMLElement
      -- Fetch the full representation of the selected buyer, if possible.
      let
        mCrmAccountId = do
          (SS.Buyer buyer) <- st'.selected
          buyer.crmAccountId
      selectedFull <- case mCrmAccountId of
        Nothing -> pure $ Error "No CRM account ID"
        Just crmAccountId -> do
          full <- H.lift $ getBuyer crmAccountId
          H.modify_ \st -> st { selectedFull = full }
          pure full
      -- Let the parent component know about the new selection.
      H.raise selectedFull
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML _ () m
  render st = case st.selectedFull of
    Loading ->
      HH.div
        [ Css.classes
            [ "nectary-input"
            , "w-full"
            , "flex"
            , "items-center"
            , "space-x-3"
            ]
        ]
        [ HH.div [ Css.class_ "grow" ] [ HH.text "Loading customer …" ]
        , Spinner.render [ Css.c "my-4" ]
        ]
    _ ->
      Typeahead.render
        $ (Typeahead.initRenderState st)
            { selected = map (\(SS.Buyer { corporateName }) -> corporateName) st.selected
            , selectedIndex =
              do
                SS.Buyer { corporateName: selName } <- st.selected
                vals <- Loadable.toMaybe st.available
                A.findIndex (\(SS.Buyer { corporateName: name }) -> name == selName) vals
            , values =
              case st.available of
                Loaded available ->
                  let
                    renderItem (SS.Buyer buyer) =
                      HH.span_
                        [ HH.text buyer.corporateName
                        , HH.text " "
                        , HH.span
                            [ Css.class_ "text-gray-400" ]
                            [ HH.text $ maybe "(No CRM account ID)" unwrap buyer.crmAccountId ]
                        ]
                  in
                    renderItem <$> available
                _ -> []
            , noSelectionText = "Type to search customer  …"
            , loading = Loadable.isLoading st.available
            }
