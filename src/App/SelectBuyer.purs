module App.SelectBuyer (Slot, Output(..), proxy, component) where

import Prelude
import App.Requests (getBuyer, getBuyers)
import Css as Css
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec as SS
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Sel
import Select.Setters as SelSet
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLInputElement as HTMLInputElement

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "selectBuyer"
proxy = Proxy

type Output
  = Maybe SS.Buyer

type State
  = ( selected :: Maybe SS.Buyer -- ^ The chosen buyer, if any.
    , selectedFull :: Loadable SS.Buyer -- ^ The chosen buyer in full, if any.
    , available :: Loadable (Array SS.Buyer) -- ^ The available buyers.
    )

component ::
  forall query input m.
  MonadAff m => CredentialStore m => H.Component query input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render: const $ HH.slot selectLabel unit selectComponent unit identity
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
        { handleEvent = handleEvent
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

  handleEvent = case _ of
    Sel.Searched str ->
      when (S.length str >= 3)
        $ do
            H.modify_ _ { available = Loading }
            result <- H.lift $ getBuyers str
            H.modify_ _ { available = result }
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
      H.raise $ Loadable.toMaybe selectedFull
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML Sel.Action' () m
  render st = HH.div_ $ [ renderInput ] <> renderResults
    where
    renderInput :: H.ComponentHTML Sel.Action' () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.class_ Css.taInput
            , HP.placeholder "Type to search buyer…"
            ]

    renderResults :: Array (H.ComponentHTML Sel.Action' () m)
    renderResults
      | st.visibility == Sel.Off = case st.selectedFull of
        Error msg ->
          let
            renderError b =
              [ HH.text "Error loading "
              , HH.span_ $ renderBuyerSummary b
              , HH.text ": "
              , HH.text msg
              ]
          in
            [ HH.div_ $ maybe [] renderError st.selected ]
        Loading ->
          let
            renderLoading b = [ HH.text "Loading ", HH.span_ $ renderBuyerSummary b ]
          in
            [ HH.div_ $ maybe [] renderLoading st.selected ]
        _ -> []
      | otherwise = case st.available of
        Idle -> [ HH.div_ [ HH.text "No active search …" ] ]
        Loading -> [ HH.div_ [ HH.text "Loading search results …" ] ]
        Error msg -> [ HH.div_ [ HH.text "Error: ", HH.text msg ] ]
        Loaded [] -> [ HH.div_ [ HH.text "No matching buyers …" ] ]
        Loaded available ->
          [ HH.div (SelSet.setContainerProps [ HP.class_ Css.taContainer ])
              $ A.mapWithIndex renderItem available
          ]

    renderItem idx buyer =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes $ [ Css.taItem ]
                <> if st.highlightedIndex == Just idx then [ Css.taHighlight ] else []
            ]
        )
        (renderBuyerSummary buyer)

    renderBuyerSummary (SS.Buyer buyer) =
      [ HH.text buyer.corporateName
      , HH.text " "
      , HH.span
          [ HP.style "color:gray" ]
          [ HH.text $ maybe "(No CRM account ID)" unwrap buyer.crmAccountId ]
      ]
