module App.OrderForm.SelectBuyer (Slot, Output(..), proxy, component) where

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
import HtmlUtils (focusElementByQuery)
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

data Action
  = Initialize

type Action'
  = Sel.Action Action

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
    Initialize -> focusElementByQuery "input#buyer-search"

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

  render :: Sel.State State -> H.ComponentHTML Action' () m
  render st = HH.span_ $ [ renderInput ] <> renderResults
    where
    renderInput :: H.ComponentHTML Action' () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.type_ HP.InputText
            , HP.id "buyer-search"
            , HP.classes
                [ Css.c "w-72"
                , Css.c "mt-2"
                , Css.c "mr-5"
                , Css.c "focus-outline"
                , Css.c "outline-1"
                , Css.c "outline-gray-300"
                , Css.c "placeholder:italic"
                , Css.c "rounded-sm"
                ]
            , HP.placeholder "Type to search buyer…"
            ]

    containerClasses =
      [ Css.c "absolute"
      , Css.c "mt-1"
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

    renderResults :: Array (H.ComponentHTML Action' () m)
    renderResults
      | st.visibility == Sel.Off = case st.selectedFull of
        Loading -> [ HH.div [ HP.classes loadingClasses ] [ HH.text "Loading buyer …" ] ]
        _ -> []
      | otherwise = case st.available of
        Idle -> [ HH.div [ HP.classes infoClasses ] [ HH.text "No active search …" ] ]
        Loading -> [ HH.div [ HP.classes loadingClasses ] [ HH.text "Loading search results …" ] ]
        Error msg -> [ HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ] ]
        Loaded [] -> [ HH.div [ HP.classes infoClasses ] [ HH.text "No matching buyers …" ] ]
        Loaded available ->
          [ HH.div (SelSet.setContainerProps [ HP.classes containerClasses ])
              $ A.mapWithIndex renderItem available
          ]

    renderItem idx buyer =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes
                $ if st.highlightedIndex == Just idx then
                    selectedClasses
                  else
                    itemClasses
            ]
        )
        (renderBuyerSummary buyer)
      where
      itemClasses = [ Css.c "p-2" ]

      selectedClasses = [ Css.c "p-2", Css.c "bg-snow-500" ]

    renderBuyerSummary (SS.Buyer buyer) =
      [ HH.text buyer.corporateName
      , HH.text " "
      , HH.span
          [ HP.class_ (Css.c "text-gray-400") ]
          [ HH.text $ maybe "(No CRM account ID)" unwrap buyer.crmAccountId ]
      ]
