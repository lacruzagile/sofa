module App.OrderForm.SelectLegalEntity (Slot, Output(..), proxy, component) where

import Prelude
import App.Requests (getLegalEntities)
import Css as Css
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
import Halogen.HTML.Properties as HP
import HtmlUtils (focusElementByQuery)
import Select as Sel
import Select.Setters as SelSet
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
  = Load

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
        , initialize = Just Load
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
    Load -> do
      -- Focus the search box to allow immediate typing.
      focusElementByQuery "input#le-search"
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

  render :: Sel.State State -> H.ComponentHTML (Sel.Action Action) () m
  render st = HH.span_ $ [ renderInput ] <> renderResults
    where
    renderInput :: H.ComponentHTML (Sel.Action Action) () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.type_ HP.InputText
            , HP.id "le-search"
            , HP.classes
                [ Css.c "w-72"
                , Css.c "mr-5"
                , Css.c "focus-outline"
                , Css.c "outline-1"
                , Css.c "outline-gray-300"
                , Css.c "placeholder:italic"
                , Css.c "rounded-sm"
                ]
            , HP.placeholder "Type to search legal entity…"
            ]

    renderResults :: Array (H.ComponentHTML (Sel.Action Action) () m)
    renderResults
      | st.visibility == Sel.Off = []
      | otherwise = case st.filtered of
        Idle -> [ HH.div [ HP.classes infoClasses ] [ HH.text "No active search …" ] ]
        Loading -> [ HH.div [ HP.classes infoClasses ] [ HH.text "Loading search results …" ] ]
        Error msg -> [ HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ] ]
        Loaded [] -> [ HH.div [ HP.classes infoClasses ] [ HH.text "No matching legal entities …" ] ]
        Loaded filtered ->
          [ HH.div (SelSet.setContainerProps [ HP.classes containerClasses ])
              $ A.mapWithIndex renderItem filtered
          ]
        where
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

    renderItem idx legalEntity =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes
                $ if st.highlightedIndex == Just idx then
                    selectedClasses
                  else
                    itemClasses
            ]
        )
        (renderSummary legalEntity)
      where
      itemClasses = [ Css.c "p-2" ]

      selectedClasses = [ Css.c "p-2", Css.c "bg-snow-500" ]

    renderSummary (SS.LegalEntity le) = [ HH.text le.registeredName ]
