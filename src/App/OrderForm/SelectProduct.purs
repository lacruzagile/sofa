module App.OrderForm.SelectProduct (Slot, Output(..), proxy, component) where

import Prelude
import Css as Css
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Maybe (Maybe(..), maybe, maybe')
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

proxy :: Proxy "selectProduct"
proxy = Proxy

type Input
  = Array SS.Product

type Output
  = SS.Product

type State
  = ( selected :: Maybe SS.Product
    , filtered :: Array SS.Product
    , available :: Array SS.Product
    )

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

selectComponent :: forall query m. MonadAff m => H.Component (Sel.Query query ()) Input Output m
selectComponent =
  Sel.component input
    $ Sel.defaultSpec
        { handleEvent = handleEvent
        , render = render
        }
  where
  input :: Input -> Sel.Input State
  input products =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds 50.0)
    , search: Nothing
    , getItemCount: A.length <<< _.filtered
    , selected: Nothing
    , filtered: products
    , available: products
    }

  handleEvent :: Sel.Event -> H.HalogenM (Sel.State State) _ () Output m Unit
  handleEvent = case _ of
    Sel.Searched str -> do
      H.modify_ \st ->
        st
          { filtered =
            let
              pat = S.Pattern $ S.toLower str

              match (SS.Product { sku }) = S.contains pat (S.toLower (show sku))
            in
              A.filter match st.available
          }
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { search = ""
            , selected = st.filtered !! idx
            , filtered = st.available
            , visibility = Sel.Off
            }
      -- Clear the input element.
      inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
      for_ inputElement
        $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.setValue "")
        <<< HTMLInputElement.fromHTMLElement
      -- Let the parent component know about the new selection.
      maybe' pure H.raise st'.selected
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML _ () m
  render st = HH.span_ $ [ renderInput ] <> renderResults
    where
    renderInput :: H.ComponentHTML _ () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.type_ HP.InputText
            , HP.classes
                [ Css.c "w-96"
                , Css.c "mr-5"
                , Css.c "text-lg"
                , Css.c "focus-outline"
                , Css.c "outline-1"
                , Css.c "outline-gray-300"
                , Css.c "placeholder:italic"
                , Css.c "rounded-sm"
                ]
            , HP.placeholder "Type to search product…"
            ]

    renderResults :: Array (H.ComponentHTML _ () m)
    renderResults
      | st.visibility == Sel.Off = []
      | otherwise = case st.filtered of
        [] -> [ HH.div_ [ HH.text "No matching legal entities …" ] ]
        filtered ->
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
          , Css.c "w-96"
          , Css.c "max-h-96"
          , Css.c "overflow-auto"
          , Css.c "border"
          , Css.c "rounded-md"
          ]

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

    renderSummary (SS.Product { sku }) = [ HH.text (show sku) ]
