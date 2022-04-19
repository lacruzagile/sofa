module Sofa.App.OrderForm.SelectProduct (Slot, Output(..), proxy, component) where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Sel
import Sofa.Component.Typeahead as Typeahead
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.SmartSpec as SS
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

              containsPat t = S.contains pat (S.toLower t)

              match (SS.Product { sku, title }) =
                containsPat (show sku)
                  || maybe false containsPat title
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
  render st =
    Typeahead.render
      $ (Typeahead.initRenderState st)
          { selected = map (\(SS.Product { sku }) -> show sku) st.selected
          , selectedIndex =
            do
              SS.Product { sku: selSku } <- st.selected
              A.findIndex (\(SS.Product { sku }) -> sku == selSku) st.filtered
          , values = renderItem <$> st.filtered
          , noSelectionText = "Type to search product …"
          , wrapperClasses = [ Css.c "w-96" ]
          }

  renderItem :: SS.Product -> HH.PlainHTML
  renderItem (SS.Product { sku, title }) = case title of
    Nothing -> HH.text (show sku)
    Just t ->
      HH.div_
        [ HH.text t
        , HH.br_
        , HH.span
            [ HP.classes [ Css.c "text-xs", Css.c "text-stormy-300" ] ]
            [ HH.text (show sku) ]
        ]
