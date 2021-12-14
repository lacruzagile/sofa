module App.SelectCommercial (Slot, Query(..), Output(..), proxy, component) where

import Prelude
import App.Requests (getBillingAccount, getBillingAccounts)
import Css as Css
import Data.Array ((!!))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
  = H.Slot Query Output id

proxy :: Proxy "selectCommercial"
proxy = Proxy

type Input
  = SS.CrmAccountId

type Output
  = Maybe SS.Commercial

data Query a
  = SetCrmAccountId SS.CrmAccountId a

type State
  = ( crmAccountId :: SS.CrmAccountId
    , selected :: Maybe SS.BillingAccount
    , filtered :: Loadable (Array SS.BillingAccount)
    , available :: Loadable (Array SS.BillingAccount)
    )

component ::
  forall m.
  MonadAff m => CredentialStore m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render: \st -> HH.slot selectLabel unit selectComponent st identity
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = H.raise
            , handleQuery =
              case _ of
                SetCrmAccountId crmAccountId next -> do
                  H.tell selectLabel unit (Sel.Query <<< SetCrmAccountId crmAccountId)
                  pure $ Just next
            }
    }
  where
  selectLabel = Proxy :: Proxy "select"

  selectComponent :: H.Component (Sel.Query Query ()) Input Output m
  selectComponent =
    Sel.component input
      $ Sel.defaultSpec
          { handleEvent = handleEvent
          , handleQuery = handleQuery
          , render = render
          }

  input :: SS.CrmAccountId -> Sel.Input State
  input crmAccountId =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds 50.0)
    , search: Nothing
    , getItemCount: maybe 0 A.length <<< Loadable.toMaybe <<< _.filtered
    , crmAccountId: crmAccountId
    , selected: Nothing
    , filtered: Idle
    , available: Idle
    }

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCrmAccountId crmAccountId next -> do
      H.modify_
        _
          { crmAccountId = crmAccountId
          , selected = Nothing
          , available = Idle
          , filtered = Idle
          }
      pure (Just next)

  handleEvent = case _ of
    Sel.Searched str -> do
      state <- H.get
      available <- case state.available of
        Loaded _ -> pure state.available
        Loading -> pure state.available
        _ -> do
          H.modify_ $ \st -> st { available = Loading, filtered = Loading }
          H.lift $ getBillingAccounts state.crmAccountId
      H.modify_ \st ->
        st
          { available = available
          , filtered =
            let
              pat = S.Pattern $ S.toLower str

              containsNc = S.contains pat <<< S.toLower

              match (SS.BillingAccount ba) =
                containsNc ba.displayName
                  || containsNc ba.shortId
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
      -- Fetch the full representation of the selected billing account, if possible.
      selected <-
        fromMaybe (pure Nothing)
          $ do
              (SS.BillingAccount ba) <- st'.selected
              let
                baId = ba.billingAccountId
              pure $ H.lift $ Loadable.toMaybe <$> getBillingAccount st'.crmAccountId baId
      -- Let the parent component know about the new selection.
      H.raise $ (\(SS.BillingAccount { commercial }) -> commercial) <$> selected
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML _ () m
  render st = HH.div_ $ [ renderInput ] <> renderSelected <> renderResults
    where
    renderInput :: H.ComponentHTML _ () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.class_ Css.taInput
            , HP.placeholder "Type to search billing account…"
            ]

    renderSelected :: Array (H.ComponentHTML _ () m)
    renderSelected
      | st.visibility == Sel.On = []
      | otherwise = case st.selected of
        Nothing -> [ HH.div_ [ HH.text "No billing account selected" ] ]
        Just billingAccount -> [ HH.div_ (renderSummary billingAccount) ]

    renderResults :: Array (H.ComponentHTML _ () m)
    renderResults
      | st.visibility == Sel.Off = []
      | otherwise = case st.filtered of
        Idle -> [ HH.div_ [ HH.text "No active search …" ] ]
        Loading -> [ HH.div_ [ HH.text "Loading search results …" ] ]
        Error msg -> [ HH.div_ [ HH.text "Error: ", HH.text msg ] ]
        Loaded [] -> [ HH.div_ [ HH.text "No matching billing accounts …" ] ]
        Loaded filtered ->
          [ HH.div (SelSet.setContainerProps [ HP.class_ Css.taContainer ])
              $ A.mapWithIndex renderItem filtered
          ]

    renderItem idx billingAccount =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes $ [ Css.taItem ]
                <> if st.highlightedIndex == Just idx then [ Css.taHighlight ] else []
            ]
        )
        (renderSummary billingAccount)

    renderSummary (SS.BillingAccount ba) =
      [ HH.text ba.displayName
      , HH.text " "
      , HH.span [ HP.style "color:gray" ] [ HH.text ba.shortId ]
      ]
