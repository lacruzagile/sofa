module Sofa.App.OrderForm.SelectCommercial (Slot, Query(..), Output(..), proxy, component) where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Select as Sel
import Sofa.App.Requests (getBillingAccount, getBillingAccounts)
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
  = H.Slot Query Output id

proxy :: Proxy "selectCommercial"
proxy = Proxy

type Input
  = { crmAccountId :: SS.CrmAccountId
    , billingAccountId :: Maybe SS.BillingAccountId
    -- ^ The selected billing account, if available.
    }

type Output
  = Loadable SS.Commercial

data Query a
  = SetCrmAccountId SS.CrmAccountId a

type State
  = ( crmAccountId :: SS.CrmAccountId
    , selected :: Maybe SS.BillingAccountId
    , selectedFull :: Loadable SS.BillingAccount
    , filtered :: Loadable (Array SS.BillingAccount)
    , available :: Loadable (Array SS.BillingAccount)
    )

data Action
  = Initialize

type Action'
  = Sel.Action Action

component ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component Query Input Output m
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
    Sel.component handleInput
      $ Sel.defaultSpec
          { initialize = Just Initialize
          , handleAction = handleAction
          , handleEvent = handleEvent
          , handleQuery = handleQuery
          , render = render
          }

  handleInput :: Input -> Sel.Input State
  handleInput input =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds 50.0)
    , search: Nothing
    , getItemCount: maybe 0 A.length <<< Loadable.toMaybe <<< _.filtered
    , crmAccountId: input.crmAccountId
    , selected: input.billingAccountId
    , selectedFull: Idle
    , filtered: Idle
    , available: Idle
    }

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.modify _ { available = Loading, filtered = Loading }
      result <- H.lift $ getBillingAccounts state.crmAccountId
      H.modify_ _ { available = result, filtered = result }
      -- If the result is an error then we also propagate this to the parent.
      case result of
        Error msg -> H.raise $ Error msg
        _ -> pure unit
      -- Focus the input if we don't have any selection.
      when (isNothing state.selected)
        $ focusElementByRef (H.RefLabel "select-input")

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCrmAccountId crmAccountId next -> do
      H.modify_
        _
          { crmAccountId = crmAccountId
          , selected = Nothing
          , selectedFull = Idle
          , available = Idle
          , filtered = Idle
          }
      pure (Just next)

  handleEvent :: Sel.Event -> H.HalogenM _ _ _ _ _ Unit
  handleEvent = case _ of
    Sel.Searched str -> do
      H.modify_ \st -> st { filtered = filterAvailable str st.available }
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { search = ""
            , selected =
              do
                filtered <- Loadable.toMaybe st.filtered
                SS.BillingAccount { billingAccountId } <- filtered !! idx
                pure billingAccountId
            , selectedFull = Loading
            , filtered = st.available
            , visibility = Sel.Off
            }
      -- Clear the input element.
      inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
      for_ inputElement
        $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.setValue "")
        <<< HTMLInputElement.fromHTMLElement
      -- Fetch the full representation of the selected billing account, if
      -- possible.
      selectedFull <- case st'.selected of
        Just billingAccountId ->
          H.lift
            $ getBillingAccount st'.crmAccountId billingAccountId
        _ -> pure Idle
      case selectedFull of
        Error err -> H.liftEffect $ Console.error $ "When fetching commercial data: " <> err
        _ -> pure unit
      H.modify_ \st -> st { selectedFull = selectedFull }
      -- Let the parent component know about the new selection.
      H.raise
        $ (\(SS.BillingAccount { commercial }) -> commercial)
        <$> selectedFull
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML Action' () m
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
        [ HH.div [ Css.class_ "grow" ] [ HH.text "Loading billing account …" ]
        , Spinner.render [ Css.c "my-4" ]
        ]
    _ ->
      let
        selected = do
          selectedId <- st.selected
          vals <- Loadable.toMaybe st.available
          index <- A.findIndex (\(SS.BillingAccount ba) -> ba.billingAccountId == selectedId) vals
          value <- A.index vals index
          pure $ { index, value }
      in
        Typeahead.render
          $ (Typeahead.initRenderState st)
              { selected =
                do
                  SS.BillingAccount { displayName } <- _.value <$> selected
                  pure displayName
              , selectedIndex = _.index <$> selected
              , values =
                case st.filtered of
                  Loaded filtered ->
                    let
                      renderItem (SS.BillingAccount ba) =
                        HH.span_
                          [ HH.text ba.displayName
                          , HH.text " "
                          , HH.span [ Css.class_ "text-stormy-300" ] [ HH.text ba.shortId ]
                          ]
                    in
                      renderItem <$> filtered
                  _ -> []
              , noSelectionText = "Type to search billing account  …"
              , loading = Loadable.isLoading st.available
              }

filterAvailable ::
  forall f.
  Functor f =>
  String ->
  f (Array SS.BillingAccount) ->
  f (Array SS.BillingAccount)
filterAvailable needle available =
  let
    -- The string we're searching for. Note, we don't use the string passed in
    -- `Sel.Searched` since it may be out of date at the time
    -- `getBillingAccounts` finishes.
    pat = S.Pattern $ S.toLower needle

    containsNc = S.contains pat <<< S.toLower

    match (SS.BillingAccount ba) =
      containsNc ba.displayName
        || containsNc ba.shortId
  in
    A.filter match <$> available
