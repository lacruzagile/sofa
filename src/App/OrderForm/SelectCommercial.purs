module App.OrderForm.SelectCommercial (Slot, Query(..), Output(..), proxy, component) where

import Prelude
import App.Requests (getBillingAccount, getBillingAccounts)
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
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HtmlUtils (focusElementByQuery)
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
    , selectedFull :: Loadable SS.BillingAccount
    , filtered :: Loadable (Array SS.BillingAccount)
    , available :: Loadable (Array SS.BillingAccount)
    )

data Action
  = Initialize

type Action'
  = Sel.Action Action

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
          { initialize = Just Initialize
          , handleAction = handleAction
          , handleEvent = handleEvent
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
    , selectedFull: Idle
    , filtered: Idle
    , available: Idle
    }

  handleAction = case _ of
    Initialize -> focusElementByQuery "input#commercial-search"

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

  handleEvent = case _ of
    Sel.Searched _ -> do
      state <- H.get
      mAvailable <- case state.available of
        Loaded _ -> pure $ Just state.available
        Loading -> pure $ Nothing
        _ -> do
          H.modify_ $ \st -> st { available = Loading, filtered = Loading }
          H.lift $ Just <$> getBillingAccounts state.crmAccountId
      case mAvailable of
        Nothing -> pure unit
        Just available ->
          H.modify_ \st ->
            st
              { available = available
              -- Update the array of filtered matches. Note, we don't filter
              -- using the string passed in `Sel.Searched` since it may be out
              -- of date at the time `getBillingAccounts` finishes.
              , filtered = filterAvailable st.search available
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
        Just (SS.BillingAccount ba) ->
          H.lift
            $ getBillingAccount st'.crmAccountId ba.billingAccountId
        _ -> pure Idle
      case selectedFull of
        Error err -> H.liftEffect $ Console.error $ "When fetching commercial data: " <> err
        _ -> pure unit
      H.modify_ \st -> st { selectedFull = selectedFull }
      -- Let the parent component know about the new selection.
      H.raise
        $ (\(SS.BillingAccount { commercial }) -> commercial)
        <$> Loadable.toMaybe selectedFull
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML Action' () m
  render st = HH.span_ $ [ renderInput ] <> renderResults
    where
    renderInput :: H.ComponentHTML Action' () m
    renderInput =
      HH.input
        $ SelSet.setInputProps
            [ HP.id "commercial-search"
            , HP.classes
                [ Css.tw.w72
                , Css.tw.mr5
                , Css.tw.focusOutline
                , Css.tw.outline1
                , Css.tw.outlineGray300
                , Css.tw.placeholderItalic
                , Css.tw.roundedSm
                ]
            , HP.placeholder "Type to search billing account…"
            ]

    containerClasses =
      [ Css.tw.absolute
      , Css.tw.mt1
      , Css.tw.flex
      , Css.tw.flexCol
      , Css.tw.bgWhite
      , Css.tw.w72
      , Css.tw.maxH72
      , Css.tw.overflowAuto
      , Css.tw.border
      , Css.tw.roundedMd
      ]

    infoClasses = containerClasses <> [ Css.tw.p2 ]

    loadingClasses = infoClasses <> [ Css.tw.animatePulse ]

    renderResults :: Array (H.ComponentHTML Action' () m)
    renderResults
      | st.visibility == Sel.Off = case st.selectedFull of
        Loading -> [ HH.div [ HP.classes loadingClasses ] [ HH.text "Loading commercial …" ] ]
        _ -> []
      | otherwise = case st.filtered of
        Idle -> [ HH.div [ HP.classes infoClasses ] [ HH.text "No active search …" ] ]
        Loading -> [ HH.div [ HP.classes loadingClasses ] [ HH.text "Loading search results …" ] ]
        Error msg -> [ HH.div [ HP.classes infoClasses ] [ HH.text "Error: ", HH.text msg ] ]
        Loaded [] -> [ HH.div [ HP.classes infoClasses ] [ HH.text "No matching billing accounts …" ] ]
        Loaded filtered ->
          [ HH.div (SelSet.setContainerProps [ HP.classes containerClasses ])
              $ A.mapWithIndex renderItem filtered
          ]

    renderItem idx billingAccount =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes
                $ if st.highlightedIndex == Just idx then
                    selectedClasses
                  else
                    itemClasses
            ]
        )
        (renderSummary billingAccount)
      where
      itemClasses = [ Css.tw.p2 ]

      selectedClasses = [ Css.tw.p2, Css.tw.bgSky100 ]

    renderSummary (SS.BillingAccount ba) =
      [ HH.text ba.displayName
      , HH.text " "
      , HH.span [ HP.style "color:gray" ] [ HH.text ba.shortId ]
      ]

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
