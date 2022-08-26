-- | The commercial component of the order form.
module Sofa.App.OrderForm.Commercial (Slot, Input(..), Output(..), Query(..), proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm.SelectCommercial as SelectCommercial
import Sofa.App.Requests as Requests
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..), isLoaded)
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (focusElementByRef)
import Type.Proxy (Proxy(..))

type Slot id
  = H.Slot Query Output id

proxy :: Proxy "commercial"
proxy = Proxy

type Slots
  = ( selectCommercial :: SelectCommercial.Slot Unit )

data Input
  = InputIds
    { billingAccountId :: SS.BillingAccountId
    , crmAccountId :: SS.CrmAccountId
    , readOnly :: Boolean
    }
  | InputCommercial
    { commercial :: SS.Commercial
    , crmAccountId :: SS.CrmAccountId
    , readOnly :: Boolean
    }
  | InputNothing

type Output
  = SS.BillingAccount

type State
  = { commercial :: Loadable SS.Commercial -- ^ The currently chosen commercial.
    , billingAccount :: Loadable SS.BillingAccount
    -- ^ The currently chosen billing account.
    , acceptedCommercial :: Loadable SS.Commercial -- ^ The latest accepted commercial.
    , acceptedBillingAccount :: Loadable SS.BillingAccount -- ^ The latest accepted billing account.
    , crmAccountId :: Maybe SS.CrmAccountId
    , billingAccountId :: Maybe SS.BillingAccountId
    , readOnly :: Boolean
    , enabled :: Boolean
    , open :: Boolean -- ^ Whether the details modal is open.
    }

data Action
  = Initialize
  | ChooseCommercial (Loadable SS.BillingAccount)
  | OpenDetails
  | AcceptAndCloseDetails
  | CancelAndCloseDetails

data Query a
  = ResetCommercial
    { commercial :: Maybe SS.Commercial
    , crmAccountId :: Maybe SS.CrmAccountId
    , enabled :: Boolean
    }
    a
  | ResetCommercialFixed
    { commercial :: Maybe SS.Commercial
    , crmAccountId :: Maybe SS.CrmAccountId
    , enabled :: Boolean
    , open :: Boolean
    }
    a

component ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { initialize = Just Initialize
            , handleAction = handleAction
            , handleQuery = handleQuery
            }
    }

initialState :: Input -> State
initialState input = case input of
  InputIds { billingAccountId, crmAccountId, readOnly } ->
    { commercial: Loading
    , billingAccount: Loading
    , acceptedCommercial: Loading
    , acceptedBillingAccount: Loading
    , crmAccountId: Just crmAccountId
    , billingAccountId: Just billingAccountId
    , readOnly
    , enabled: true
    , open: false
    }
  InputCommercial { commercial, crmAccountId, readOnly } ->
    { commercial: Loaded commercial
    , billingAccount: Loading
    , acceptedCommercial: Loaded commercial
    , acceptedBillingAccount: Loading
    , crmAccountId: Just crmAccountId
    , billingAccountId: let SS.Commercial { billingAccountId } = commercial in billingAccountId
    , readOnly
    , enabled: true
    , open: false
    }
  InputNothing ->
    { commercial: Idle
    , billingAccount: Idle
    , acceptedCommercial: Idle
    , acceptedBillingAccount: Idle
    , crmAccountId: Nothing
    , billingAccountId: Nothing
    , readOnly: false
    , enabled: false
    , open: false
    }

okBtnLabel :: H.RefLabel
okBtnLabel = H.RefLabel "ok-btn"

render ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action Slots m
render state
  | state.open = renderDetails state
  | otherwise = renderSummary state

renderBillingOption :: forall slot action. SS.Commercial -> HH.HTML slot action
renderBillingOption (SS.Commercial { billingOption }) =
  HH.text
    $ case billingOption of
        SS.Prepay -> "Prepay"
        SS.Postpay -> "Postpay"

renderContractTerm :: forall slot action. SS.Commercial -> HH.HTML slot action
renderContractTerm (SS.Commercial { contractTerm }) =
  HH.text
    $ case contractTerm of
        SS.Ongoing -> "Ongoing"
        SS.Fixed -> "Fixed"

renderBillingCurrency :: forall slot action. SS.Commercial -> HH.HTML slot action
renderBillingCurrency (SS.Commercial { billingCurrency }) = HH.text $ show billingCurrency

renderPaymentCurrency :: forall slot action. SS.Commercial -> HH.HTML slot action
renderPaymentCurrency (SS.Commercial { paymentCurrency }) = HH.text $ show paymentCurrency

renderSummary :: forall slots m. State -> H.ComponentHTML Action slots m
renderSummary st
  | not st.enabled =
    HH.div
      [ Css.class_ "text-gray-400" ]
      [ HH.text "Not available" ]
  | otherwise = case st.acceptedBillingAccount of
    Loaded (SS.BillingAccount ba) ->
      btn okClasses
        [ HH.text ba.displayName
        , subtleSlash
        , HH.text ba.shortId
        ]
    Loading -> HH.div_ [ HH.text "Loading …" ]
    _ -> btn badClasses [ HH.text "Select …" ]
    where
    btn classes = HH.button [ HP.classes classes, HE.onClick $ \_ -> OpenDetails ]

    subtleSlash = HH.span [ Css.class_ "text-gray-400" ] [ HH.text " / " ]

    okClasses =
      Css.cs
        [ "block"
        , "text-left"
        , "underline"
        , "underline-offset-4"
        , "decoration-honey-500"
        ]

    badClasses =
      Css.cs
        [ "block"
        , "text-left"
        , "underline"
        , "underline-offset-4"
        , "decoration-honey-500"
        ]

renderDetails ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action Slots m
renderDetails st =
  HH.div_
    [ renderSummary st
    , Modal.render
        $ Modal.defaultInput
            { title = HH.text "Commercial"
            , closeAction = Just (const CancelAndCloseDetails)
            , content = renderContent st.billingAccount
            }
    ]
  where
  renderContent billingAccountLoadable =
    HH.div [ Css.classes [ "flex", "flex-col", "gap-y-4" ] ]
      [ case st.crmAccountId of
          Just crmAccountId
            | not st.readOnly ->
              HH.slot
                SelectCommercial.proxy
                unit
                SelectCommercial.component
                { crmAccountId
                , billingAccountId: st.billingAccountId
                }
                ChooseCommercial
          _ -> HH.text ""
      , case billingAccountLoadable of
          Error err ->
            HH.p
              [ Css.classes
                  [ "p-3"
                  , "bg-red-100"
                  , "border"
                  , "border-red-400"
                  , "text-raspberry-500"
                  ]
              ]
              [ HH.text err ]
          _ -> HH.text ""
      , HH.div
          [ Css.classes
              [ "w-full"
              , "min-w-96"
              , "p-8"
              , "grid"
              , "grid-cols-[12rem_auto]"
              , "gap-4"
              , "rounded"
              , "bg-snow-500"
              ]
          ]
          $ [ renderSmallTitle "Billing Option"
            , HH.div_ [ maybe empty renderBillingOption mCommercial ]
            , renderSmallTitle "Contract Term"
            , HH.div_ [ maybe empty renderContractTerm mCommercial ]
            , renderSmallTitle "Payment Currency"
            , HH.div_ [ maybe empty renderPaymentCurrency mCommercial ]
            , renderSmallTitle "Billing Currency"
            , HH.div_ [ maybe empty renderBillingCurrency mCommercial ]
            ]
      , HH.div [ Css.classes [ "flex", "space-x-5" ] ] bottomButtons
      ]
    where
    mCommercial = case billingAccountLoadable of
      Loaded (SS.BillingAccount { commercial }) -> Just commercial
      _ -> Nothing

    empty = HH.text ""

    bottomButtons
      | st.readOnly =
        [ HH.div [ Css.class_ "grow" ] []
        , HH.button
            [ Css.class_ "nectary-btn-primary"
            , HE.onClick \_ -> CancelAndCloseDetails
            ]
            [ HH.text "Close" ]
        ]
      | otherwise =
        [ HH.div [ Css.class_ "grow" ] []
        , HH.button
            [ Css.class_ "nectary-btn-secondary"
            , HE.onClick \_ -> CancelAndCloseDetails
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.ref okBtnLabel
            , Css.class_ "nectary-btn-primary"
            , HP.enabled (isLoaded st.billingAccount)
            , HE.onClick \_ -> AcceptAndCloseDetails
            ]
            [ HH.text "OK" ]
        ]

  renderSmallTitle t = HH.h4_ [ HH.text t ]

getBillingAccount ::
  forall slots f m.
  MonadAff m =>
  CredentialStore f m =>
  H.HalogenM State Action slots Output m (Loadable SS.BillingAccount)
getBillingAccount = do
  state <- H.get
  -- Try to fetch the billing account. This is only possible if we have a CRM
  -- account ID and billing account ID.
  case Tuple state.crmAccountId state.billingAccountId of
    Tuple (Just crmAccountId) (Just billingAccountId) ->
      H.lift
        $ Requests.getBillingAccount crmAccountId billingAccountId
    _ -> pure Idle

handleAction ::
  forall slots f m.
  MonadAff m =>
  CredentialStore f m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Initialize -> do
    billingAccount <- getBillingAccount
    H.modify_
      _
        { billingAccount = billingAccount
        , acceptedBillingAccount = billingAccount
        }
    -- If we get a nice billing account then let the parent component know about
    -- the commercial object.
    case billingAccount of
      Loaded ba -> H.raise ba
      _ -> pure unit
  ChooseCommercial billingAccount -> do
    H.modify_
      _
        { commercial = map (\(SS.BillingAccount ba) -> ba.commercial) billingAccount
        , billingAccount = billingAccount
        }
    -- Switch focus to OK button.
    focusElementByRef okBtnLabel
  OpenDetails -> H.modify_ $ \st -> st { open = true }
  AcceptAndCloseDetails -> do
    st' <-
      H.modify \st ->
        st
          { acceptedCommercial = st.commercial
          , acceptedBillingAccount = st.billingAccount
          , open = false
          }
    case st'.acceptedBillingAccount of
      Loaded billingAccount -> H.raise billingAccount
      _ -> pure unit
  CancelAndCloseDetails ->
    H.modify_ \st ->
      st
        { commercial = st.acceptedCommercial
        , billingAccount = st.acceptedBillingAccount
        , open = false
        }

handleQuery ::
  forall action output a m.
  MonadAff m =>
  Query a -> H.HalogenM State action Slots output m (Maybe a)
handleQuery = case _ of
  ResetCommercial { commercial, crmAccountId, enabled } next -> do
    H.modify_ \st ->
      st
        { commercial = maybe Idle Loaded commercial
        , acceptedBillingAccount = Idle
        , crmAccountId = crmAccountId
        , billingAccountId =
          do
            SS.Commercial { billingAccountId } <- commercial
            billingAccountId
        , enabled = enabled
        }
    case crmAccountId of
      Nothing -> pure unit
      Just id -> H.tell SelectCommercial.proxy unit (SelectCommercial.SetCrmAccountId id)
    pure $ Just next
  ResetCommercialFixed { commercial, crmAccountId, enabled, open } next -> do
    H.modify_ \st ->
      st
        { commercial = maybe Idle Loaded commercial
        , acceptedBillingAccount = Idle
        , crmAccountId = crmAccountId
        , billingAccountId =
          do
            SS.Commercial { billingAccountId } <- commercial
            billingAccountId
        , enabled = enabled
        , open = open
        }
    case crmAccountId of
      Nothing -> pure unit
      Just id -> H.tell SelectCommercial.proxy unit (SelectCommercial.SetCrmAccountId id)
    pure $ Just next
