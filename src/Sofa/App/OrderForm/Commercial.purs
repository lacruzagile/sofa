-- | The commercial component of the order form.
module Sofa.App.OrderForm.Commercial (Slot, Input(..), Output(..), Query(..), proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm.SelectCommercial as SelectCommercial
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..), isLoaded)
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (focusElementByRef)
import Type.Proxy (Proxy(..))

type Slot id
  = H.Slot Query Output id

proxy :: Proxy "commercial"
proxy = Proxy

type Slots
  = ( selectCommercial :: SelectCommercial.Slot Unit )

type Input
  = Maybe
      { commercial :: SS.Commercial
      , crmAccountId :: SS.CrmAccountId
      , readOnly :: Boolean
      }

type Output
  = SS.Commercial

type State
  = { commercial :: Loadable SS.Commercial -- ^ The currently chosen commercial.
    , acceptedCommercial :: Loadable SS.Commercial -- ^ The latest accepted commercial.
    , crmAccountId :: Maybe SS.CrmAccountId
    , readOnly :: Boolean
    , enabled :: Boolean
    , open :: Boolean -- ^ Whether the details modal is open.
    }

data Action
  = NoOp
  | ChooseCommercial (Loadable SS.Commercial)
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

component ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }

initialState :: Input -> State
initialState input = case input of
  Nothing ->
    { commercial: Idle
    , acceptedCommercial: Idle
    , crmAccountId: Nothing
    , readOnly: false
    , enabled: false
    , open: false
    }
  Just { commercial, crmAccountId, readOnly } ->
    { commercial: Loaded commercial
    , acceptedCommercial: Loaded commercial
    , crmAccountId: Just crmAccountId
    , readOnly
    , enabled: true
    , open: false
    }

okBtnLabel :: H.RefLabel
okBtnLabel = H.RefLabel "ok-btn"

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
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
      [ HP.classes [ Css.c "text-gray-400" ] ]
      [ HH.text "Not available" ]
  | otherwise = case st.acceptedCommercial of
    Loaded c ->
      btn okClasses
        [ renderBillingOption c
        , subtleSlash
        , renderContractTerm c
        , subtleSlash
        , renderBillingCurrency c
        ]
    _ -> btn badClasses [ HH.text "Select …" ]
    where
    btn classes = HH.button [ HP.classes classes, HE.onClick $ \_ -> OpenDetails ]

    subtleSlash = HH.span [ HP.class_ (Css.c "text-gray-400") ] [ HH.text " / " ]

    okClasses =
      [ Css.c "block"
      , Css.c "text-left"
      , Css.c "underline"
      , Css.c "underline-offset-4"
      , Css.c "decoration-honey-500"
      ]

    badClasses =
      [ Css.c "block"
      , Css.c "text-left"
      , Css.c "underline"
      , Css.c "underline-offset-4"
      , Css.c "decoration-honey-500"
      ]

renderDetails ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action Slots m
renderDetails st =
  HH.div_
    [ renderSummary st
    , Modal.render
        $ Modal.defaultInput
            { title = HH.text "Commercial"
            , closeAction = Just (const CancelAndCloseDetails)
            , content = renderContent st.commercial
            }
    ]
  where
  renderContent commercialLoadable =
    HH.div [ HP.classes [ Css.c "flex", Css.c "flex-col", Css.c "gap-y-4" ] ]
      [ case st.crmAccountId of
          Just crmAccountId
            | not st.readOnly ->
              HH.slot
                SelectCommercial.proxy
                unit
                SelectCommercial.component
                crmAccountId
                ChooseCommercial
          _ -> HH.text ""
      , case commercialLoadable of
          Error err ->
            HH.p
              [ HP.classes
                  [ Css.c "p-3"
                  , Css.c "bg-red-100"
                  , Css.c "border"
                  , Css.c "border-red-400"
                  , Css.c "text-raspberry-500"
                  ]
              ]
              [ HH.text err ]
          _ -> HH.text ""
      , HH.div
          [ HP.classes
              [ Css.c "w-full"
              , Css.c "min-w-96"
              , Css.c "p-8"
              , Css.c "grid"
              , Css.c "grid-cols-[12rem_auto]"
              , Css.c "gap-4"
              , Css.c "rounded"
              , Css.c "bg-snow-500"
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
      , HH.div [ HP.classes [ Css.c "flex", Css.c "space-x-5" ] ] bottomButtons
      ]
    where
    mCommercial = Loadable.toMaybe commercialLoadable

    empty = HH.text ""

    bottomButtons
      | st.readOnly =
        [ HH.div [ HP.class_ (Css.c "grow") ] []
        , HH.button
            [ HP.class_ (Css.c "sofa-btn-primary")
            , HE.onClick \_ -> CancelAndCloseDetails
            ]
            [ HH.text "Close" ]
        ]
      | otherwise =
        [ HH.div [ HP.class_ (Css.c "grow") ] []
        , HH.button
            [ HP.class_ (Css.c "sofa-btn-secondary")
            , HE.onClick \_ -> CancelAndCloseDetails
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.ref okBtnLabel
            , HP.class_ (Css.c "sofa-btn-primary")
            , HP.enabled (isLoaded st.commercial)
            , HE.onClick \_ -> AcceptAndCloseDetails
            ]
            [ HH.text "OK" ]
        ]

  renderSmallTitle t = HH.h4_ [ HH.text t ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ChooseCommercial commercial -> do
    H.modify_ $ \st -> st { commercial = commercial }
    -- Switch focus to OK button.
    focusElementByRef okBtnLabel
  OpenDetails -> H.modify_ $ \st -> st { open = true }
  AcceptAndCloseDetails -> do
    st' <- H.modify $ \st -> st { acceptedCommercial = st.commercial, open = false }
    case st'.acceptedCommercial of
      Loaded commercial -> H.raise commercial
      _ -> pure unit
  CancelAndCloseDetails -> H.modify_ $ \st -> st { commercial = st.acceptedCommercial, open = false }

handleQuery ::
  forall action output a m.
  MonadAff m =>
  Query a -> H.HalogenM State action Slots output m (Maybe a)
handleQuery = case _ of
  ResetCommercial { commercial, crmAccountId, enabled } next -> do
    H.modify_ \st ->
      st
        { commercial = maybe Idle Loaded commercial
        , acceptedCommercial = Idle
        , crmAccountId = crmAccountId
        , enabled = enabled
        }
    case crmAccountId of
      Nothing -> pure unit
      Just id -> H.tell SelectCommercial.proxy unit (SelectCommercial.SetCrmAccountId id)
    pure $ Just next
