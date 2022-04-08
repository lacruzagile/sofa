-- | The buyer component of the order form.
module Sofa.App.OrderForm.Buyer (Slot, Input(..), Output(..), Query(..), proxy, component) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String as S
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm.SelectBuyer as SelectBuyer
import Sofa.App.Requests (getBuyerContacts)
import Sofa.Component.Modal as Modal
import Sofa.Component.Select as Select
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))

type Slot id
  = H.Slot Query Output id

proxy :: Proxy "buyer"
proxy = Proxy

type Slots
  = ( selectBuyer :: SelectBuyer.Slot Unit
    , nectaryDropdown :: Select.Slot Int SS.Contact
    )

type ContactType
  = Int

contactPrimary = 1 :: ContactType

contactFinance = 2 :: ContactType

type Input
  = Maybe
      { buyer :: SS.Buyer
      , readOnly :: Boolean
      }

type Output
  = SS.Buyer

type State
  = { buyer :: Loadable SS.Buyer -- ^ The currently chosen buyer.
    , buyerAvailableContacts :: Loadable (Array (SS.Contact)) -- ^ Available contacts for the chosen buyer.
    , acceptedBuyer :: Loadable SS.Buyer -- ^ The latest accepted buyer.
    , readOnly :: Boolean
    , enabled :: Boolean
    , open :: Boolean -- ^ Whether the details modal is open.
    }

data Action
  = NoOp
  | ChooseBuyer (Loadable SS.Buyer)
  | SetContactPrimary SS.Contact
  | SetContactFinance SS.Contact
  | SetCustomerStatus Boolean
  | OpenDetails
  | AcceptAndCloseDetails
  | CancelAndCloseDetails

data Query a
  = ResetBuyer (Maybe SS.Buyer) Boolean a

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
    { buyer: Idle
    , buyerAvailableContacts: Idle
    , acceptedBuyer: Idle
    , readOnly: false
    , enabled: false
    , open: false
    }
  Just { buyer, readOnly } ->
    { buyer: Loaded buyer
    , buyerAvailableContacts: Idle
    , acceptedBuyer: Loaded buyer
    , readOnly
    , enabled: true
    , open: false
    }

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action Slots m
render state
  | state.open = renderDetails state
  | otherwise = renderSummary state

renderSummary :: forall slots m. State -> H.ComponentHTML Action slots m
renderSummary st
  | not st.enabled =
    HH.div
      [ HP.classes [ Css.c "text-gray-400" ] ]
      [ HH.text "Not available" ]
  | otherwise = case st.acceptedBuyer of
    Loaded (SS.Buyer { corporateName }) -> btn okClasses corporateName
    _ -> btn badClasses "Select …"
    where
    btn classes txt =
      HH.button
        [ HP.classes classes, HE.onClick $ \_ -> OpenDetails ]
        [ HH.text txt ]

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
    , Modal.render [] $ renderBody st.buyer
    ]
  where
  mkWebsiteUrl s
    | startsWith "http://" s || startsWith "https://" s = s
    | otherwise = "https://" <> s

  renderBody buyerLoadable =
    let
      renderBuyerData (SS.Buyer buyer) =
        HH.div [ HP.classes [ Css.c "flex", Css.c "flex-col", Css.c "gap-y-4" ] ]
          [ if st.readOnly then
              HH.text ""
            else
              HH.slot SelectBuyer.proxy unit SelectBuyer.component absurd ChooseBuyer
          , case buyerLoadable of
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
          , if buyer.corporateName == "" then
              HH.text ""
            else
              HH.div
                [ HP.classes
                    [ Css.c "w-full"
                    , Css.c "min-w-128"
                    , Css.c "p-8"
                    , Css.c "grid"
                    , Css.c "grid-cols-[12rem_auto]"
                    , Css.c "gap-4"
                    , Css.c "rounded"
                    , Css.c "bg-snow-500"
                    ]
                ]
                $ [ HH.h3 [ HP.class_ (Css.c "col-span-2") ] [ HH.text buyer.corporateName ]
                  , renderSmallTitle "CRM Account ID"
                  , HH.div_ [ HH.text (maybe "N/A" unwrap buyer.crmAccountId) ]
                  , renderSmallTitle "Registration No"
                  , HH.div_ [ HH.text buyer.registrationNr ]
                  , renderSmallTitle "Tax ID"
                  , HH.div_ [ HH.text buyer.taxId ]
                  , renderSmallTitle "Website"
                  , if S.null buyer.website then
                      HH.div_ [ HH.text "" ]
                    else
                      HH.a [ HP.href (mkWebsiteUrl buyer.website) ] [ HH.text buyer.website ]
                  , HH.h4 [ HP.class_ (Css.c "col-span-2") ] [ HH.text "Primary Contact" ]
                  , renderContact contactPrimary buyer.contacts.primary SetContactPrimary
                  , HH.h4 [ HP.class_ (Css.c "col-span-2") ] [ HH.text "Finance Contact" ]
                  , renderContact contactFinance buyer.contacts.finance SetContactFinance
                  , HH.h4 [ HP.class_ (Css.c "col-span-2") ] [ HH.text "Customer Status" ]
                  , HH.fieldset [ HP.class_ (Css.c "col-span-2") ]
                      [ HH.label [ HP.class_ (Css.c "ml-2") ]
                          [ HH.input
                              [ HP.type_ HP.InputRadio
                              , HP.name "buyer-existing-customer"
                              , HP.class_ (Css.c "nectary-input-radio")
                              , HP.checked $ not buyer.existingCustomer
                              , HP.enabled $ not st.readOnly
                              , HE.onChange \_ -> SetCustomerStatus false
                              ]
                          , HH.span [ HP.class_ (Css.c "ml-2") ] [ HH.text "New Customer" ]
                          ]
                      , HH.label [ HP.class_ (Css.c "ml-2") ]
                          [ HH.input
                              [ HP.type_ HP.InputRadio
                              , HP.name "buyer-existing-customer"
                              , HP.class_ (Css.c "nectary-input-radio")
                              , HP.checked buyer.existingCustomer
                              , HP.enabled $ not st.readOnly
                              , HE.onChange \_ -> SetCustomerStatus true
                              ]
                          , HH.span [ HP.class_ (Css.c "ml-2") ] [ HH.text "Existing Customer" ]
                          ]
                      ]
                  , HH.h4 [ HP.class_ (Css.c "col-span-2") ] [ HH.text "Address" ]
                  ]
                <> Widgets.address buyer.address
          , HH.div [ HP.classes [ Css.c "flex", Css.c "space-x-5" ] ] bottomButtons
          ]
        where
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
                [ HP.class_ (Css.c "sofa-btn-primary")
                , HP.enabled
                    ( Loadable.isLoaded st.buyer
                        && (buyer.contacts.primary /= SS.emptyContact)
                        && (buyer.contacts.finance /= SS.emptyContact)
                    )
                , HE.onClick \_ -> AcceptAndCloseDetails
                ]
                [ HH.text "OK" ]
            ]
    in
      renderBuyerData $ fromMaybe emptyBuyer $ Loadable.toMaybe buyerLoadable

  renderSmallTitle t = HH.h4_ [ HH.text t ]

  renderContact ::
    ContactType ->
    SS.Contact ->
    (SS.Contact -> Action) ->
    H.ComponentHTML Action Slots m
  renderContact contactType (SS.Contact contact) act
    | st.readOnly =
      let
        opt = case _ of
          Nothing -> []
          Just "" -> []
          Just val -> [ HH.text val ]

        handleNoContact = case _ of
          [] -> [ HH.span [ HP.class_ (Css.c "text-gray-400") ] [ HH.text "None" ] ]
          vals -> vals

        subtleSlash = HH.span [ HP.class_ (Css.c "text-gray-400") ] [ HH.text " / " ]
      in
        HH.div [ HP.class_ (Css.c "col-span-2") ]
          $ handleNoContact
          $ A.intersperse subtleSlash
          $ opt contact.displayName
          <> opt contact.email
          <> opt contact.phone
    | otherwise =
      let
        selectOption c@(SS.Contact { displayName, email }) =
          Tuple
            ( HH.span_
                [ HH.text $ fromMaybe "" displayName
                , HH.span_ $ maybe [] (\e -> [ HH.text " <", HH.text e, HH.text ">" ]) email
                ]
            )
            c
      in
        HH.div [ HP.class_ (Css.c "col-span-2") ]
          [ case st.buyerAvailableContacts of
              Idle -> HH.text "No contacts loaded"
              Loaded contacts ->
                let
                  input =
                    Select.defaultInput
                      { selected = Just $ SS.Contact contact
                      , values = selectOption <$> contacts
                      , noSelectionText = "Please choose a contact"
                      , wrapperClasses = [ Css.c "min-w-[20rem]", Css.c "max-w-128" ]
                      }
                in
                  HH.slot Select.proxy contactType Select.component input act
              Loading ->
                HH.span
                  [ HP.class_ $ Css.c "animate-pulse" ]
                  [ HH.text "Loading contacts …" ]
              Error _ -> HH.text $ "Error loading contacts"
          ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ChooseBuyer (Loaded buyer) -> do
    H.modify_ $ \st -> st { buyer = Loaded buyer, buyerAvailableContacts = Loading }
    case buyer of
      SS.Buyer { crmAccountId: Just crmAccountId } -> do
        -- Fetch the buyer contacts.
        contacts <- H.lift $ getBuyerContacts crmAccountId
        case contacts of
          Error err -> Console.error $ "When fetching contacts: " <> err
          _ -> pure unit
        H.modify_ _ { buyerAvailableContacts = contacts }
      _ -> pure unit
  ChooseBuyer buyer ->
    H.modify_
      $ \st ->
          st
            { buyer = buyer
            , buyerAvailableContacts = Idle
            }
  SetContactPrimary contact ->
    let
      setContact (SS.Buyer s) = SS.Buyer $ s { contacts { primary = contact } }
    in
      H.modify_ $ \st -> st { buyer = setContact <$> st.buyer }
  SetContactFinance contact ->
    let
      setContact (SS.Buyer s) = SS.Buyer $ s { contacts { finance = contact } }
    in
      H.modify_ $ \st -> st { buyer = setContact <$> st.buyer }
  SetCustomerStatus existingCustomer ->
    let
      setExistingCustomer (SS.Buyer s) = SS.Buyer $ s { existingCustomer = existingCustomer }
    in
      H.modify_ $ \st -> st { buyer = setExistingCustomer <$> st.buyer }
  OpenDetails -> H.modify_ $ \st -> st { open = true }
  AcceptAndCloseDetails -> do
    st' <- H.modify \st -> st { acceptedBuyer = st.buyer, open = false }
    case st'.acceptedBuyer of
      Loaded buyer -> H.raise buyer
      _ -> pure unit
  CancelAndCloseDetails -> H.modify_ \st -> st { buyer = st.acceptedBuyer, open = false }

handleQuery ::
  forall action slots output a m.
  MonadAff m =>
  Query a -> H.HalogenM State action slots output m (Maybe a)
handleQuery = case _ of
  ResetBuyer buyer enabled next -> do
    H.modify_ \st ->
      st
        { buyer = maybe Idle Loaded buyer
        , buyerAvailableContacts = Idle
        , acceptedBuyer = Idle
        , enabled = enabled
        }
    pure $ Just next

emptyBuyer :: SS.Buyer
emptyBuyer =
  SS.Buyer
    { buyerId: Nothing
    , crmAccountId: Nothing
    , address: SS.emptyAddress
    , contacts: { primary: SS.emptyContact, finance: SS.emptyContact }
    , corporateName: ""
    , registrationNr: ""
    , existingCustomer: true
    , taxId: ""
    , website: ""
    }
