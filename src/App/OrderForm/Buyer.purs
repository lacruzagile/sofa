-- | The buyer component of the order form.
module App.OrderForm.Buyer (Slot, Input(..), Output(..), Query(..), proxy, component) where

import Prelude
import App.OrderForm.SelectBuyer as SelectBuyer
import App.Requests (getBuyerContacts)
import Css as Css
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec as SS
import Data.String as S
import Data.String.Utils (startsWith)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Widgets as Widgets

type Slot id
  = H.Slot Query Output id

proxy :: Proxy "buyer"
proxy = Proxy

type Slots
  = ( selectBuyer :: SelectBuyer.Slot Unit )

type Input
  = Maybe
      { buyer :: SS.Buyer
      , readOnly :: Boolean
      }

type Output
  = SS.Buyer

type State
  = { buyer :: Maybe SS.Buyer -- ^ The currently chosen buyer.
    , buyerAvailableContacts :: Loadable (Array (SS.Contact)) -- ^ Available contacts for the chosen buyer.
    , acceptedBuyer :: Maybe SS.Buyer -- ^ The latest accepted buyer.
    , readOnly :: Boolean
    , enabled :: Boolean
    , open :: Boolean -- ^ Whether the details modal is open.
    }

data Action
  = NoOp
  | ChooseBuyer (Maybe SS.Buyer)
  | SetContactPrimary SS.Contact
  | SetContactFinance SS.Contact
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
    { buyer: Nothing
    , buyerAvailableContacts: Idle
    , acceptedBuyer: Nothing
    , readOnly: false
    , enabled: false
    , open: false
    }
  Just { buyer, readOnly } ->
    { buyer: Just buyer
    , buyerAvailableContacts: Idle
    , acceptedBuyer: Just buyer
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
      [ HP.classes [ Css.tw.text2Xl, Css.tw.textGray400 ] ]
      [ HH.text "Not available" ]
  | otherwise = case st.acceptedBuyer of
    Just (SS.Buyer { corporateName }) -> btn okClasses corporateName
    Nothing -> btn badClasses "None selected"
    where
    btn classes txt =
      HH.button
        [ HP.classes classes, HE.onClick $ \_ -> OpenDetails ]
        [ HH.text txt ]

    okClasses =
      [ Css.tw.block
      , Css.tw.textLeft
      , Css.tw.text2Xl
      , Css.tw.underline
      , Css.tw.underlineOffset4
      , Css.tw.decorationSky300
      ]

    badClasses =
      [ Css.tw.block
      , Css.tw.textLeft
      , Css.tw.text2Xl
      , Css.tw.underline
      , Css.tw.underlineOffset4
      , Css.tw.decorationSky300
      ]

renderDetails ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action Slots m
renderDetails st =
  HH.div_
    [ renderSummary st
    , Widgets.modal modalToolbar $ renderBody st.buyer
    ]
  where
  modalToolbar =
    if st.readOnly then
      [ Widgets.modalCloseBtn (\_ -> CancelAndCloseDetails) ]
    else
      [ HH.slot SelectBuyer.proxy unit SelectBuyer.component absurd ChooseBuyer
      , Widgets.modalCloseBtn (\_ -> CancelAndCloseDetails)
      ]

  mkWebsiteUrl s
    | startsWith "http://" s || startsWith "https://" s = s
    | otherwise = "https://" <> s

  renderBody buyerOpt =
    let
      renderBuyerData (SS.Buyer buyer) =
        HH.div [ HP.classes [ Css.tw.wFull, Css.tw.minW128, Css.tw.flex, Css.tw.flexCol, Css.tw.spaceY4 ] ]
          $ [ HH.div_
                [ renderSmallTitle "Corporate Name"
                , HH.div [ HP.classes [ Css.tw.ml2, Css.tw.text2Xl ] ] [ HH.text buyer.corporateName ]
                ]
            , HH.div_
                [ renderSmallTitle "CRM Account ID"
                , HH.div [ HP.class_ Css.tw.ml2 ] [ HH.text (maybe "N/A" unwrap buyer.crmAccountId) ]
                ]
            , HH.div_
                [ renderSmallTitle "Registration No"
                , HH.div [ HP.class_ Css.tw.ml2 ] [ HH.text buyer.registrationNr ]
                ]
            , HH.div_
                [ renderSmallTitle "Tax ID"
                , HH.div [ HP.class_ Css.tw.ml2 ] [ HH.text buyer.taxId ]
                ]
            , HH.div_
                [ renderSmallTitle "Website"
                , HH.div [ HP.class_ Css.tw.ml2 ]
                    $ if S.null buyer.website then
                        []
                      else
                        [ HH.a [ HP.href (mkWebsiteUrl buyer.website) ] [ HH.text buyer.website ] ]
                ]
            , renderContact "Primary Contact" buyer.contacts.primary SetContactPrimary
            , renderContact "Finance Contact" buyer.contacts.finance SetContactFinance
            , HH.div_
                [ renderSmallTitle "Address"
                , Widgets.address buyer.address
                ]
            , HH.hr_
            , HH.div [ HP.classes [ Css.tw.flex, Css.tw.spaceX5 ] ] bottomButtons
            ]
        where
        bottomButtons
          | st.readOnly =
            [ HH.div [ HP.class_ Css.tw.grow ] []
            , HH.button
                [ HP.class_ Css.btnTropical, HE.onClick \_ -> CancelAndCloseDetails ]
                [ HH.text "Close" ]
            ]
          | otherwise =
            [ HH.div [ HP.class_ Css.tw.grow ] []
            , HH.button
                [ HP.class_ Css.btnTropical
                , HP.enabled
                    ( isJust st.buyer
                        && (buyer.contacts.primary /= SS.emptyContact)
                        && (buyer.contacts.finance /= SS.emptyContact)
                    )
                , HE.onClick \_ -> AcceptAndCloseDetails
                ]
                [ HH.text "OK" ]
            , HH.button
                [ HP.class_ Css.btnRed100, HE.onClick \_ -> CancelAndCloseDetails ]
                [ HH.text "Cancel" ]
            ]
    in
      renderBuyerData $ fromMaybe emptyBuyer buyerOpt

  renderSmallTitle t = HH.div [ HP.class_ Css.smallTitle ] [ HH.text t ]

  renderContact label (SS.Contact contact) act
    | st.readOnly =
      let
        opt = case _ of
          Nothing -> []
          Just "" -> []
          Just val -> [ HH.text val ]

        handleNoContact = case _ of
          [] -> [ HH.span [ HP.class_ Css.tw.textGray400 ] [ HH.text "None" ] ]
          vals -> vals

        subtleSlash = HH.span [ HP.class_ Css.tw.textGray400 ] [ HH.text " / " ]
      in
        HH.div_
          [ renderSmallTitle label
          , HH.div [ HP.classes [ Css.tw.ml2, Css.tw.textLg ] ]
              $ handleNoContact
              $ A.intersperse subtleSlash
              $ opt contact.displayName
              <> opt contact.email
              <> opt contact.phone
          ]
    | otherwise =
      let
        renderContactOption (SS.Contact c) =
          HH.option
            [ HP.value $ maybe "" unwrap c.contactId, HP.selected $ contact == c ]
            [ HH.text $ fromMaybe "" c.displayName
            , HH.span_ $ maybe [] (\e -> [ HH.text " <", HH.text e, HH.text ">" ]) c.email
            ]

        actionSetContact id = case st.buyerAvailableContacts of
          Loaded contacts ->
            maybe NoOp act
              $ let
                  id' = Just (SS.ContactId id)
                in
                  A.find (\(SS.Contact le) -> id' == le.contactId) contacts
          _ -> NoOp
      in
        HH.label_
          [ renderSmallTitle label
          , HH.select
              [ HP.classes
                  [ Css.tw.appearanceNone
                  , Css.tw.bgTransparent
                  , Css.tw.w96
                  , Css.tw.textEllipsis
                  , Css.tw.underline
                  , Css.tw.underlineOffset4
                  , Css.tw.decorationSky300
                  ]
              , HE.onValueChange actionSetContact
              ]
              $ case st.buyerAvailableContacts of
                  Idle ->
                    [ HH.option
                        [ HP.value "", HP.disabled true, HP.selected true ]
                        [ HH.text "No contacts loaded" ]
                    ]
                  Loaded contacts ->
                    [ HH.option
                        [ HP.value ""
                        , HP.disabled true
                        , HP.selected $ SS.Contact contact == SS.emptyContact
                        ]
                        [ HH.text "Please choose a contact" ]
                    ]
                      <> map renderContactOption contacts
                  Loading ->
                    [ HH.option
                        [ HP.value "", HP.disabled true, HP.selected true ]
                        [ HH.text "Loading contacts…" ]
                    ]
                  Error _ ->
                    [ HH.option
                        [ HP.value "", HP.disabled true, HP.selected true ]
                        [ HH.text $ "Error loading contacts" ]
                    ]
          ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  NoOp -> pure unit
  ChooseBuyer Nothing ->
    H.modify_
      $ \st ->
          st
            { buyer = Nothing
            , buyerAvailableContacts = Idle
            }
  ChooseBuyer (Just buyer) -> do
    H.modify_ $ \st -> st { buyer = Just buyer, buyerAvailableContacts = Loading }
    case buyer of
      SS.Buyer { crmAccountId: Just crmAccountId } -> do
        -- Fetch the buyer contacts.
        contacts <- H.lift $ getBuyerContacts crmAccountId
        case contacts of
          Error err -> Console.error $ "When fetching contacts: " <> err
          _ -> pure unit
        H.modify_ _ { buyerAvailableContacts = contacts }
      _ -> pure unit
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
  OpenDetails -> H.modify_ $ \st -> st { open = true }
  AcceptAndCloseDetails -> do
    st' <- H.modify $ \st -> st { acceptedBuyer = st.buyer, open = false }
    case st'.acceptedBuyer of
      Nothing -> pure unit
      Just buyer -> H.raise buyer
  CancelAndCloseDetails -> H.modify_ $ \st -> st { buyer = st.acceptedBuyer, open = false }

handleQuery ::
  forall action slots output a m.
  MonadAff m =>
  Query a -> H.HalogenM State action slots output m (Maybe a)
handleQuery = case _ of
  ResetBuyer buyer enabled next -> do
    H.modify_ \st ->
      st
        { buyer = buyer
        , buyerAvailableContacts = Idle
        , acceptedBuyer = Nothing
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
    , taxId: ""
    , website: ""
    }
