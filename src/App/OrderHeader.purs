module App.OrderForm.OrderHeader (Slot, Output, OrderHeader, proxy, component) where

import Prelude
import App.Requests (getBuyerContacts)
import App.SelectBuyer as SelectBuyer
import App.SelectCommercial as SelectCommercial
import App.SelectLegalEntity as SelectLegalEntity
import Css as Css
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Iso3166 (countryForCode, subdivisionForCode)
import Data.Loadable (Loadable(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.SmartSpec as SS
import Data.String as S
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Widgets as Widgets

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "orderHeader"
proxy = Proxy

type Slots
  = ( selectBuyer :: SelectBuyer.Slot Unit
    , selectCommercial :: SelectCommercial.Slot Unit
    , selectLegalEntity :: SelectLegalEntity.Slot Unit
    )

type OrderHeader
  = { commercial :: SS.Commercial, buyer :: SS.Buyer, seller :: SS.Seller }

type Input
  = Maybe OrderHeader

type Output
  = Maybe OrderHeader

type State
  = { legalEntity :: Maybe SS.LegalEntity -- ^ The chosen legal entity.
    , seller :: Maybe SS.Seller
    , buyer :: Maybe SS.Buyer
    , buyerAvailableContacts :: Loadable (Array (SS.Contact))
    , commercial :: Maybe SS.Commercial
    }

data Action
  = NoOp
  | UpdateCommercial (SS.Commercial -> SS.Commercial)
  | UpdateBuyer (SS.Buyer -> SS.Buyer)
  | UpdateSeller (SS.Seller -> SS.Seller)
  | SetLegalEntity (Maybe SS.LegalEntity)
  | SetBuyer (Maybe SS.Buyer)
  | SetBuyerContactPrimary SS.Contact
  | SetBuyerContactFinance SS.Contact
  | SetCommercial (Maybe SS.Commercial)

component ::
  forall query m.
  MonadAff m => CredentialStore m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState customer =
  { legalEntity: Nothing
  , seller: _.seller <$> customer
  , buyer: _.buyer <$> customer
  , buyerAvailableContacts: Idle
  , commercial: _.commercial <$> customer
  }

emptyCommercial :: SS.Commercial
emptyCommercial =
  SS.Commercial
    { billingOption: SS.Prepay
    , contractTerm: SS.Ongoing
    , paymentCurrency: SS.PaymentCurrency (SS.Currency "")
    , billingCurrency: SS.PricingCurrency (SS.Currency "")
    }

emptyBuyer :: SS.Buyer
emptyBuyer =
  SS.Buyer
    { crmAccountId: Nothing
    , address: SS.emptyAddress
    , contacts: { primary: SS.emptyContact, finance: SS.emptyContact }
    , corporateName: ""
    , registrationNr: ""
    , taxId: ""
    , website: ""
    }

emptySeller :: SS.Seller
emptySeller =
  SS.Seller
    { registeredName: ""
    , novaShortName: ""
    , address: SS.emptyAddress
    , contacts: { primary: SS.emptyContact, finance: SS.emptyContact, support: SS.emptyContact }
    }

render :: forall m. MonadAff m => CredentialStore m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div [ HP.classes [ Css.flex, Css.three ] ]
    $ [ renderSeller st.seller
      , renderBuyer st.buyer
      , renderCommercial st.commercial
      ]
  where
  renderCurrency ::
    forall currency.
    Newtype currency SS.Currency =>
    String ->
    currency ->
    (SS.Currency -> Action) ->
    H.ComponentHTML Action Slots m
  renderCurrency legend currency update =
    HH.div_
      [ HH.label_ [ HH.text legend ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.required true
          , HP.pattern "[A-Z]{3}"
          , HP.placeholder "Currency (e.g. EUR)"
          , HE.onValueChange $ \c -> update (SS.Currency c)
          , HP.value (let SS.Currency code = unwrap currency in code)
          ]
      ]

  renderCurrencySelect ::
    forall currency.
    Newtype currency SS.Currency =>
    String ->
    currency ->
    Set SS.Currency ->
    (SS.Currency -> Action) ->
    H.ComponentHTML Action Slots m
  renderCurrencySelect legend currency availableCurrencies update =
    let
      mkOption c =
        let
          str = show c
        in
          HH.option [ HP.value str, HP.selected $ c == unwrap currency ] [ HH.text str ]
    in
      HH.div_
        [ HH.label_ [ HH.text legend ]
        , HH.select [ HE.onValueChange $ \c -> update (SS.Currency c) ]
            $ [ HH.option
                  [ HP.value "", HP.disabled true, HP.selected true ]
                  [ HH.text
                      $ if availableCurrencies == mempty then
                          "No currencies available"
                        else
                          "Please choose a currency"
                  ]
              ]
            <> map mkOption (A.fromFoldable availableCurrencies)
        ]

  renderCommercial commercialOpt = case st.buyer of
    Just (SS.Buyer { crmAccountId: Just crmAccountId }) -> renderCommercial' crmAccountId
    _ ->
      HH.div_
        [ HH.button [ HP.disabled true, HP.style "width:100%" ] [ HH.text "Commercial" ]
        ]
    where
    update f = UpdateCommercial $ \(SS.Commercial old) -> SS.Commercial (f old)

    updateBillingOption = case _ of
      0 -> update (_ { billingOption = SS.Prepay })
      _ -> update (_ { billingOption = SS.Postpay })

    updateContractTerm = case _ of
      0 -> update (_ { contractTerm = SS.Ongoing })
      _ -> update (_ { contractTerm = SS.Fixed })

    renderCommercialData (SS.Commercial commercial) =
      HH.div_
        [ HH.label_
            [ HH.text "Billing Option"
            , HH.select [ HE.onSelectedIndexChange updateBillingOption ]
                [ HH.option
                    [ HP.value "Prepay"
                    , HP.selected (commercial.billingOption == SS.Prepay)
                    ]
                    [ HH.text "Pre-pay" ]
                , HH.option
                    [ HP.value "Postpay"
                    , HP.selected (commercial.billingOption == SS.Postpay)
                    ]
                    [ HH.text "Post-pay" ]
                ]
            ]
        , HH.label_
            [ HH.text "Contract Term"
            , HH.select [ HE.onSelectedIndexChange updateContractTerm ]
                [ HH.option
                    [ HP.value "Ongoing"
                    , HP.selected (commercial.contractTerm == SS.Ongoing)
                    ]
                    [ HH.text "Ongoing" ]
                , HH.option
                    [ HP.value "Fixed"
                    , HP.selected (commercial.contractTerm == SS.Fixed)
                    ]
                    [ HH.text "Fixed" ]
                ]
            ]
        , HH.div [ HP.classes [ Css.flex, Css.two ] ]
            [ renderCurrency
                "Payment Currency"
                commercial.paymentCurrency
                $ \currency -> update (\c -> c { paymentCurrency = SS.PaymentCurrency currency })
            , renderCurrencySelect
                "Billing Currency"
                commercial.billingCurrency
                (maybe mempty (_.availableCurrencies <<< unwrap) st.legalEntity)
                $ \currency -> update (\c -> c { billingCurrency = SS.PricingCurrency currency })
            ]
        ]

    renderCommercial' crmAccountId =
      HH.div_
        [ HH.label [ HP.for "of-commercial", HP.class_ Css.button, HP.style "width:100%" ]
            [ HH.text "Commercial" ]
        , Widgets.modal "of-commercial" "Commercial"
            [ HH.slot SelectCommercial.proxy unit SelectCommercial.component crmAccountId SetCommercial
            , HH.hr_
            , renderCommercialData $ fromMaybe emptyCommercial commercialOpt
            ]
            [ HH.label
                [ HP.for "of-commercial", HP.class_ Css.button ]
                [ HH.text "OK" ]
            ]
        ]

  renderReadOnlyAddress (SS.Address address) =
    let
      entry title value = case value of
        Nothing -> []
        Just v ->
          [ HH.div_ [ HH.text title ]
          , HH.div [ HP.class_ Css.twoThird ] [ HH.text v ]
          ]
    in
      [ HH.fieldset_
          [ HH.legend_ [ HH.text "Address" ]
          , HH.div [ HP.classes [ Css.flex, Css.three ] ]
              $ entry "" address.line1
              <> entry "" address.line2
              <> entry "" address.line3
              <> entry "P/O Box" address.postOfficeBox
              <> entry "Postal Code" address.postalCode
              <> entry "City" address.city
              <> entry "County" address.county
              <> entry "State or Province"
                  ( do
                      SS.Country cCode <- address.country
                      SS.Subdivision sCode <- address.stateOrProvince
                      subdiv <- subdivisionForCode cCode sCode
                      pure subdiv.name
                  )
              <> entry "Country"
                  ( do
                      SS.Country cCode <- address.country
                      country <- countryForCode cCode
                      pure country.name
                  )
          ]
      ]

  renderContact label (SS.Contact contact) update =
    let
      opt = fromMaybe ""

      unopt v = if v == "" then Nothing else Just v

      update' f = update \(SS.Contact oldContact) -> SS.Contact $ f oldContact
    in
      HH.fieldset_
        [ HH.legend_ [ HH.text label ]
        , HH.div [ HP.class_ Css.three ]
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.placeholder "Name"
                , HP.required true
                , HP.value $ opt contact.displayName
                , HE.onValueChange \v -> update' \c -> c { displayName = unopt v }
                ]
            , HH.input
                [ HP.type_ HP.InputEmail
                , HP.placeholder "Email"
                , HP.required true
                , HP.value $ opt contact.email
                , HE.onValueChange \v -> update' \c -> c { email = unopt v }
                ]
            , HH.input
                [ HP.type_ HP.InputTel
                , HP.placeholder "Phone No"
                , HP.value $ opt contact.phone
                , HE.onValueChange \v -> update' \c -> c { phone = unopt v }
                ]
            ]
        ]

  renderBuyer buyerOpt
    | isNothing st.seller =
      HH.div_
        [ HH.button [ HP.disabled true, HP.style "width:100%" ] [ HH.text "Buyer" ]
        ]
    | otherwise =
      let
        renderContactOption (SS.Contact c) =
          HH.option
            [ HP.value $ maybe "" unwrap c.contactId ]
            [ HH.text $ fromMaybe "" c.displayName
            , HH.span_ $ maybe [] (\e -> [ HH.text " <", HH.text e, HH.text ">" ]) c.email
            ]

        actionSetContact act id = case st.buyerAvailableContacts of
          Loaded contacts ->
            maybe NoOp act
              $ let
                  id' = Just (SS.ContactId id)
                in
                  A.find (\(SS.Contact le) -> id' == le.contactId) contacts
          _ -> NoOp

        renderContactSelect lbl act =
          HH.label_
            [ HH.text lbl
            , HH.select [ HE.onValueChange $ actionSetContact act ]
                $ case st.buyerAvailableContacts of
                    Idle ->
                      [ HH.option
                          [ HP.value "", HP.disabled true, HP.selected true ]
                          [ HH.text "No contacts loaded" ]
                      ]
                    Loaded contacts ->
                      [ HH.option
                          [ HP.value "", HP.disabled true, HP.selected true ]
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

        renderBuyerData (SS.Buyer buyer) =
          HH.div_
            $ [ HH.label_
                  [ HH.text "Corporate Name"
                  , HH.input
                      [ HP.type_ HP.InputText
                      , HP.required true
                      , HP.readOnly true
                      , HP.value buyer.corporateName
                      ]
                  ]
              , HH.label_
                  [ HH.text "Registration Number"
                  , HH.input
                      [ HP.type_ HP.InputText
                      , HP.required true
                      , HP.readOnly true
                      , HP.placeholder "012345"
                      , HP.value buyer.registrationNr
                      ]
                  ]
              , HH.label_
                  [ HH.text "Tax ID"
                  , HH.input
                      [ HP.type_ HP.InputText
                      , HP.required true
                      , HP.readOnly true
                      , HP.placeholder "012345"
                      , HP.value buyer.taxId
                      ]
                  ]
              , HH.label_
                  [ HH.text "Website"
                  , HH.input
                      [ HP.type_ HP.InputUrl
                      , HP.required true
                      , HP.readOnly true
                      , HP.placeholder "https://example.org/"
                      , HP.value buyer.website
                      ]
                  ]
              , renderContactSelect "Primary Contact" SetBuyerContactPrimary
              , renderContactSelect "Finance Contact" SetBuyerContactFinance
              ]
            <> renderReadOnlyAddress buyer.address

        buyerName (SS.Buyer { corporateName }) = corporateName
      in
        HH.div_
          [ HH.label
              [ HP.for "of-buyer", HP.class_ Css.button, HP.style "width:100%" ]
              [ HH.text "Buyer"
              , HH.br_
              , HH.text $ maybe "No buyer selected" buyerName buyerOpt
              ]
          , Widgets.modal "of-buyer" "Buyer"
              [ HH.slot SelectBuyer.proxy unit SelectBuyer.component absurd SetBuyer
              , HH.hr_
              , renderBuyerData $ fromMaybe emptyBuyer buyerOpt
              ]
              [ HH.label
                  [ HP.for "of-buyer", HP.class_ Css.button ]
                  [ HH.text "OK" ]
              ]
          ]

  renderSeller sellerOpt =
    let
      update f = UpdateSeller $ \(SS.Seller old) -> SS.Seller (f old)

      defaultCurrencyStr = maybe "" (show <<< _.defaultBankCurrency <<< unwrap) st.legalEntity

      currenciesStr =
        S.joinWith ", " $ map show $ A.fromFoldable
          $ maybe mempty (_.availableCurrencies <<< unwrap) st.legalEntity

      renderSellerData (SS.Seller seller) =
        HH.div_
          $ [ HH.label_
                [ HH.text "Registered Name"
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.value seller.registeredName
                    , HP.readOnly true
                    ]
                ]
            , HH.div [ HP.classes [ Css.flex, Css.two ] ]
                [ HH.label_
                    [ HH.text "Default Bank Currency"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.value defaultCurrencyStr
                        , HP.readOnly true
                        ]
                    ]
                , HH.label_
                    [ HH.text "Available Currencies"
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.value $ currenciesStr
                        , HP.readOnly true
                        ]
                    ]
                ]
            , renderContact "Primary Contract"
                seller.contacts.primary
                $ \f -> update (\s -> s { contacts { primary = f s.contacts.primary } })
            , renderContact "Finance Contract"
                seller.contacts.finance
                $ \f -> update (\s -> s { contacts { finance = f s.contacts.finance } })
            , renderContact "Support Contract"
                seller.contacts.support
                $ \f -> update (\s -> s { contacts { support = f s.contacts.support } })
            ]
          <> renderReadOnlyAddress seller.address

      sellerName (SS.Seller { registeredName }) = registeredName
    in
      HH.div_
        [ HH.label
            [ HP.for "of-seller", HP.class_ Css.button, HP.style "width:100%" ]
            [ HH.text "Seller"
            , HH.br_
            , HH.text $ maybe "No seller selected" sellerName sellerOpt
            ]
        , Widgets.modal "of-seller" "Seller"
            [ HH.slot SelectLegalEntity.proxy unit SelectLegalEntity.component absurd SetLegalEntity
            , HH.hr_
            , renderSellerData $ fromMaybe emptySeller sellerOpt
            ]
            [ HH.label
                [ HP.for "of-seller", HP.class_ Css.button ]
                [ HH.text "OK" ]
            ]
        ]

raiseOrderHeaderIfComplete :: forall m. H.HalogenM State Action Slots Output m Unit
raiseOrderHeaderIfComplete = do
  { commercial, buyer, seller } <- H.get
  H.raise $ { commercial: _, buyer: _, seller: _ } <$> commercial <*> buyer <*> seller

handleAction ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  NoOp -> pure unit
  UpdateCommercial updater -> do
    H.modify_ $ \st -> st { commercial = updater <$> st.commercial }
    raiseOrderHeaderIfComplete
  UpdateBuyer updater -> do
    H.modify_ $ \st -> st { buyer = updater <$> st.buyer }
    raiseOrderHeaderIfComplete
  UpdateSeller updater -> do
    H.modify_ $ \st -> st { seller = updater <$> st.seller }
    raiseOrderHeaderIfComplete
  SetLegalEntity legalEntity -> do
    H.modify_
      $ \st ->
          let
            toSeller (SS.LegalEntity le) =
              SS.Seller
                { registeredName: le.registeredName
                , novaShortName: le.novaShortName
                , address: le.address
                , contacts: le.contacts
                }
          in
            st
              { legalEntity = legalEntity
              , seller = toSeller <$> legalEntity
              , commercial = Nothing
              , buyer = Nothing
              , buyerAvailableContacts = Idle
              }
  SetBuyer Nothing ->
    H.modify_
      _
        { commercial = Nothing
        , buyer = Nothing
        , buyerAvailableContacts = Idle
        }
  SetBuyer (Just buyer) -> do
    H.modify_
      _
        { commercial = Nothing
        , buyer = Just buyer
        , buyerAvailableContacts = Loading
        }
    case buyer of
      SS.Buyer { crmAccountId: Just crmAccountId } -> do
        -- Reset the commercial selector.
        H.tell SelectCommercial.proxy unit
          $ SelectCommercial.SetCrmAccountId crmAccountId
        -- Fetch the buyer contacts.
        contacts <- H.lift $ getBuyerContacts crmAccountId
        case contacts of
          Error err -> Console.error $ "When fetching contacts: " <> err
          _ -> pure unit
        H.modify_ _ { buyerAvailableContacts = contacts }
      _ -> pure unit
    raiseOrderHeaderIfComplete
  SetBuyerContactPrimary contact -> do
    H.modify_
      $ \st ->
          st { buyer = (\(SS.Buyer b) -> SS.Buyer $ b { contacts { primary = contact } }) <$> st.buyer }
    raiseOrderHeaderIfComplete
  SetBuyerContactFinance contact -> do
    H.modify_
      $ \st ->
          st { buyer = (\(SS.Buyer b) -> SS.Buyer $ b { contacts { finance = contact } }) <$> st.buyer }
    raiseOrderHeaderIfComplete
  SetCommercial commercial -> do
    H.modify_ _ { commercial = commercial }
    raiseOrderHeaderIfComplete
