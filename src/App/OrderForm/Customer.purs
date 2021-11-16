module App.OrderForm.Customer (Slot, Output, proxy, component) where

import Prelude
import App.Requests (getLegalEntities)
import Css as Css
import Data.Array as A
import Data.Iso3166 (countryForCode, subdivisionForCode)
import Data.Iso3166 as Iso3166
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

proxy :: Proxy "customer"
proxy = Proxy

type Input
  = Maybe SS.Customer

type Output
  = SS.Customer

type State
  = { customer :: Maybe SS.Customer
    , legalEntity :: Maybe SS.LegalEntity -- ^ The chosen legal entity.
    , legalEntities :: Loadable SS.LegalEntities
    }

data Action
  = NoOp
  | Initialize SS.Customer
  | UpdateCustomer (SS.Customer -> SS.Customer)
  | SetLegalEntity SS.LegalEntity

component ::
  forall query m.
  MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState customer = { customer, legalEntity: Nothing, legalEntities: Idle }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render st =
  HH.div [ HP.classes [ Css.flex, Css.two ] ]
    [ HH.div [ HP.class_ Css.one ] renderSelectCustomerType
    , HH.div [ HP.class_ Css.one ] (renderCustomerDetails st.customer)
    ]
  where
  renderSelectCustomerType =
    [ HH.button
        [ HE.onClick \_ ->
            Initialize
              $ SS.NewCustomer
                  { commercial:
                      SS.Commercial
                        { billingOption: SS.Prepay
                        , contractTerm: SS.Ongoing
                        , paymentCurrency: SS.PaymentCurrency (SS.Currency "")
                        , billingCurrency: SS.PricingCurrency (SS.Currency "")
                        }
                  , purchaser:
                      SS.Purchaser
                        { address: SS.emptyAddress
                        , contacts:
                            { primary: SS.emptyContact
                            , finance: SS.emptyContact
                            }
                        , corporateName: ""
                        , registrationNr: ""
                        , taxID: ""
                        , website: ""
                        }
                  , seller:
                      SS.Seller
                        { name: ""
                        , address: SS.emptyAddress
                        , contacts:
                            { primary: SS.emptyContact
                            , finance: SS.emptyContact
                            , support: SS.emptyContact
                            }
                        }
                  }
        ]
        [ HH.text "New Customer →" ]
    , HH.br_
    , HH.button
        [ HE.onClick \_ ->
            Initialize
              $ SS.ReturnCustomer
                  { commercial:
                      SS.RccBillingAccountRef
                        $ SS.BillingAccountRef
                            { billingAccountID: ""
                            }
                  , customer:
                      SS.ReturnCustomerData
                        { assets: []
                        , salesforceAccountRef:
                            SS.SalesforceAccountRef
                              { salesforceAccountID: ""
                              }
                        }
                  }
        ]
        [ HH.text "Return Customer →" ]
    ]

  renderCustomerDetails = case _ of
    Nothing -> []
    Just (SS.NewCustomer c) -> renderNewCustomer c
    Just (SS.ReturnCustomer c) -> renderReturnCustomer c

  renderNewCustomer c =
    renderCommercial c.commercial
      <> renderPurchaser c.purchaser
      <> renderSeller c.seller

  renderReturnCustomer _c = [ HH.text "Return customers are unsupported at the moment" ]

  renderCurrency ::
    forall currency.
    Newtype currency SS.Currency =>
    String ->
    currency ->
    (SS.Currency -> Action) ->
    H.ComponentHTML Action slots m
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
    H.ComponentHTML Action slots m
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

  renderCommercial (SS.Commercial commercial) =
    let
      update f =
        UpdateCustomer
          $ case _ of
              SS.NewCustomer oldCustomer ->
                let
                  SS.Commercial oldCommercial = oldCustomer.commercial
                in
                  SS.NewCustomer $ oldCustomer { commercial = SS.Commercial (f oldCommercial) }
              returnCustomer -> returnCustomer

      updateBillingOption = case _ of
        0 -> update (_ { billingOption = SS.Prepay })
        _ -> update (_ { billingOption = SS.Postpay })

      updateContractTerm = case _ of
        0 -> update (_ { contractTerm = SS.Ongoing })
        _ -> update (_ { contractTerm = SS.Fixed })
    in
      [ HH.label [ HP.for "of-commercial", HP.class_ Css.button ] [ HH.text "Commercial" ]
      , Widgets.modal "of-commercial" "Commercial"
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
          [ HH.label
              [ HP.for "of-commercial", HP.class_ Css.button ]
              [ HH.text "OK" ]
          ]
      ]

  -- Renders a drop-down list of countries.
  renderCountrySelect ::
    { id :: String
    , set :: SS.Country -> Action
    , selected :: Maybe SS.Country
    } ->
    H.ComponentHTML Action slots m
  renderCountrySelect { id, set, selected } =
    let
      countryEntry c =
        HH.option
          [ HP.value c.alpha2, HP.selected (Just (SS.Country c.alpha2) == selected) ]
          [ HH.text c.name ]
    in
      HH.select
        [ HP.id id, HP.class_ Css.twoThird, HE.onValueChange (set <<< SS.Country) ]
        $ [ HH.option
              [ HP.value "", HP.disabled true, HP.selected (isNothing selected) ]
              [ HH.text "Please choose a country" ]
          ]
        <> map countryEntry Iso3166.countries

  -- Renders a drop-down list of subdivisions for the given country.
  renderSubdivisionSelect ::
    { id :: String
    , country :: Maybe SS.Country
    , set :: (Maybe SS.Subdivision -> Action)
    , selected :: Maybe SS.Subdivision
    } ->
    H.ComponentHTML Action slots m
  renderSubdivisionSelect { id, country, set, selected } = case country of
    Nothing ->
      HH.select
        [ HP.id id, HP.class_ Css.twoThird ]
        [ HH.option
            [ HP.value "", HP.disabled true, HP.selected true ]
            [ HH.text "Please choose a country" ]
        ]
    Just (SS.Country countryCode) ->
      let
        subdivEntry c =
          let
            -- Drop country code and '-'.
            code = S.drop 3 c.code
          in
            HH.option
              [ HP.value code, HP.selected (Just (SS.Subdivision code) == selected) ]
              [ HH.text c.name ]

        subdivs = A.fromFoldable (subdivEntry <$> Iso3166.countrySubdivisions countryCode)

        set' =
          set
            <<< case _ of
                "" -> Nothing
                s -> Just $ SS.Subdivision s
      in
        HH.select
          [ HP.id id, HP.class_ Css.twoThird, HE.onValueChange set' ]
          $ [ HH.option
                [ HP.value "", HP.selected (isNothing selected) ]
                [ HH.text "No state or province" ]
            ]
          <> subdivs

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

  renderAddress baseId (SS.Address address) update =
    let
      opt = fromMaybe ""

      unopt v = if v == "" then Nothing else Just v

      input { id, placeholder, pattern, get, set } =
        let
          fullId = baseId <> "-" <> id
        in
          [ HH.label [ HP.for fullId ] [ HH.text placeholder ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.placeholder placeholder
              , HP.pattern pattern
              , HP.value $ opt get
              , HP.id fullId
              , HP.class_ Css.twoThird
              , HE.onValueChange \v -> update (\a -> set a (unopt v))
              ]
          ]
    in
      [ HH.fieldset_
          [ HH.legend_ [ HH.text "Address" ]
          , HH.div [ HP.classes [ Css.flex, Css.three ] ]
              $ input
                  { id: "line1"
                  , placeholder: "Line 1"
                  , pattern: ".{0,250}"
                  , get: address.line1
                  , set: \a v -> a { line1 = v }
                  }
              <> input
                  { id: "line2"
                  , placeholder: "Line 2"
                  , pattern: ".{0,250}"
                  , get: address.line2
                  , set: \a v -> a { line2 = v }
                  }
              <> input
                  { id: "line3"
                  , placeholder: "Line 3"
                  , pattern: ".{0,250}"
                  , get: address.line3
                  , set: \a v -> a { line3 = v }
                  }
              <> input
                  { id: "postOfficeBox"
                  , placeholder: "P/O Box"
                  , pattern: ".{0,20}"
                  , get: address.postOfficeBox
                  , set: \a v -> a { postOfficeBox = v }
                  }
              <> input
                  { id: "postalCode"
                  , placeholder: "Postal Code"
                  , pattern: ".{0,20}"
                  , get: address.postalCode
                  , set: \a v -> a { postalCode = v }
                  }
              <> input
                  { id: "city"
                  , placeholder: "City"
                  , pattern: ".{0,80}"
                  , get: address.city
                  , set: \a v -> a { city = v }
                  }
              <> input
                  { id: "county"
                  , placeholder: "County"
                  , pattern: ".{0,50}"
                  , get: address.county
                  , set: \a v -> a { county = v }
                  }
              <> [ HH.label [ HP.for "stateOrProvince" ] [ HH.text "State or Province" ]
                , renderSubdivisionSelect
                    { id: "stateOrProvince"
                    , country: address.country
                    , set: \v -> update (\a -> a { stateOrProvince = v })
                    , selected: address.stateOrProvince
                    }
                , HH.label [ HP.for "country" ] [ HH.text "Country" ]
                , renderCountrySelect
                    { id: "country"
                    , set: \v -> update (\a -> a { country = Just v })
                    , selected: address.country
                    }
                ]
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
                , HP.value $ opt contact.name
                , HE.onValueChange \v -> update' \c -> c { name = unopt v }
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

  renderPurchaser (SS.Purchaser purchaser) =
    let
      update f =
        UpdateCustomer
          $ case _ of
              SS.NewCustomer oldCustomer ->
                let
                  SS.Purchaser oldPurchaser = oldCustomer.purchaser
                in
                  SS.NewCustomer $ oldCustomer { purchaser = SS.Purchaser (f oldPurchaser) }
              returnCustomer -> returnCustomer
    in
      [ HH.label [ HP.for "of-purchaser", HP.class_ Css.button ] [ HH.text "Purchaser" ]
      , Widgets.modal "of-purchaser" "Purchaser"
          ( [ HH.label_
                [ HH.text "Corporate Name"
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.required true
                    , HP.value purchaser.corporateName
                    , HE.onValueChange \v -> update _ { corporateName = v }
                    ]
                ]
            , HH.label_
                [ HH.text "Registration Number"
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.required true
                    , HP.placeholder "012345"
                    , HP.value purchaser.registrationNr
                    , HE.onValueChange \v -> update _ { registrationNr = v }
                    ]
                ]
            , HH.label_
                [ HH.text "Tax ID"
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.required true
                    , HP.placeholder "012345"
                    , HP.value purchaser.taxID
                    , HE.onValueChange \v -> update _ { taxID = v }
                    ]
                ]
            , HH.label_
                [ HH.text "Website"
                , HH.input
                    [ HP.type_ HP.InputUrl
                    , HP.required true
                    , HP.placeholder "https://example.org/"
                    , HP.value purchaser.website
                    , HE.onValueChange \v -> update _ { website = v }
                    ]
                ]
            , renderContact "Primary Contact"
                purchaser.contacts.primary
                $ \f -> update (\c -> c { contacts { primary = f c.contacts.primary } })
            , renderContact "Finance Contact"
                purchaser.contacts.finance
                $ \f -> update (\c -> c { contacts { finance = f c.contacts.finance } })
            ]
              <> renderAddress
                  "of-purchaser"
                  purchaser.address
                  (\f -> update (\c -> c { address = let SS.Address addr = c.address in SS.Address (f addr) }))
          )
          [ HH.label
              [ HP.for "of-purchaser", HP.class_ Css.button ]
              [ HH.text "OK" ]
          ]
      ]

  renderSeller (SS.Seller seller) =
    let
      update f =
        UpdateCustomer
          $ case _ of
              SS.NewCustomer oldCustomer ->
                let
                  SS.Seller oldSeller = oldCustomer.seller
                in
                  SS.NewCustomer $ oldCustomer { seller = SS.Seller (f oldSeller) }
              returnCustomer -> returnCustomer

      renderLegalEntityOption (SS.LegalEntity le) =
        HH.option
          [ HP.value le.novaShortName ]
          [ HH.text le.registeredName ]

      sellerOptions = case st.legalEntities of
        Idle ->
          [ HH.option
              [ HP.value "", HP.disabled true, HP.selected true ]
              [ HH.text "No legal entities loaded" ]
          ]
        Loaded (SS.LegalEntities { legalEntities }) ->
          [ HH.option
              [ HP.value "", HP.disabled true, HP.selected true ]
              [ HH.text "Please choose a legal entity" ]
          ]
            <> map renderLegalEntityOption legalEntities
        Loading ->
          [ HH.option
              [ HP.value "", HP.disabled true, HP.selected true ]
              [ HH.text "Loading legal entities…" ]
          ]
        Error _ ->
          [ HH.option
              [ HP.value "", HP.disabled true, HP.selected true ]
              [ HH.text $ "Error loading legal entities" ]
          ]

      actionSetLegalEntity name = case st.legalEntities of
        Loaded (SS.LegalEntities { legalEntities }) ->
          maybe NoOp SetLegalEntity
            $ A.find (\(SS.LegalEntity le) -> name == le.novaShortName) legalEntities
        _ -> NoOp

      defaultCurrencyStr = maybe "" (show <<< _.defaultBankCurrency <<< unwrap) st.legalEntity

      currenciesStr =
        S.joinWith ", " $ map show $ A.fromFoldable
          $ maybe mempty (_.availableCurrencies <<< unwrap) st.legalEntity
    in
      [ HH.label [ HP.for "of-seller", HP.class_ Css.button ] [ HH.text "Seller" ]
      , Widgets.modal "of-seller" "Seller"
          ( [ HH.label_
                [ HH.text "Populate From"
                , HH.select [ HE.onValueChange actionSetLegalEntity ] sellerOptions
                ]
            , HH.hr_
            , HH.label_
                [ HH.text "Legal Entity Name"
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.readOnly true
                    , HP.value seller.name
                    , HE.onValueChange $ \v -> update \s -> s { name = v }
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
          )
          [ HH.label
              [ HP.for "of-seller", HP.class_ Css.button ]
              [ HH.text "OK" ]
          ]
      ]

  renderBillingAccountRef =
    [ HH.label [ HP.for "of-billing-account", HP.class_ Css.button ] [ HH.text "Billing Account" ]
    , Widgets.modal "of-billing-account" "Billing Account"
        [ HH.label_
            [ HH.text "Identifier"
            , HH.input [ HP.type_ HP.InputText, HP.name "billing-account-id" ]
            ]
        ]
        [ HH.label
            [ HP.for "of-billing-account", HP.classes [ Css.button ] ]
            [ HH.text "Save" ]
        , HH.label
            [ HP.for "of-billing-account", HP.classes [ Css.button, Css.dangerous ] ]
            [ HH.text "Cancel" ]
        ]
    ]

handleAction ::
  forall m.
  MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  NoOp -> pure unit
  Initialize customer' -> do
    H.modify_ $ \st -> st { customer = Just customer', legalEntities = Loading }
    legalEntities <- getLegalEntities
    H.modify_ $ \st -> st { legalEntities = legalEntities }
    case legalEntities of
      Error err -> Console.error $ "When fetching legal entities: " <> err
      _ -> pure unit
  UpdateCustomer updater -> do
    { customer } <- H.modify $ \st -> st { customer = map updater st.customer }
    maybe (pure unit) H.raise customer
  SetLegalEntity (SS.LegalEntity le) -> do
    { customer } <-
      H.modify
        $ \st ->
            let
              setBillingCurrency currency = case _ of
                Just (SS.NewCustomer c) ->
                  Just $ SS.NewCustomer
                    $ let
                        SS.Commercial commercial = c.commercial
                      in
                        c
                          { commercial =
                            SS.Commercial
                              $ commercial
                                  { billingCurrency = SS.PricingCurrency currency
                                  }
                          }
                c -> c

              setSeller s = case _ of
                Just (SS.NewCustomer c) -> Just $ SS.NewCustomer $ c { seller = SS.Seller s }
                c -> c
            in
              st
                { legalEntity = Just $ SS.LegalEntity le
                , customer =
                  setBillingCurrency le.defaultBankCurrency
                    $ setSeller
                        { name: le.registeredName
                        , address: le.address
                        , contacts: le.contacts
                        }
                    $ st.customer
                }
    maybe (pure unit) H.raise customer
