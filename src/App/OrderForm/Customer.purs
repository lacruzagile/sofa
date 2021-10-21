module App.OrderForm.Customer (Slot, Output, proxy, component) where

import Prelude
import App.Requests (getLegalEntities)
import Css as Css
import Data.Array as A
import Data.Loadable (Loadable(..))
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
import Effect.Aff.Class (class MonadAff)
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
    , legalEntities :: Loadable SS.LegalEntities
    }

data Action
  = NoOp
  | Initialize SS.Customer
  | UpdateCustomer (SS.Customer -> SS.Customer)
  | PopulateSeller SS.Le

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
initialState customer = { customer, legalEntities: Idle }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render st =
  HH.div [ HP.classes [ Css.flex, Css.two ] ]
    [ HH.div [ HP.class_ Css.one ] renderSelectCustomerType
    , HH.div [ HP.class_ Css.one ] (renderCustomerDetails st.customer)
    ]
  where
  countryPattern = "[A-Z]{2}(-[0-9A-Z]{1,3})?"

  renderSelectCustomerType =
    [ HH.button
        [ HE.onClick \_ ->
            Initialize
              $ SS.NewCustomer
                  { commercial:
                      SS.Commercial
                        { billingOption: SS.Prepay
                        , contractTerm: SS.Ongoing
                        , paymentCurrency: SS.Currency ""
                        , priceCurrency: SS.Currency ""
                        }
                  , purchaser:
                      SS.Purchaser
                        { address: SS.Address ""
                        , contacts:
                            { primary: SS.Contact { email: "", name: "", phone: "" }
                            , finance: SS.Contact { email: "", name: "", phone: "" }
                            }
                        , corporateName: ""
                        , country: ""
                        , registrationNr: ""
                        , taxID: ""
                        , website: ""
                        }
                  , seller:
                      SS.Seller
                        { contacts:
                            { primary: SS.Contact { email: "", name: "", phone: "" }
                            , finance: SS.Contact { email: "", name: "", phone: "" }
                            , support: SS.Contact { email: "", name: "", phone: "" }
                            }
                        , legalEntity:
                            SS.LegalEntity
                              { name: ""
                              , address: SS.Address ""
                              , country: ""
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

  renderCurrency legend (SS.Currency code) update =
    HH.div_
      [ HH.label_ [ HH.text legend ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.required true
          , HP.pattern "[A-Z]{3}"
          , HP.placeholder "Currency (e.g. EUR)"
          , HE.onValueChange $ \c -> update \(SS.Currency _) -> SS.Currency c
          , HP.value code
          ]
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
        _ -> update (_ { billingOption = SS.PostPay })

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
                      [ HP.value "PostPay"
                      , HP.selected (commercial.billingOption == SS.PostPay)
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
                  $ \f -> update (\c -> c { paymentCurrency = f c.paymentCurrency })
              , renderCurrency
                  "Price Currency"
                  commercial.priceCurrency
                  $ \f -> update (\c -> c { priceCurrency = f c.priceCurrency })
              ]
          ]
          [ HH.label
              [ HP.for "of-commercial", HP.class_ Css.button ]
              [ HH.text "OK" ]
          ]
      ]

  renderAddress (SS.Address address) onValueChange =
    [ HH.fieldset_
        [ HH.legend_ [ HH.text "Address" ]
        , HH.textarea
            [ HP.rows 4
            , HP.placeholder "Address"
            , HP.value address
            , HE.onValueChange onValueChange
            ]
        ]
    ]

  renderContact label (SS.Contact contact) update =
    let
      update' f = update \(SS.Contact oldContact) -> SS.Contact $ f oldContact
    in
      HH.fieldset_
        [ HH.legend_ [ HH.text label ]
        , HH.div [ HP.class_ Css.three ]
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.placeholder "Name"
                , HP.required true
                , HP.value contact.name
                , HE.onValueChange \v -> update' \c -> c { name = v }
                ]
            , HH.input
                [ HP.type_ HP.InputEmail
                , HP.placeholder "Email"
                , HP.required true
                , HP.value contact.email
                , HE.onValueChange \v -> update' \c -> c { email = v }
                ]
            , HH.input
                [ HP.type_ HP.InputTel
                , HP.placeholder "Phone No"
                , HP.required true
                , HP.value contact.phone
                , HE.onValueChange \v -> update' \c -> c { phone = v }
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
                [ HH.text "Country"
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.required true
                    , HP.pattern countryPattern
                    , HP.placeholder "Country (e.g. DE, US-AL)"
                    , HP.value purchaser.country
                    , HE.onValueChange \v -> update _ { country = v }
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
              <> renderAddress purchaser.address (\v -> update (_ { address = SS.Address v }))
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

      SS.LegalEntity legalEntity = seller.legalEntity

      legalEntityProps get set =
        [ HP.value $ get legalEntity
        , HE.onValueChange $ \v -> update \s -> s { legalEntity = SS.LegalEntity $ set legalEntity v }
        ]

      sellerOptions = case st.legalEntities of
        Loaded (SS.LegalEntities { legalEntities }) ->
          [ HH.option
              [ HP.value "", HP.disabled true, HP.selected true ]
              [ HH.text "Please choose a legal entity" ]
          ]
            <> map (\(SS.Le le) -> HH.option [ HP.value le.novaShortName ] [ HH.text le.registeredName ]) legalEntities
        _ ->
          [ HH.option
              [ HP.value "", HP.disabled true, HP.selected true ]
              [ HH.text "No legal entities loaded" ]
          ]

      actionPopulateSeller name = case st.legalEntities of
        Loaded (SS.LegalEntities { legalEntities }) -> maybe NoOp PopulateSeller $ A.find (\(SS.Le le) -> name == le.novaShortName) legalEntities
        _ -> NoOp
    in
      [ HH.label [ HP.for "of-seller", HP.class_ Css.button ] [ HH.text "Seller" ]
      , Widgets.modal "of-seller" "Seller"
          ( [ HH.label_
                [ HH.text "Populate From"
                , HH.select [ HE.onValueChange actionPopulateSeller ] sellerOptions
                ]
            , HH.label_
                [ HH.text "Legal Entity Name"
                , HH.input
                    $ [ HP.type_ HP.InputText
                      , HP.required true
                      ]
                    <> legalEntityProps (\le -> le.name) (\le v -> le { name = v })
                ]
            , HH.label_
                [ HH.text "Legal Entity Country"
                , HH.input
                    $ [ HP.type_ HP.InputText
                      , HP.pattern countryPattern
                      , HP.placeholder "Country (e.g. DE, US-AL)"
                      , HP.required true
                      ]
                    <> legalEntityProps (\le -> le.country) (\le v -> le { country = v })
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
              <> renderAddress
                  legalEntity.address
                  ( \v ->
                      update
                        ( \s ->
                            s
                              { legalEntity =
                                let
                                  SS.LegalEntity le = s.legalEntity
                                in
                                  SS.LegalEntity $ le { address = SS.Address v }
                              }
                        )
                  )
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
    legalEntities <- H.liftAff getLegalEntities
    H.modify_ $ \st -> st { legalEntities = legalEntities }
  UpdateCustomer updater -> do
    { customer } <- H.modify $ \st -> st { customer = map updater st.customer }
    maybe (pure unit) H.raise customer
  PopulateSeller (SS.Le le) ->
    H.modify_
      $ \st ->
          let
            SS.LeAddress address = le.address

            setSeller s = case st.customer of
              Just (SS.NewCustomer c) -> Just $ SS.NewCustomer $ c { seller = SS.Seller s }
              _ -> st.customer
          in
            st
              { customer =
                setSeller
                  { contacts:
                      { primary: SS.Contact { email: "", name: "", phone: "" }
                      , finance: SS.Contact { email: le.financeContact, name: "", phone: "" }
                      , support: SS.Contact { email: le.supportContact, name: "", phone: "" }
                      }
                  , legalEntity:
                      SS.LegalEntity
                        { name: le.registeredName
                        , address:
                            SS.Address $ address.street
                              <> "\n"
                              <> address.postalCode
                              <> " "
                              <> address.city
                              <> "\n"
                              <> address.state
                        , country: address.country
                        }
                  }
              }
