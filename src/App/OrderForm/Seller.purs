-- | The seller component of the order form.
module App.OrderForm.Seller (Slot, Input(..), Output(..), Query(..), proxy, component) where

import Prelude
import App.OrderForm.SelectLegalEntity as SelectLegalEntity
import Css as Css
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.SmartSpec as SS
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlUtils (focusElementByQuery)
import Type.Proxy (Proxy(..))
import Widgets as Widgets

type Slot id
  = H.Slot Query Output id

proxy :: Proxy "seller"
proxy = Proxy

type Slots
  = ( selectLegalEntity :: SelectLegalEntity.Slot Unit )

type Input
  = Maybe
      { seller :: SS.Seller
      , readOnly :: Boolean
      }

type Output
  = SS.Seller

type State
  = { legalEntity :: Maybe SS.LegalEntity -- ^ The currently chosen legal entity.
    , seller :: Maybe SS.Seller -- ^ The currently chosen seller.
    , acceptedSeller :: Maybe SS.Seller -- ^ The latest accepted seller.
    , readOnly :: Boolean
    , open :: Boolean -- ^ Whether the details modal is open.
    }

data Action
  = ChooseLegalEntity (Maybe SS.LegalEntity)
  | UpdateContactPrimary (SS.Contact -> SS.Contact)
  | UpdateContactFinance (SS.Contact -> SS.Contact)
  | UpdateContactSupport (SS.Contact -> SS.Contact)
  | OpenDetails
  | AcceptAndCloseDetails
  | CancelAndCloseDetails

data Query a
  = SetSeller (Maybe SS.Seller) a

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
    { legalEntity: Nothing
    , seller: Nothing
    , acceptedSeller: Nothing
    , readOnly: false
    , open: false
    }
  Just { seller, readOnly } ->
    { legalEntity: Nothing
    , seller: Just seller
    , acceptedSeller: Just seller
    , readOnly
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
renderSummary st = case st.acceptedSeller of
  Just (SS.Seller { registeredName }) -> btn okClasses registeredName
  Nothing -> btn badClasses "None selected"
  where
  btn classes txt = HH.button [ HP.classes classes, HE.onClick $ \_ -> OpenDetails ] [ HH.text txt ]

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
    , Widgets.modal modalToolbar $ renderBody st.seller
    ]
  where
  modalToolbar =
    if st.readOnly then
      [ Widgets.modalCloseBtn (\_ -> CancelAndCloseDetails) ]
    else
      [ HH.slot SelectLegalEntity.proxy unit SelectLegalEntity.component absurd ChooseLegalEntity
      , Widgets.modalCloseBtn (\_ -> CancelAndCloseDetails)
      ]

  renderBody sellerOpt =
    let
      defaultCurrency = HH.text $ maybe "" (show <<< _.defaultBankCurrency <<< unwrap) st.legalEntity

      subtleComma = HH.span [ HP.class_ Css.tw.textGray400 ] [ HH.text ", " ]

      currencies =
        A.intersperse subtleComma
          $ map (HH.text <<< show)
          $ A.fromFoldable
          $ maybe mempty (_.availableCurrencies <<< unwrap) st.legalEntity

      renderSellerData (SS.Seller seller) =
        HH.div [ HP.classes [ Css.tw.wFull, Css.tw.minW128, Css.tw.flex, Css.tw.flexCol, Css.tw.spaceY4 ] ]
          $ [ HH.div_
                [ renderSmallTitle "Registered Name"
                , HH.div [ HP.classes [ Css.tw.ml2, Css.tw.text2Xl ] ] [ HH.text seller.registeredName ]
                ]
            , HH.div [ HP.class_ Css.tw.flex ]
                [ HH.div [ HP.class_ Css.tw.w1_2 ]
                    [ renderSmallTitle "Default Bank Currency"
                    , HH.div [ HP.classes [ Css.tw.ml2, Css.tw.text2Xl ] ] [ defaultCurrency ]
                    ]
                , HH.div [ HP.class_ Css.tw.w1_2 ]
                    [ renderSmallTitle "Available Currencies"
                    , HH.div [ HP.classes [ Css.tw.ml2, Css.tw.textLg ] ] currencies
                    ]
                ]
            , renderContact "Primary Contact" seller.contacts.primary
            , renderContact "Finance Contact" seller.contacts.finance
            , renderContact "Support Contact" seller.contacts.support
            , HH.div_
                [ renderSmallTitle "Address"
                , Widgets.address seller.address
                ]
            , HH.hr_
            , HH.div [ HP.classes [ Css.tw.flex, Css.tw.spaceX5 ] ] bottomButtons
            ]

      bottomButtons
        | st.readOnly =
          [ HH.div [ HP.class_ Css.tw.grow ] []
          , HH.button
              [ HP.class_ Css.btnSky100, HE.onClick \_ -> CancelAndCloseDetails ]
              [ HH.text "Close" ]
          ]
        | otherwise =
          [ HH.div [ HP.class_ Css.tw.grow ] []
          , HH.button
              [ HP.id "seller-ok"
              , HP.class_ Css.btnSky100
              , HP.enabled (isJust st.seller)
              , HE.onClick \_ -> AcceptAndCloseDetails
              ]
              [ HH.text "OK" ]
          , HH.button
              [ HP.class_ Css.btnRed100, HE.onClick \_ -> CancelAndCloseDetails ]
              [ HH.text "Cancel" ]
          ]
    in
      renderSellerData $ fromMaybe emptySeller sellerOpt

  renderSmallTitle t = HH.div [ HP.class_ Css.smallTitle ] [ HH.text t ]

  renderContact label (SS.Contact contact) =
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

handleAction ::
  forall slots m.
  MonadAff m => Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  ChooseLegalEntity legalEntity -> do
    H.modify_
      $ \st ->
          let
            toSeller (SS.LegalEntity le) =
              SS.Seller
                { sellerId: Nothing
                , registeredName: le.registeredName
                , novaShortName: le.novaShortName
                , address: le.address
                , contacts: le.contacts
                }
          in
            st
              { legalEntity = legalEntity
              , seller = toSeller <$> legalEntity
              }
    -- Switch focus to OK button.
    focusElementByQuery "button#seller-ok"
  UpdateContactPrimary update ->
    let
      setContact (SS.Seller s) = SS.Seller $ s { contacts { primary = update s.contacts.primary } }
    in
      H.modify_ $ \st -> st { seller = setContact <$> st.seller }
  UpdateContactFinance update ->
    let
      setContact (SS.Seller s) = SS.Seller $ s { contacts { finance = update s.contacts.finance } }
    in
      H.modify_ $ \st -> st { seller = setContact <$> st.seller }
  UpdateContactSupport update ->
    let
      setContact (SS.Seller s) = SS.Seller $ s { contacts { support = update s.contacts.primary } }
    in
      H.modify_ $ \st -> st { seller = setContact <$> st.seller }
  OpenDetails -> H.modify_ $ \st -> st { open = true }
  AcceptAndCloseDetails -> do
    st' <-
      H.modify \st ->
        st
          { legalEntity = Nothing
          , acceptedSeller = st.seller
          , open = false
          }
    case st'.acceptedSeller of
      Nothing -> pure unit
      Just seller -> H.raise seller
  CancelAndCloseDetails ->
    H.modify_ \st ->
      st
        { legalEntity = Nothing
        , seller = st.acceptedSeller
        , open = false
        }

handleQuery ::
  forall action slots output a m.
  MonadAff m =>
  Query a -> H.HalogenM State action slots output m (Maybe a)
handleQuery = case _ of
  SetSeller seller next -> do
    H.modify_ \st -> st { seller = seller }
    pure $ Just next

emptySeller :: SS.Seller
emptySeller =
  SS.Seller
    { sellerId: Nothing
    , registeredName: ""
    , novaShortName: ""
    , address: SS.emptyAddress
    , contacts: { primary: SS.emptyContact, finance: SS.emptyContact, support: SS.emptyContact }
    }