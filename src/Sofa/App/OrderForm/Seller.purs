-- | The seller component of the order form.
module Sofa.App.OrderForm.Seller (Slot, Input(..), Output(..), Query(..), proxy, component) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm.SelectLegalEntity as SelectLegalEntity
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (focusElementByRef)
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))

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
    , acceptedLegalEntity :: Maybe SS.LegalEntity -- ^ The latest accepted legal entity.
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
    , acceptedLegalEntity: Nothing
    , acceptedSeller: Nothing
    , readOnly: false
    , open: false
    }
  Just { seller, readOnly } ->
    { legalEntity: Nothing
    , seller: Just seller
    -- TODO: Fetch legal entity and populate this field.
    , acceptedLegalEntity: Nothing
    , acceptedSeller: Just seller
    , readOnly
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

renderSummary :: forall slots m. State -> H.ComponentHTML Action slots m
renderSummary st = case st.acceptedSeller of
  Just (SS.Seller { registeredName }) -> btn okClasses registeredName
  Nothing -> btn badClasses "Select …"
  where
  btn classes txt = HH.button [ HP.classes classes, HE.onClick $ \_ -> OpenDetails ] [ HH.text txt ]

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
    , Widgets.modal modalToolbar $ renderBody st.seller
    ]
  where
  modalToolbar = [ Widgets.modalCloseBtn (\_ -> CancelAndCloseDetails) ]

  renderBody sellerOpt =
    let
      defaultCurrency = HH.text $ maybe "" (show <<< _.defaultBankCurrency <<< unwrap) st.legalEntity

      subtleComma = HH.span [ HP.class_ (Css.c "text-gray-400") ] [ HH.text ", " ]

      currencies =
        A.intersperse subtleComma
          $ map (HH.text <<< show)
          $ A.fromFoldable
          $ maybe mempty (_.availableCurrencies <<< unwrap) st.legalEntity

      renderSellerData (SS.Seller seller) =
        HH.div
          [ HP.classes
              [ Css.c "w-full"
              , Css.c "min-w-128"
              , Css.c "flex"
              , Css.c "flex-col"
              , Css.c "space-y-4"
              ]
          ]
          $ [ if st.readOnly then
                HH.text ""
              else
                HH.slot SelectLegalEntity.proxy unit SelectLegalEntity.component absurd ChooseLegalEntity
            , HH.div_
                [ renderSmallTitle "Registered Name"
                , HH.div [ HP.classes [ Css.c "ml-2", Css.c "text-2xl" ] ] [ HH.text seller.registeredName ]
                ]
            , HH.div [ HP.class_ (Css.c "flex") ]
                [ HH.div [ HP.class_ (Css.c "w-1/2") ]
                    [ renderSmallTitle "Default Bank Currency"
                    , HH.div [ HP.classes [ Css.c "ml-2", Css.c "text-2xl" ] ] [ defaultCurrency ]
                    ]
                , HH.div [ HP.class_ (Css.c "w-1/2") ]
                    [ renderSmallTitle "Available Currencies"
                    , HH.div [ HP.classes [ Css.c "ml-2" ] ] currencies
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
            , HH.div [ HP.classes [ Css.c "flex", Css.c "space-x-5" ] ] bottomButtons
            ]

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
              , HP.enabled (isJust st.seller)
              , HE.onClick \_ -> AcceptAndCloseDetails
              ]
              [ HH.text "OK" ]
          ]
    in
      renderSellerData $ fromMaybe emptySeller sellerOpt

  renderSmallTitle t = HH.div [ HP.class_ (Css.c "sofa-small-title") ] [ HH.text t ]

  renderContact label (SS.Contact contact) =
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
      HH.div_
        [ renderSmallTitle label
        , HH.div [ HP.classes [ Css.c "ml-2", Css.c "text-lg" ] ]
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
    focusElementByRef okBtnLabel
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
          { acceptedLegalEntity = st.legalEntity
          , acceptedSeller = st.seller
          , open = false
          }
    case st'.acceptedSeller of
      Nothing -> pure unit
      Just seller -> H.raise seller
  CancelAndCloseDetails ->
    H.modify_ \st ->
      st
        { legalEntity = st.acceptedLegalEntity
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