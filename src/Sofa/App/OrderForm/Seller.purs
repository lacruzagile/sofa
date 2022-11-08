-- | The seller component of the order form.
module Sofa.App.OrderForm.Seller (Slot, Input(..), Output(..), Query(..), proxy, component) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe, maybe')
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm.SelectLegalEntity as SelectLegalEntity
import Sofa.App.Requests as Requests
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable as Loadable
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

data Input
  = InputSeller
    { seller :: SS.Seller
    , readOnly :: Boolean
    }
  | InputRegisteredName
    { registeredName :: String
    , readOnly :: Boolean
    }
  | InputLegalEntity
    {
      novaShortName :: String
    , readOnly :: Boolean
    }
  | InputNothing

type Output
  = SS.Seller

type State
  = { registeredName :: Maybe String -- ^ The seller's registered name.
    , legalEntity :: Maybe SS.LegalEntity -- ^ The currently chosen legal entity.
    , seller :: Maybe SS.Seller -- ^ The currently chosen seller.
    , acceptedLegalEntity :: Maybe SS.LegalEntity -- ^ The latest accepted legal entity.
    , acceptedSeller :: Maybe SS.Seller -- ^ The latest accepted seller.
    , readOnly :: Boolean
    , open :: Boolean -- ^ Whether the details modal is open.
    , novaShortName :: Maybe String
    }

data Action
  = Initialize
  | ChooseLegalEntity (Maybe SS.LegalEntity)
 {-  | UpdateContactPrimary (SS.Contact -> SS.Contact)
  | UpdateContactFinance (SS.Contact -> SS.Contact)
  | UpdateContactSupport (SS.Contact -> SS.Contact) -}
  | OpenDetails
  | AcceptAndCloseDetails
  | CancelAndCloseDetails

data Query a
  = SetSeller (Maybe SS.Seller) a

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
            { handleAction = handleAction
            , handleQuery = handleQuery
            , initialize = Just Initialize
            }
    }

initialState :: Input -> State
initialState input = case input of
  InputSeller { seller, readOnly } ->
    { registeredName:
        let SS.Seller { registeredName } = seller in Just registeredName
    , legalEntity: Nothing
    , seller: Just seller
    , acceptedLegalEntity: Nothing
    , acceptedSeller: Just seller
    , readOnly
    , open: false
    , novaShortName: Nothing
    }
  InputRegisteredName { registeredName, readOnly } ->
    { registeredName: Just registeredName
    , legalEntity: Nothing
    , seller: Nothing
    , acceptedLegalEntity: Nothing
    , acceptedSeller: Nothing
    , readOnly
    , open: false
    , novaShortName: Nothing
    }
  InputLegalEntity { novaShortName, readOnly } ->
    { registeredName: Nothing
    , legalEntity: Nothing
    , seller: Nothing
    , acceptedLegalEntity: Nothing
    , acceptedSeller: Nothing
    , readOnly
    , open: false
    , novaShortName: Just novaShortName
    }
  InputNothing ->
    { registeredName: Nothing
    , legalEntity: Nothing
    , seller: Nothing
    , acceptedLegalEntity: Nothing
    , acceptedSeller: Nothing
    , readOnly: false
    , open: false
    , novaShortName: Nothing
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

renderSummary :: forall slots m. State -> H.ComponentHTML Action slots m
renderSummary st = case st.acceptedSeller of
  Just (SS.Seller { registeredName }) -> btn okClasses registeredName
  Nothing -> btn badClasses "Select …"
  where
  btn classes txt =
    HH.button
      [ HP.classes classes, HE.onClick $ \_ -> OpenDetails ]
      [ HH.text txt ]

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
            { title = HH.text "Legal entity"
            , content = renderContent st.seller
            }
    ]
  where
  notAvailableIfNull = case _ of
    [] -> HH.div [ Css.class_ "text-stormy-300" ] [ HH.text "None" ]
    elems -> HH.div_ elems

  renderContent sellerOpt =
    let
      defaultCurrency =
        notAvailableIfNull
          $ case st.legalEntity of
              Nothing -> []
              Just (SS.LegalEntity le) -> [ HH.text (show le.defaultBankCurrency) ]

      subtleComma = HH.span [ Css.class_ "text-stormy-300" ] [ HH.text ", " ]

      currencies =
        notAvailableIfNull
          $ A.intersperse subtleComma
          $ map (HH.text <<< show)
          $ A.fromFoldable
          $ maybe mempty (_.availableCurrencies <<< unwrap) st.legalEntity

      renderSellerData (SS.Seller seller) =
        HH.div [ Css.classes [ "flex", "flex-col", "gap-y-4" ] ]
          [ if st.readOnly then
              HH.text ""
            else
              HH.slot SelectLegalEntity.proxy unit SelectLegalEntity.component absurd ChooseLegalEntity
          , if seller.registeredName == "" then
              HH.text ""
            else
              HH.div
                [ Css.classes
                    [ "w-full"
                    , "p-8"
                    , "min-w-128"
                    , "grid"
                    , "grid-cols-[12rem_auto]"
                    , "gap-4"
                    , "rounded"
                    , "bg-snow-500"
                    ]
                ]
                $ [ HH.h3 [ Css.class_ "col-span-2" ] [ HH.text seller.registeredName ]
                  , renderSmallTitle "Bank currency"
                  , defaultCurrency
                  , renderSmallTitle "Available currencies"
                  , currencies
                  {- , renderSmallTitle "Primary Contact"
                  , renderContact seller.contacts.primary
                  , renderSmallTitle "Finance Contact"
                  , renderContact seller.contacts.finance
                  , renderSmallTitle "Support Contact"
                  , renderContact seller.contacts.support -}
                  , HH.h4 [ Css.class_ "col-span-2" ] [ HH.text "Address" ]
                  ]
                <> Widgets.address seller.address
          , HH.div [ Css.classes [ "flex", "space-x-5" ] ] bottomButtons
          ]

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
              , HP.enabled (isJust st.seller)
              , HE.onClick \_ -> AcceptAndCloseDetails
              ]
              [ HH.text "OK" ]
          ]
    in
      renderSellerData $ fromMaybe emptySeller sellerOpt

  renderSmallTitle t = HH.h4_ [ HH.text t ]

{-   renderContact (SS.Contact contact) =
    let
      opt = case _ of
        Nothing -> []
        Just "" -> []
        Just val -> [ HH.text val ]

      subtleSlash = HH.span [ Css.class_ "text-stormy-300" ] [ HH.text " / " ]
    in
      notAvailableIfNull
        $ A.intersperse subtleSlash
        $ opt contact.displayName
        <> opt contact.email
        <> opt contact.phone -}

toSeller :: SS.LegalEntity -> SS.Seller
toSeller (SS.LegalEntity le) =
  SS.Seller
    { sellerId: Nothing
    , registeredName: le.registeredName
    , novaShortName: le.novaShortName
    , address: le.address
    , contacts: le.contacts
    }

handleAction ::
  forall slots m.
  MonadAff m => Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Initialize -> do
    { registeredName } <- H.get
    case registeredName of
      Nothing -> pure unit
      Just regName -> do
        result <- H.lift $ Requests.getLegalEntity regName
        let
          legalEntity = Loadable.toMaybe result

          seller = toSeller <$> legalEntity
        H.modify_
          _
            { legalEntity = legalEntity
            , acceptedLegalEntity = legalEntity
            , seller = seller
            , acceptedSeller = seller
            }
        maybe' pure H.raise seller
    { novaShortName } <- H.get
    case novaShortName of
      Nothing -> pure unit
      Just name -> do
        result <- H.lift $ Requests.getLegalEntityByShortName name
        let
          legalEntity = Loadable.toMaybe result

          seller = toSeller <$> legalEntity
        H.modify_
          _
            { legalEntity = legalEntity
            , acceptedLegalEntity = legalEntity
            , seller = seller
            , acceptedSeller = seller
            }
        maybe' pure H.raise seller
  ChooseLegalEntity legalEntity -> do
    H.modify_
      _
        { legalEntity = legalEntity
        , seller = toSeller <$> legalEntity
        }
    -- Switch focus to OK button.
    focusElementByRef okBtnLabel
  {- UpdateContactPrimary update ->
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
      H.modify_ $ \st -> st { seller = setContact <$> st.seller } -}
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
