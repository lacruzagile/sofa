-- | Provides a page used to temporarily debug the Salesforce input values.
module Sofa.App.SalesforceDebugPage (Slot, Input(..), component) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Sofa.App.OrderForm.Buyer as Buyer
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Deployment (SalesforcePageData(..))
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

type Slots
  = ( buyer :: Buyer.Slot Unit
    )

type Input
  = SalesforcePageData

type State
  = Input

component ::
  forall query output f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query Input output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

render ::
  forall action f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML action Slots m
render = case _ of
  SfPageOrderForm { buyer, contacts, billingAccountId } ->
    HH.div [ Css.classes [ "grid", "grid-cols-2", "gap-5" ] ]
      [ HH.strong_ [ HH.text "Buyer" ]
      , HH.slot_
          (Proxy :: Proxy "buyer")
          unit
          Buyer.component
          ( Just
              { buyer: buyer
              , buyerAvailableContacts: Just contacts
              , readOnly: false
              }
          )
      , HH.strong_ [ HH.text "Platform account ID" ]
      , HH.div_ [ HH.text billingAccountId ]
      ]
  SfPageCustomerOrderList { crmAccountId } ->
    HH.div_
      [ HH.text "Listing orders for customer "
      , HH.text $ show crmAccountId
      ]
  SfPageUserOrderList ->
    HH.div_
      [ HH.text "Listing orders visible for logged in user"
      ]
