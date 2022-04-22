module Sofa.App.OrderForm.Widget.Textarea (Slot, Output(..), proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetTextarea"
proxy = Proxy

type Input
  = { value :: Maybe String }

type Output
  = Maybe String

type State
  = { value :: Maybe String
    }

data Action
  = SetValue String

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
initialState = identity

render ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action slots m
render st =
  HH.textarea
    [ HP.value $ fromMaybe "" st.value
    , Css.classes [ "nectary-textarea", "w-96" ]
    , HE.onValueChange SetValue
    ]

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  SetValue text -> do
    st' <- H.modify \st -> st { value = Just text }
    H.raise st'.value
