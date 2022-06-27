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
  = { value :: Maybe String
    , readOnly :: Boolean
    }

type Output
  = Maybe String

type State
  = Input

data Action
  = SetValue String

component ::
  forall query f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState = identity

render ::
  forall slots f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action slots m
render st
  | st.readOnly =
    HH.div
      [ Css.classes
          [ "w-96"
          , "min-h-[5rem]"
          , "max-h-[12rem]"
          , "px-3"
          , "py-2"
          , "my-0.5"
          , "rounded"
          , "bg-snow-100"
          , "overflow-scroll"
          ]
      ]
      [ HH.text $ fromMaybe " " st.value ]
  | otherwise =
    HH.textarea
      [ HP.value $ fromMaybe "" st.value
      , Css.classes [ "nectary-textarea", "w-96" ]
      , HE.onValueChange SetValue
      ]

handleAction ::
  forall slots f m.
  MonadAff m =>
  CredentialStore f m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  SetValue text -> do
    st' <- H.modify \st -> st { value = Just text }
    H.raise st'.value
