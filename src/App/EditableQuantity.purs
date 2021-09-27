module App.EditableQuantity (Slot, Input(..), Query(..), proxy, component) where

import Prelude
import Data.Estimate (Estimate(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Slot id
  = H.Slot Query Input id

proxy :: Proxy "editableQuantity"
proxy = Proxy

type Input
  = Maybe (Estimate Int)

type Output
  = Maybe (Estimate Int)

data EditState
  = Editing String
  | Viewing

type State
  = { quantity :: Maybe (Estimate Int)
    , editState :: EditState
    }

data Action
  = SetEditing
  | SetViewing Event
  | UpdateContent String

data Query a
  = SetQuantity (Maybe (Estimate Int)) a

component ::
  forall m.
  MonadAff m => H.Component Query Input Output m
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
initialState input =
  { quantity: input
  , editState: Viewing
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = case state.editState of
  Viewing ->
    HH.a
      [ HP.href "javascript:void(0);", HE.onClick \_ -> SetEditing ]
      [ HH.text $ showQuantity state.quantity ]
  Editing value ->
    HH.form [ HE.onSubmit SetViewing ]
      [ HH.input
          [ HP.placeholder $ "1000 | ~1000 | ?"
          , HP.pattern """~?\d+|\?"""
          , HP.value value
          , HE.onValueInput UpdateContent
          ]
      ]

parseQuantity :: String -> Maybe (Estimate Int)
parseQuantity str = case String.splitAt 1 str of
  { before: "~", after: s } -> Estimate <$> Int.fromString s
  _ -> Exact <$> Int.fromString str

showQuantity :: Maybe (Estimate Int) -> String
showQuantity = maybe "?" show

handleAction ::
  forall m.
  MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetEditing -> H.modify_ $ \st -> st { editState = Editing $ showQuantity st.quantity }
  SetViewing event -> do
    H.liftEffect $ Event.preventDefault event
    st' <-
      H.modify
        $ \st ->
            st
              { quantity =
                case st.editState of
                  Viewing -> st.quantity
                  Editing q -> case parseQuantity q of
                    Nothing -> st.quantity
                    Just n -> Just n
              , editState = Viewing
              }
    H.raise st'.quantity
  UpdateContent content -> do
    H.modify_
      $ \st ->
          st
            { editState =
              case st.editState of
                Viewing -> Viewing
                Editing _ -> Editing content
            }

handleQuery :: forall action output a m. MonadAff m => Query a -> H.HalogenM State action () output m (Maybe a)
handleQuery = case _ of
  SetQuantity quantity next -> do
    H.modify_ \st ->
      st
        { editState = Viewing
        , quantity = quantity
        }
    pure $ Just next
