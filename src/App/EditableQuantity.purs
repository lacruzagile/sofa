module App.EditableQuantity (Slot, Input(..), Query(..), proxy, component) where

import Prelude
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Quantity (Quantity)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Css as Css

type Slot id
  = H.Slot Query Input id

proxy :: Proxy "editableQuantity"
proxy = Proxy

type Input
  = Maybe Quantity

type Output
  = Maybe Quantity

data EditState
  = Editing String
  | Viewing

type State
  = { quantity :: Maybe Quantity
    , editState :: EditState
    }

data Action
  = SetEditing
  | SetViewing Event
  | UpdateContent String

data Query a
  = SetQuantity (Maybe Quantity) a

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
      [ HP.href "javascript:void(0);"
      , HP.classes
          [ Css.tw.underline
          , Css.tw.underlineOffset4
          , Css.tw.decorationSky300
          ]
      , HE.onClick \_ -> SetEditing
      ]
      [ HH.text $ showQuantity state.quantity ]
  Editing value ->
    HH.form [ HE.onSubmit SetViewing ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.classes [ Css.tw.bgTransparent, Css.tw.border ]
          , HP.placeholder $ "1000 | 1k | 1M"
          , HP.pattern """\d+[kM]?"""
          , HP.value value
          , HP.style "max-width:10em"
          , HE.onValueInput UpdateContent
          ]
      ]

parseQuantity :: String -> Maybe Quantity
parseQuantity = parseMagInt
  where
  parseMagInt s = case String.stripSuffix (String.Pattern "M") s of
    Just s' -> (1_000_000 * _) <$> Int.fromString s'
    Nothing -> case String.stripSuffix (String.Pattern "k") s of
      Just s' -> (1_000 * _) <$> Int.fromString s'
      Nothing -> Int.fromString s

showQuantity :: Maybe Quantity -> String
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
