module App.EditablePrice (Slot, Input(..), Output(..), proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), isJust)
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Data.SmartSpec as SS
import Data.String (Pattern(..), stripSuffix)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "editablePrice"
proxy = Proxy

type Input
  = { price :: SS.Price, currency :: SS.ChargeCurrency }

type Output
  = SS.Price

data EditState
  = Editing String
  | Viewing

type State
  = { price :: SS.Price
    , currency :: SS.ChargeCurrency
    , editState :: EditState
    , initial :: Boolean
    }

data Action
  = SetEditing
  | SetViewing Event
  | UpdateContent String

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
initialState input =
  { price: input.price
  , currency: input.currency
  , editState: Viewing
  , initial: true
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = case state.editState of
  Viewing ->
    HH.a
      [ HP.href "javascript:void(0);", HE.onClick \_ -> SetEditing ]
      $ renderPrice state.price state.currency
  Editing value ->
    HH.form [ HE.onSubmit SetViewing ]
      [ HH.input
          [ HP.pattern """\d+(\.\d+)?%?"""
          , HP.placeholder $ "E.g. 80 or 10%"
          , HP.autofocus state.initial
          , HP.value value
          , HE.onValueInput UpdateContent
          ]
      ]
  where
  renderPrice :: SS.Price -> SS.ChargeCurrency -> Array (H.ComponentHTML Action slots m)
  renderPrice (SS.Price p) currency = case p.discount of
    Nothing -> [ price ]
    Just d -> [ price, discount d ]
    where
    price = case p.discount of
      Nothing -> HH.text $ showPrice p.listPrice
      Just _ -> HH.span [ HP.style "color:red" ] [ HH.text $ showPrice p.price ]

    showPrice priceToShow = showMonetary priceToShow <> " " <> show currency

    discount d = HH.small_ [ HH.text $ " (" <> showDiscount d <> ")" ]

    showDiscount = case _ of
      SS.DiscountPercentage d -> show d <> "%"
      SS.DiscountAbsolute d -> showMonetary d

showMonetary :: Number -> String
showMonetary = toStringWith (fixed 3)

handleAction ::
  forall m.
  MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetEditing ->
    H.modify_
      $ \st ->
          st
            { editState =
              Editing
                $ let
                    SS.Price p = st.price
                  in
                    case p of
                      { discount: Just (SS.DiscountPercentage percent) } -> show percent <> "%"
                      { discount: Just (SS.DiscountAbsolute _) } -> show p.price
                      _ -> show p.listPrice
            }
  SetViewing event -> do
    H.liftEffect $ Event.preventDefault event
    st' <-
      H.modify
        $ \st ->
            st
              { price =
                case st.editState of
                  Viewing -> st.price
                  Editing c ->
                    let
                      SS.Price p = st.price
                    in
                      SS.Price
                        $ case Number.fromString c of
                            Nothing -> p { price = p.listPrice, discount = Nothing }
                            Just n ->
                              let
                                isPercent = isJust (stripSuffix (Pattern "%") c)
                              in
                                if isPercent && n /= 0.0 then
                                  p
                                    { price = p.listPrice - p.listPrice * (n / 100.0)
                                    , discount = Just $ SS.DiscountPercentage n
                                    }
                                else if not isPercent && n /= p.listPrice then
                                  p
                                    { price = n
                                    , discount = Just $ SS.DiscountAbsolute $ n - p.listPrice
                                    }
                                else
                                  p { price = p.listPrice, discount = Nothing }
              , editState = Viewing
              }
    H.raise st'.price
  UpdateContent content -> do
    H.modify_
      $ \st ->
          st
            { editState =
              case st.editState of
                Viewing -> Viewing
                Editing _ -> Editing content
            , initial = false
            }
