module App.EditableSegmentPrice (Slot, Input(..), proxy, component) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Number as Number
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
  = forall query. H.Slot query Input id

proxy :: Proxy "editableSegmentPrice"
proxy = Proxy

type Input
  = SS.PricePerSegment

type Output
  = SS.PricePerSegment

data EditState
  = Editing String
  | Viewing

type State
  = { price :: SS.PricePerSegment
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
  { price: input
  , editState: Viewing
  , initial: true
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = case state.editState of
  Viewing ->
    HH.a
      [ HE.onClick \_ -> SetEditing ]
      $ renderPricePerSegment state.price
      <> [ HH.sup_ [ HH.small_ [ HH.text "🖊" ] ] ]
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
  renderPricePerSegment :: SS.PricePerSegment -> Array (H.ComponentHTML Action slots m)
  renderPricePerSegment (SS.PricePerSegment p) = case p.discount of
    Nothing -> [ price, segment ]
    Just d -> [ price, discount d, segment ]
    where
    price =
      maybe
        (HH.text $ show $ p.listPrice)
        (HH.span [ HP.style "color:red" ] <<< A.singleton <<< HH.text <<< show)
        p.salesPrice

    segment = HH.text $ " [" <> show p.minimum <> "," <> maybe "∞" show p.exclusiveMaximum <> ")"

    discount d = HH.small_ [ HH.text $ " (" <> showDiscount d <> ")" ]

    showDiscount = case _ of
      SS.DiscountPercentage d -> show d <> "%"
      SS.DiscountAbsolute d -> show d

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
                    SS.PricePerSegment p = st.price
                  in
                    case p of
                      { discount: Just (SS.DiscountPercentage percent) } -> show percent <> "%"
                      { salesPrice: Just salesPrice } -> show salesPrice
                      { listPrice } -> show listPrice
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
                      SS.PricePerSegment p = st.price
                    in
                      SS.PricePerSegment
                        $ case Number.fromString c of
                            Nothing -> p { salesPrice = Nothing, discount = Nothing }
                            Just n ->
                              let
                                isPercent = isJust (stripSuffix (Pattern "%") c)
                              in
                                if isPercent && n /= 0.0 then
                                  p
                                    { salesPrice = Just $ p.listPrice - p.listPrice * (n / 100.0)
                                    , discount = Just $ SS.DiscountPercentage n
                                    }
                                else if not isPercent && n /= p.listPrice then
                                  p
                                    { salesPrice = Just n
                                    , discount = Just $ SS.DiscountAbsolute $ n - p.listPrice
                                    }
                                else
                                  p { salesPrice = Nothing, discount = Nothing }
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
