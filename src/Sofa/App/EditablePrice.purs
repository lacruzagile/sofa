module Sofa.App.EditablePrice (Slot, Input(..), Output(..), proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number.Format (fixed, toStringWith)
import Data.String (Pattern(..), stripSuffix)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Css as Css
import Sofa.Data.BigNumber as BN
import Sofa.Data.SmartSpec as SS
import Sofa.Widgets as Widgets
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
      [ HP.href "javascript:void(0);"
      , HP.classes
          [ Css.c "underline"
          , Css.c "underline-offset-4"
          , Css.c "decoration-honey-500"
          ]
      , HE.onClick \_ -> SetEditing
      ]
      $ renderPrice state.price state.currency
  Editing value ->
    HH.form [ HE.onSubmit SetViewing ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.classes [ Css.c "bg-transparent", Css.c "border" ]
          , HP.pattern """\d+(\.\d+)?%?"""
          , HP.placeholder $ "E.g. 80 or 10%"
          , HP.autofocus state.initial
          , HP.value value
          , HE.onValueInput UpdateContent
          ]
      ]
  where
  renderPrice :: SS.Price -> SS.ChargeCurrency -> Array (H.ComponentHTML Action slots m)
  renderPrice (SS.Price p) (SS.ChargeCurrency currency) = case p.discount of
    Nothing -> price
    Just d -> price <> [ discount d ]
    where
    price = case p.discount of
      Nothing -> showPrice p.listPrice
      Just _ -> [ HH.span [ HP.class_ (Css.c "text-raspberry-500") ] (showPrice p.price) ]

    showPrice = Widgets.monetaryAmount currency

    discount d = HH.small_ [ HH.text $ " (" <> showDiscount d <> ")" ]

    showDiscount = case _ of
      SS.DiscountPercentage d -> show d <> "%"
      SS.DiscountAbsolute d -> toStringWith (fixed 3) d

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

                      percentNumStr = stripSuffix (Pattern "%") c

                      numStr = fromMaybe c percentNumStr
                    in
                      SS.Price
                        $ case BN.fromString numStr of
                            Nothing -> p { price = p.listPrice, discount = Nothing }
                            Just n ->
                              let
                                isPercent = isJust percentNumStr

                                lp = BN.fromNumber p.listPrice
                              in
                                if isPercent && n /= zero then
                                  p
                                    { price = BN.toNumber $ lp - lp * (n / BN.fromInt 100)
                                    , discount = Just $ SS.DiscountPercentage $ BN.toNumber n
                                    }
                                else if not isPercent && n /= lp then
                                  p
                                    { price = BN.toNumber n
                                    , discount = Just $ SS.DiscountAbsolute $ BN.toNumber $ n - lp
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
