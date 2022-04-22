module Sofa.App.EditablePrice (Slot, Input(..), Output(..), proxy, component) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number.Format (fixed, toStringWith)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Component.Modal as Modal
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.BigNumber as BN
import Sofa.Data.Currency as Currency
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
  = { price :: SS.Price
    , currency :: SS.ChargeCurrency
    , readOnly :: Boolean
    }

type Output
  = SS.Price

data DiscountType
  = Absolute
  | Percentage

data EditState
  = Editing DiscountType String
  | Viewing

type State
  = { price :: SS.Price
    , currency :: SS.ChargeCurrency
    , readOnly :: Boolean
    , editState :: EditState
    }

data Action
  = SetEditing
  | SetViewing Event
  | UpdateValue DiscountType String
  | RemoveDiscount
  | CloseEditing

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
  , readOnly: input.readOnly
  , editState: Viewing
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state
  | state.readOnly = HH.span_ (renderPrice state.price state.currency)
  | otherwise = case state.editState of
    Viewing ->
      HH.button
        [ Css.classes
            [ "nectary-btn-secondary"
            , "w-full"
            , "text-stormy-500"
            ]
        , HE.onClick \_ -> SetEditing
        ]
        $ renderPrice state.price state.currency
    Editing typ_ value ->
      let
        inputTag tag =
          HH.div
            [ Css.classes
                [ "absolute"
                , "top-0"
                , "right-0"
                , "h-12"
                , "w-12"
                , "my-0.5"
                , "flex"
                , "items-center"
                , "justify-center"
                , "pointer-events-none"
                ]
            ]
            [ HH.div
                [ Css.classes
                    [ "p-1"
                    , "px-1.5"
                    , "text-sm"
                    , "bg-snow-600"
                    , "rounded"
                    , "shadow-sm"
                    ]
                ]
                [ HH.text tag ]
            ]

        renderContent =
          HH.div [ Css.class_ "max-w-128" ]
            [ HH.p
                [ Css.class_ "my-4" ]
                [ HH.text "Here you can modify the existing price." ]
            , HH.p
                [ Css.class_ "my-4" ]
                [ HH.text "You may add a discount percentage as 10% or 5% OR change the price directly!" ]
            , HH.form
                [ Css.classes [ "grid", "grid-cols-2", "gap-2", "mt-10" ]
                , HE.onSubmit SetViewing
                ]
                [ HH.h4_ [ HH.text "New price" ]
                , HH.h4_ [ HH.text "Discount percentage" ]
                , HH.div [ Css.class_ "relative" ]
                    [ HH.input
                        [ HP.type_ HP.InputNumber
                        , Css.classes
                            [ "nectary-input"
                            , "nectary-input-number"
                            , "w-full"
                            , "pr-12"
                            ]
                        , HP.placeholder "1.25"
                        , HP.step HP.Any
                        , HP.value case typ_ of
                            Absolute -> value
                            Percentage -> ""
                        , HE.onValueInput (UpdateValue Absolute)
                        ]
                    , inputTag (show state.currency)
                    ]
                , HH.div [ Css.class_ "relative" ]
                    [ HH.input
                        [ HP.type_ HP.InputNumber
                        , Css.classes
                            [ "nectary-input"
                            , "nectary-input-number"
                            , "w-full"
                            , "pr-12"
                            ]
                        , HP.placeholder "10"
                        , HP.min 0.0
                        , HP.max 100.0
                        , HP.value case typ_ of
                            Absolute -> ""
                            Percentage -> value
                        , HE.onValueInput (UpdateValue Percentage)
                        ]
                    , inputTag "%"
                    ]
                , HH.div
                    [ Css.classes
                        [ "col-span-2"
                        , "flex"
                        , "gap-x-4"
                        , "justify-end"
                        , "mt-10"
                        ]
                    ]
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        , Css.class_ "nectary-btn-destructive"
                        , HP.enabled
                            let
                              SS.Price { discount } = state.price
                            in
                              isJust discount
                        , HE.onClick \_ -> RemoveDiscount
                        ]
                        [ HH.text "Remove" ]
                    , HH.div [ Css.class_ "grow" ] []
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        , Css.class_ "nectary-btn-secondary"
                        , HE.onClick \_ -> CloseEditing
                        ]
                        [ HH.text "Cancel" ]
                    , HH.button
                        [ HP.type_ HP.ButtonSubmit
                        , Css.class_ "nectary-btn-primary"
                        , HP.enabled (value /= "")
                        ]
                        [ HH.text "Save new price" ]
                    ]
                ]
            ]
      in
        HH.div_
          [ HH.span_ $ renderPrice state.price state.currency
          , Modal.render
              $ Modal.defaultInput
                  { title = HH.text "Price"
                  , closeAction = Just (const CloseEditing)
                  , content = renderContent
                  }
          ]

renderPrice :: forall w i. SS.Price -> SS.ChargeCurrency -> Array (HH.HTML w i)
renderPrice (SS.Price p) (SS.ChargeCurrency currency) = price
  where
  price = case p.discount of
    Nothing -> showPrice p.listPrice
    Just d ->
      [ Tooltip.render
          ( Tooltip.defaultInput
              { text =
                "Discount: "
                  <> showDiscount d
                  <> ". Without discount: "
                  <> Currency.formatter currency p.listPrice
              }
          )
          $ HH.span [ Css.class_ "text-raspberry-500" ]
              (showPrice p.price)
      ]

  showPrice = Widgets.monetaryAmount currency

  showDiscount = case _ of
    SS.DiscountPercentage d -> show d <> "%"
    SS.DiscountAbsolute d -> toStringWith (fixed 3) d

handleAction ::
  forall m.
  MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetEditing ->
    H.modify_ \st ->
      st
        { editState =
          let
            SS.Price { discount } = st.price
          in
            case discount of
              Just (SS.DiscountAbsolute n) -> Editing Absolute (show n)
              Just (SS.DiscountPercentage n) -> Editing Percentage (show n)
              Nothing -> Editing Absolute ""
        }
  CloseEditing -> H.modify_ _ { editState = Viewing }
  RemoveDiscount -> do
    state <-
      H.modify \st ->
        st
          { price =
            let
              SS.Price p = st.price
            in
              SS.Price $ p { price = p.listPrice, discount = Nothing }
          , editState = Viewing
          }
    H.raise state.price
  SetViewing event -> do
    H.liftEffect $ Event.preventDefault event
    state <-
      H.modify \st ->
        st
          { price =
            case st.editState of
              Viewing -> st.price
              Editing typ_ value ->
                SS.Price
                  let
                    SS.Price p = st.price

                    lp = BN.fromNumber p.listPrice

                    num = fromMaybe zero $ BN.fromString value
                  in
                    case typ_ of
                      Absolute ->
                        p
                          { price = BN.toNumber num
                          , discount = Just $ SS.DiscountAbsolute $ BN.toNumber $ num - lp
                          }
                      Percentage ->
                        p
                          { price = BN.toNumber $ lp - lp * (num / BN.fromInt 100)
                          , discount = Just $ SS.DiscountPercentage $ BN.toNumber num
                          }
          , editState = Viewing
          }
    H.raise state.price
  UpdateValue typ_ value ->
    H.modify_ \st ->
      st
        { editState =
          case st.editState of
            Viewing -> Viewing
            Editing _ _ -> Editing typ_ value
        }
