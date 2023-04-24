module Sofa.App.CopyOrderModal
  ( Action(..)
  , Input
  , Slot
  , State
  , buttons
  , component
  , initialState
  )
  where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HHP
import Sofa.App.Requests (copyOrder)
import Sofa.Component.Alert as Alert
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Alerts as Alerts
import Sofa.Component.Icon as Icon
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (reload, uncheck)
import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.KeyboardEvent (KeyboardEvent, toEvent) as KE
import Web.UIEvent.MouseEvent (MouseEvent, toEvent) as Event

data Action
  = OpenModal Event.MouseEvent
  | CloseModal Event.MouseEvent
  | AcceptAndClose Event.MouseEvent
  | SetValue String

type Slot id
  = forall query output.
    H.Slot query output id

type Input
  = { orderName :: String, order :: SS.OrderForm }

type State
  = { open :: Boolean, orderName :: String, order :: SS.OrderForm }

initialState :: Input -> State
initialState input =
  { orderName: input.orderName
  , order: input.order
  , open: false
  }

component ::
  forall query output f m.
  MonadAff m =>
  MonadAlert m =>
  CredentialStore f m =>
  H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction }
    }

render ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action () m
render state
  | state.open = renderModal state
  | otherwise = renderIcon


renderModal ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action () m
renderModal state =
  HH.div_
    [ Modal.render
        $ Modal.defaultInput
            { title = HH.text "Duplicate Order"
            , closeAction = Just CloseModal
            , content = renderContent state state.order
            }
    ]


renderContent ::
  forall m.
  State -> 
  SS.OrderForm -> H.ComponentHTML Action () m
renderContent state (SS.OrderForm o) = 
  HH.div [ Css.classes [ "flex", "flex-col", "gap-y-4", "text-left" ] ]
    [ HH.div
        [ Css.classes
            [ "w-full"
            , "min-w-96"
            , "p-8"
            , "flex"
            , "flex-col"
            , "gap-4"
            , "max-h-128"
            , "overflow-auto"
            ]
        ]
        $ [
            HH.textarea
                [ HHP.value $ fromMaybe "" o.displayName <> " COPY"
                , Css.classes [ "nectary-textarea", "w-96" ]
                , HE.onValueChange (SetValue)
                ]
        ]
    , HH.div [ Css.classes [ "flex", "space-x-5" ] ] buttons
    ]

renderIcon ::
  forall m.
  H.ComponentHTML Action () m
renderIcon = HH.div [Css.classes [ "custom-padding" ], HE.onClick OpenModal]
            [
                Icon.control_point_duplicate
                    [ Icon.classes [ Css.c "h-6" , Css.c "mr-2", Css.c "list-icon"]
                    , Icon.ariaLabel "Duplicate Order"
                    , HHP.style "float: left"
                    ]
                , HH.text "Duplicate"
            ]

buttons ∷ forall w. Array (HH.HTML w Action)
buttons =
  [ HH.div [ Css.class_ "grow" ] []
  , HH.button
      [ Css.class_ "nectary-btn-secondary"
      , HE.onClick CloseModal
      ]
      [ HH.text "Close" ]
  , HH.button
        [ Css.class_ "nectary-btn-primary"
        , HE.onClick AcceptAndClose
        ]
        [ HH.text "OK" ]
  ]


createOrder ::
  forall output f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  String -> SS.OrderForm -> H.HalogenM State Action () output m Unit
createOrder name (SS.OrderForm order) = do
  order <- H.lift $ copyOrder (getId order.id) name
  case order of
    Loaded o' -> do
      H.lift
        $ Alerts.push
        $ Alert.defaultAlert
            { type_ = Alert.Success
            , content = HH.text "Order copied succesfully"
            }
      H.liftEffect $ reload
    Error errMsg ->
      H.lift
        $ Alerts.push
        $ Alert.errorAlert "Failed to copy order" errMsg
    _ -> pure unit

getId :: Maybe SS.OrderId -> String
getId orderId = case orderId of
  Just id -> show id
  Nothing -> ""

getIdByOrder :: SS.OrderForm  -> String
getIdByOrder (SS.OrderForm order) = (getId order.id)
    

handleAction ::
  forall output f m.
  MonadAff m =>
  MonadAlert m =>
  CredentialStore f m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  OpenModal event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    { orderName, order } <- H.modify $ \st -> st { open = true }
    H.modify_ _ { orderName = orderName <> " COPY" }
    H.modify_ _ { order = order }
  CloseModal event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    H.modify_ $ \st -> st { open = false }
    { order } <- H.get
    H.liftEffect $ uncheck ("check-" <> (getIdByOrder order))
  AcceptAndClose event -> do
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    { orderName, order } <- H.get
    let
      name = orderName
    void $ createOrder name order
    pure unit
    H.modify_ $ \st -> st { open = false }
  SetValue text -> do
    H.modify_ $ \st -> st { orderName = text }