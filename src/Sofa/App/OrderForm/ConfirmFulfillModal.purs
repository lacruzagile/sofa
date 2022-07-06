-- | The modal that asks the user to confirm fulfillment.
module Sofa.App.OrderForm.ConfirmFulfillModal
  ( Slot
  , Input(..)
  , Output(..)
  , MarioPriority(..)
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Component.InputField as InputField
import Sofa.Component.Modal as Modal
import Sofa.Component.Select as Select
import Sofa.Css as Css
import Type.Proxy (Proxy(..))

type Slot id
  = forall query.
    H.Slot query Output id

type Slots
  = ( marioPrioritySelect :: Select.Slot Unit MarioPriority )

type Input
  = { isMarioOrder :: Boolean }

data Output
  = FulfillCancel
  | FulfillConfirm
    { marioPriority :: Maybe MarioPriority
    -- ^ If the order contains a Mario section then this indicates the Jira
    -- ticket priority.
    , note :: Maybe String
    -- ^ Note that should be added to the order before fulfillment.
    }

data MarioPriority
  = MarioPrioCritical
  | MarioPrioHigh
  | MarioPrioMedium
  | MarioPrioLow

derive instance eqMarioPriority :: Eq MarioPriority

type State
  = { marioPriority :: Maybe MarioPriority
    , note :: String
    }

data Action
  = SetNote String
  | SetMarioPriority MarioPriority
  | Cancel
  | Confirm

showPrettyMarioPriority :: MarioPriority -> String
showPrettyMarioPriority = case _ of
  MarioPrioCritical -> "Critical"
  MarioPrioHigh -> "High"
  MarioPrioMedium -> "Medium"
  MarioPrioLow -> "Low"

component ::
  forall query m.
  MonadAff m =>
  H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState input =
  { marioPriority: if input.isMarioOrder then Just MarioPrioLow else Nothing
  , note: ""
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  Modal.render
    $ Modal.defaultInput
        { title = HH.text "Are you sure you want to fulfill this order?"
        , closeAction = Just (\_ -> Cancel)
        , backgroundClickAction = Nothing
        , content = renderContent state
        }

renderContent ::
  forall m.
  MonadAff m =>
  State -> H.ComponentHTML Action Slots m
renderContent state =
  HH.div [ Css.classes [ "flex", "flex-col", "gap-6", "max-w-128" ] ]
    [ HH.p_ [ HH.text "Once the order is fulfilled it is not possible to edit the order anymore." ]
    , case state.marioPriority of
        Nothing -> HH.text ""
        Just marioPrio -> renderMarioPrioritySelect marioPrio
    , InputField.renderTextarea
        $ InputField.defaultTextarea
            { label = "Notes"
            , props =
              [ HP.placeholder "Write something…"
              , HE.onValueChange SetNote
              ]
            }
    , HH.div [ Css.classes [ "flex", "gap-5" ] ]
        [ HH.div [ Css.class_ "grow" ] []
        , HH.button
            [ Css.class_ "nectary-btn-secondary"
            , HE.onClick \_ -> Cancel
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ Css.class_ "nectary-btn-primary"
            , HE.onClick \_ -> Confirm
            ]
            [ HH.text "Fulfill Order" ]
        ]
    ]

renderMarioPrioritySelect ::
  forall m.
  MonadAff m =>
  MarioPriority -> H.ComponentHTML Action Slots m
renderMarioPrioritySelect marioPriority =
  HH.label_
    [ HH.div
        [ Css.classes [ "grow", "font-semibold" ] ]
        [ HH.text "Priority" ]
    , HH.slot
        (Proxy :: Proxy "marioPrioritySelect")
        unit
        Select.component
        ( Select.defaultInput
            { selected = Just marioPriority
            , values =
              let
                mkValue p = Tuple (HH.text $ showPrettyMarioPriority p) p
              in
                [ mkValue MarioPrioLow
                , mkValue MarioPrioMedium
                , mkValue MarioPrioHigh
                , mkValue MarioPrioCritical
                ]
            }
        )
        SetMarioPriority
    ]

handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  SetNote note -> H.modify_ _ { note = note }
  SetMarioPriority marioPriority ->
    H.modify_
      _ { marioPriority = Just marioPriority }
  Cancel -> H.raise FulfillCancel
  Confirm -> do
    state <- H.get
    H.raise
      $ FulfillConfirm
          { marioPriority: state.marioPriority
          , note: if S.null state.note then Nothing else Just state.note
          }
