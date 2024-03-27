-- | The modal that asks the user to confirm fulfillment.
module Sofa.App.OrderForm.ConfirmFulfillModal
  ( Slot
  , Input(..)
  , Output(..)
  , component
  ) where

import Prelude

import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm.SelectBuyer as SelectBuyer
import Sofa.App.OrderForm.SelectParticipant as SelectParticipant
import Sofa.Component.Icon as Icon
import Sofa.Component.InputField as InputField
import Sofa.Component.Modal as Modal
import Sofa.Component.Select as Select
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec (MarioPriority(..), Buyer(..), Participant(..))
import Type.Proxy (Proxy(..))

type Slot id
  = forall query.
    H.Slot query Output id

type Slots
  = ( marioPrioritySelect :: Select.Slot Unit MarioPriority
    , selectBuyer :: SelectBuyer.Slot Unit
    , selectParticipant :: SelectParticipant.Slot Unit
    , relatedIssue :: String
   )

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
    , requestParticipants :: Maybe String
    , relatedIssue :: Maybe String
    }

type State
  = { marioPriority :: Maybe MarioPriority
    , note :: String
    , participants :: Array Participant
    , relatedIssue :: Maybe String
    }

data Action
  = SetNote String
  | SetMarioPriority MarioPriority
  | SetRelatedIssue String
  | Cancel
  | Confirm
  | ChooseParticipant (Loadable Participant)
  | RemoveParticipant Int

showPrettyMarioPriority :: MarioPriority -> String
showPrettyMarioPriority = case _ of
  MarioPrioCritical -> "Critical"
  MarioPrioHigh -> "High"
  MarioPrioMedium -> "Medium"
  MarioPrioLow -> "Low"

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
initialState input =
  { marioPriority: if input.isMarioOrder then Just MarioPrioLow else Nothing
  , note: ""
  , participants: []
  , relatedIssue: Just ""
  }

render :: forall f m. MonadAff m => CredentialStore f m => State -> H.ComponentHTML Action Slots m
render state =
  Modal.render
    $ Modal.defaultInput
        { title = HH.text "Are you sure you want to fulfill this order?"
        , closeAction = Just (\_ -> Cancel)
        , backgroundClickAction = Nothing
        , content = renderContent state
        }

renderContent ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action Slots m
renderContent state =
  HH.div [ Css.classes [ "flex", "flex-col", "gap-3", "max-w-128" ] ]
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
    , case state.marioPriority of
        Nothing -> HH.text ""
        Just marioPrio -> renderJiraUserSelect state marioPrio
    , case state.marioPriority of
        Nothing -> HH.text ""
        Just _ -> InputField.render
              $ InputField.defaultInput
                  { label = "Related Issue"
                  , tooltipText = Just "It will be linked always to INT issue even if it is not the main ticket."
                  , props =
                    [ HP.placeholder "Link Issue Key..."
                    , HE.onValueChange SetRelatedIssue
                    , Css.classes [ "nectary-input", "w-full" ]
                    ]
                  , wrapperClasses = [ Css.c "w-90pur" ]
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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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

renderJiraUserSelect ::
  forall f m.
  CredentialStore f m =>
  MonadAff m => State -> MarioPriority -> H.ComponentHTML Action Slots m
renderJiraUserSelect state marioPriority =
  HH.label_
    [ HH.div
        [ Css.classes [ "grow", "font-semibold" ] ]
        [ HH.text "Request Participants" ]
    , HH.slot SelectParticipant.proxy unit SelectParticipant.component absurd ChooseParticipant
    , renderParticipants state
    ]

renderParticipants :: forall slots m. State -> H.ComponentHTML Action slots m
renderParticipants st =
  HH.div
    [ Css.classes
        [ "flex"
        , "flex-wrap"
        , "justify-start"
        , "gap-x-4"
        , "gap-y-2"
        ]
    ]
    (A.mapWithIndex (renderShowParticipants st) st.participants)


renderShowParticipants ∷ forall w. State → Int → Participant → HH.HTML w Action
renderShowParticipants state idx (Participant o) =
  HH.div [ Css.classes [ "nectary-tag", "pl-3", "pr-0" ] ]
    [ HH.div_ [ HH.text (parseEmail o.email) ]
    , let
        wrapperClasses = [ "flex", "w-7", "h-full", "pr-1" ]
      in
      HH.button
          [ Css.classes wrapperClasses
          , HE.onClick $ \_ -> RemoveParticipant idx
          ]
          [ Icon.cancel
              [ Icon.classes
                  [ Css.c "w-3.5"
                  , Css.c "h-3.5"
                  , Css.c "m-auto"
                  , Css.c "fill-stormy-400"
                  ]
              , Icon.ariaLabel "Remove"
              ]
          ]
    ]

parseEmail :: String -> String
parseEmail s =
  let
    entries = S.split (S.Pattern "-") s
    value = A.last entries
  in
    case value of
      Just email -> email
      Nothing -> ""

parseParticipantUsers :: (Array Participant) -> Maybe String
parseParticipantUsers lp =
  let
    users = map (\(Participant { user } )-> user)  lp
    user = A.intercalate "," users
  in
    if S.null user then Nothing else Just user


handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  SetNote note -> H.modify_ _ { note = note }
  SetRelatedIssue relatedIssue -> H.modify_ _ { relatedIssue = Just relatedIssue }
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
          , requestParticipants: (parseParticipantUsers state.participants)
          , relatedIssue: state.relatedIssue
          }
  ChooseParticipant participant ->
    case participant of
      Loaded (Participant { email, user }) -> do
        H.modify_ \st ->
          st
            { participants = (A.concat [[(Participant { email, user })], st.participants])
            }
      _ -> H.liftEffect $ Console.log "Error"
  RemoveParticipant idx -> do
    H.modify_ \st ->
        st
          { participants = (fromMaybe st.participants (A.deleteAt idx st.participants))
          }
      
    
