-- | The order observers component of the order form.
module App.OrderForm.Observers (Slot, Input(..), Output(..), proxy, component) where

import Prelude
import App.Requests (deleteOrderObserver, patchOrderObserver, postOrderObserver)
import Css as Css
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable(..))
import Data.Loadable as Loadable
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.SmartSpec as SS
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Widgets as Widgets

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "observers"
proxy = Proxy

type Input
  = { orderId :: Maybe SS.OrderId
    , observers :: Array SS.OrderObserver
    }

type Output
  = Array SS.OrderObserver

type State
  = { orderId :: Maybe SS.OrderId
    , observers :: Array SS.OrderObserver
    , editObserver :: Maybe EditObserver -- ^ Observer currently being edited.
    , newObserver :: Maybe String -- ^ Observer currently being built.
    , open :: Boolean -- ^ Whether the observers modal is open.
    , observerAction :: ObserverAction
    }

-- | An observer action currently being performed.
data ObserverAction
  = ObserverIdle (Maybe ObserverAction) -- ^ No action currently in progress, previous action may be provided.
  | ObserverCreating (Loadable Unit)
  | ObserverDeleting Int (Loadable Unit)
  | ObserverUpdating Int (Loadable Unit)

-- | A observer with the given index being edited.
type EditObserver
  = { index :: Int, observer :: String }

data Action
  = OpenDetails
  | CloseDetails
  | SetNewEmail String -- ^ Set observer email of new observer.
  | StartNewObserver -- ^ Start adding a new observer.
  | CancelNewObserver -- ^ Cancel the new observer.
  | StopNewObserver Event -- ^ Stop and save the new observer.
  | RemoveObserver Int -- ^ Remove the observer with the given index.
  | SetEditEmail String -- ^ Set email of current node edit.
  | StartEditObserver Int -- ^ Starts editing the observer with the given index.
  | CancelEditObserver -- ^ Cancel current observer edit.
  | StopEditObserver Int Event.Event -- ^ Stop and save current observer edit with the given index.

component ::
  forall query m.
  MonadAff m =>
  CredentialStore m =>
  H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState input =
  { orderId: input.orderId
  , observers: input.observers
  , editObserver: Nothing
  , newObserver: Nothing
  , open: false
  , observerAction: ObserverIdle Nothing
  }

mkObserver :: String -> SS.OrderObserver
mkObserver observerEmail =
  SS.OrderObserver
    { observerId: Nothing, createTime: Nothing, observerEmail
    }

getObserverEmail :: SS.OrderObserver -> String
getObserverEmail (SS.OrderObserver { observerEmail }) = observerEmail

setObserverEmail :: SS.OrderObserver -> String -> SS.OrderObserver
setObserverEmail (SS.OrderObserver observer) email =
  SS.OrderObserver
    $ observer { observerEmail = email }

render ::
  forall slots m.
  MonadAff m =>
  State -> H.ComponentHTML Action slots m
render state
  | state.open = renderDetails state
  | otherwise = renderSummary state

renderSummary :: forall slots m. State -> H.ComponentHTML Action slots m
renderSummary st = btn
  where
  btn = HH.button [ HP.classes btnClasses, HE.onClick $ \_ -> OpenDetails ] label

  label = case A.length st.observers of
    0 -> [ HH.text "No observers" ]
    1 -> [ HH.text "1 observer" ]
    n -> [ HH.text (show n), HH.text " observers" ]

  btnClasses =
    [ Css.c "block"
    , Css.c "text-left"
    , Css.c "underline"
    , Css.c "underline-offset-4"
    , Css.c "decoration-honey-500"
    ]

renderDetails ::
  forall slots m.
  MonadAff m =>
  State -> H.ComponentHTML Action slots m
renderDetails st =
  HH.div_
    [ renderSummary st
    , Widgets.modal [ closeBtn ] $ renderBody
    ]
  where
  closeBtn = Widgets.modalCloseBtn (\_ -> CloseDetails)

  renderBody =
    HH.div
      [ HP.classes
          [ Css.c "w-full"
          , Css.c "min-w-96"
          , Css.c "max-w-128"
          , Css.c "flex"
          , Css.c "flex-col"
          , Css.c "space-y-4"
          ]
      ]
      [ if A.null st.observers then
          HH.div
            [ HP.classes [ Css.c "text-lg", Css.c "text-gray-600" ] ]
            [ HH.text "No observers available" ]
        else
          HH.div
            [ HP.classes
                [ Css.c "flex"
                , Css.c "flex-col"
                , Css.c "space-y-4"
                , Css.c "max-h-128"
                , Css.c "overflow-auto"
                ]
            ]
            (A.mapWithIndex renderObserver st.observers)
      , renderFooter
      ]

  actionsAllowed = case st.observerAction of
    ObserverIdle _ -> true
    _ -> false

  renderFooter = case st.newObserver of
    _
      | isJust st.editObserver -> HH.text "" -- Skip footer when editing.
    Nothing ->
      HH.div [ HP.classes [ Css.c "flex", Css.c "space-x-4" ] ]
        [ HH.div [ HP.class_ (Css.c "grow") ] []
        , HH.button
            [ HP.classes [ Css.c "sofa-btn-secondary" ]
            , HP.enabled actionsAllowed
            , HE.onClick \_ -> CloseDetails
            ]
            [ HH.text "Close" ]
        , HH.button
            [ HP.classes [ Css.c "sofa-btn-primary" ]
            , HP.enabled actionsAllowed
            , HE.onClick \_ -> StartNewObserver
            ]
            [ HH.text "Add" ]
        ]
    Just email ->
      HH.form [ HE.onSubmit StopNewObserver ]
        [ HH.input
            [ HP.type_ HP.InputEmail
            , HP.classes [ Css.c "nectary-input", Css.c "w-full" ]
            , HP.placeholder "Observer email address"
            , HP.value email
            , HE.onValueChange SetNewEmail
            ]
        , HH.div [ HP.class_ (Css.c "text-raspberry-500") ]
            $ maybe [] (\msg -> [ HH.text msg ])
            $ createError
        , HH.div
            [ HP.classes
                [ Css.c "flex"
                , Css.c "space-x-4"
                , Css.c "mt-3"
                , Css.c "mb-0.5" -- Avoid clipping of buttons.
                ]
            ]
            [ HH.div [ HP.class_ (Css.c "grow") ] []
            , HH.button
                [ HP.class_ (Css.c "sofa-btn-destructive")
                , HP.type_ HP.ButtonButton
                , HP.enabled actionsAllowed
                , HE.onClick \_ -> CancelNewObserver
                ]
                [ HH.text "Cancel" ]
            , HH.button
                [ HP.class_ (Css.c "sofa-btn-primary")
                , HP.type_ HP.ButtonSubmit
                , HP.enabled actionsAllowed
                ]
                [ HH.text "Save"
                , if isCreating then
                    Widgets.spinner [ Css.c "ml-2", Css.c "align-text-bottom" ]
                  else
                    HH.text ""
                ]
            ]
        ]
      where
      isCreating = case st.observerAction of
        ObserverCreating Loading -> true
        _ -> false

      createError = case st.observerAction of
        ObserverIdle (Just (ObserverCreating (Error msg))) -> Just msg
        _ -> Nothing

  renderObserver idx n = case st.editObserver of
    Just { index, observer }
      | idx == index -> renderEditObserver idx observer
    _ -> renderShowObserver idx n

  renderShowObserver idx (SS.OrderObserver o) =
    HH.div [ HP.classes [ Css.c "group", Css.c "py-3" ] ]
      [ HH.text o.observerEmail
      , HH.div
          [ HP.classes
              [ Css.c "text-sm"
              , Css.c "text-gray-600"
              , Css.c "flex"
              , Css.c "space-x-2"
              , Css.c "mr-0.5" -- Avoid clipping of buttons.
              ]
          ]
          [ HH.div_
              [ maybe
                  (HH.text "New")
                  Widgets.dateWithTimeTooltipRight
                  o.createTime
              ]
          , HH.div [ HP.class_ (Css.c "grow") ] []
          , HH.button
              [ HP.classes
                  $ [ Css.c "sofa-btn-destructive"
                    , Css.c "h-auto"
                    , Css.c "py-0"
                    ]
                  <> hideable isDeleting
              , HP.enabled actionsAllowed
              , HE.onClick $ \_ -> RemoveObserver idx
              ]
              [ HH.text "Remove"
              , spinner isDeleting
              ]
          , HH.button
              [ HP.classes
                  $ [ Css.c "sofa-btn-primary"
                    , Css.c "h-auto"
                    , Css.c "py-0"
                    ]
                  <> hideable false
              , HP.enabled actionsAllowed
              , HE.onClick $ \_ -> StartEditObserver idx
              ]
              [ HH.text "Edit" ]
          ]
      , maybe (HH.text "")
          (\msg -> HH.div [ HP.class_ (Css.c "text-raspberry-500") ] [ HH.text msg ])
          $ deleteError
      ]
    where
    hideable p =
      if not p then
        [ Css.c "invisible", Css.c "group-hover:visible" ]
      else
        []

    spinner p =
      if p then
        Widgets.spinner [ Css.c "w-4", Css.c "h-4", Css.c "ml-2", Css.c "align-middle" ]
      else
        HH.text ""

    isDeleting = case st.observerAction of
      ObserverDeleting didx Loading
        | idx == didx -> true
      _ -> false

    deleteError = case st.observerAction of
      ObserverIdle (Just (ObserverDeleting didx (Error msg)))
        | idx == didx -> Just msg
      _ -> Nothing

  renderEditObserver idx observer =
    HH.form [ HE.onSubmit (StopEditObserver idx) ]
      [ HH.input
          [ HP.type_ HP.InputEmail
          , HP.classes [ Css.c "nectary-input", Css.c "w-full" ]
          , HP.placeholder "Observer email address"
          , HP.value observer
          , HE.onValueChange SetEditEmail
          ]
      , HH.div [ HP.classes [ Css.c "text-raspberry-500", Css.c "w-full" ] ]
          $ maybe [] (\msg -> [ HH.text msg ])
          $ updateError
      , HH.div
          [ HP.classes
              [ Css.c "flex"
              , Css.c "space-x-4"
              , Css.c "mt-2"
              , Css.c "mr-0.5" -- Avoid clipping of buttons.
              , Css.c "mb-0.5" -- Avoid clipping of buttons.
              ]
          ]
          [ HH.div [ HP.class_ (Css.c "grow") ] []
          , HH.button
              [ HP.classes [ Css.c "sofa-btn-secondary" ]
              , HP.type_ HP.ButtonButton
              , HP.enabled actionsAllowed
              , HE.onClick \_ -> CancelEditObserver
              ]
              [ HH.text "Cancel" ]
          , HH.button
              [ HP.classes [ Css.c "sofa-btn-primary" ]
              , HP.type_ HP.ButtonSubmit
              , HP.enabled actionsAllowed
              ]
              [ HH.text "Save"
              , if isUpdating then
                  Widgets.spinner [ Css.c "ml-2", Css.c "align-text-bottom" ]
                else
                  HH.text ""
              ]
          ]
      ]
    where
    isUpdating = case st.observerAction of
      ObserverUpdating didx Loading
        | idx == didx -> true
      _ -> false

    updateError = case st.observerAction of
      ObserverIdle (Just (ObserverUpdating didx (Error msg)))
        | idx == didx -> Just msg
      _ -> Nothing

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  OpenDetails -> H.modify_ $ \st -> st { open = true }
  CloseDetails ->
    H.modify_ \st ->
      st { newObserver = Nothing, open = false }
  SetNewEmail email -> H.modify_ \st -> st { newObserver = Just email }
  StartNewObserver -> H.modify_ \st -> st { newObserver = Just "" }
  CancelNewObserver -> H.modify_ \st -> st { newObserver = Nothing }
  StopNewObserver event -> do
    H.liftEffect $ Event.preventDefault event
    state <- H.modify \st -> st { observerAction = ObserverCreating Loading }
    case Tuple state.orderId state.newObserver of
      Tuple (Just oid) (Just email) -> do
        observerResult <- H.lift $ postOrderObserver oid (mkObserver email)
        state' <-
          H.modify \st ->
            st
              { observers =
                fromMaybe st.observers
                  $ (\observer -> st.observers <> [ observer ])
                  <$> Loadable.toMaybe observerResult
              , newObserver = Nothing
              , observerAction = ObserverIdle (Just st.observerAction)
              }
        H.raise state'.observers
      _ -> pure unit
  RemoveObserver idx -> do
    state <- H.modify \st -> st { observerAction = ObserverDeleting idx Loading }
    let
      mObserver = A.index state.observers idx
    observerResult <- case Tuple state.orderId mObserver of
      Tuple
        (Just oid)
        (Just (SS.OrderObserver { observerId: Just nid })) -> H.lift $ deleteOrderObserver oid nid
      _ -> pure Idle
    state' <-
      H.modify \st ->
        st
          { observers =
            fromMaybe st.observers do
              _ <- Loadable.toMaybe observerResult
              A.deleteAt idx st.observers
          , observerAction = ObserverIdle (Just $ ObserverDeleting idx observerResult)
          }
    H.raise state'.observers
  SetEditEmail email ->
    H.modify_ \st ->
      st { editObserver = (\n -> n { observer = email }) <$> st.editObserver }
  StartEditObserver idx ->
    H.modify_ \st ->
      st
        { editObserver =
          let
            mkEditObserver n = { index: idx, observer: getObserverEmail n }
          in
            mkEditObserver <$> A.index st.observers idx
        }
  CancelEditObserver -> H.modify_ \st -> st { editObserver = Nothing }
  StopEditObserver idx event -> do
    H.liftEffect $ Event.preventDefault event
    state <- H.modify \st -> st { observerAction = ObserverUpdating idx Loading }
    let
      mObserver = A.index state.observers idx
    observerResult <- case { oid: state.orderId, observer: mObserver, eobserver: state.editObserver } of
      { oid: Just oid
      , observer: Just observer@(SS.OrderObserver { observerId: Just ooid })
      , eobserver: Just { observer: email }
      } -> H.lift $ patchOrderObserver oid ooid (setObserverEmail observer email)
      _ -> pure Idle
    st' <-
      H.modify \st ->
        st
          { editObserver = Nothing
          , observers =
            fromMaybe st.observers
              $ case st.editObserver of
                  Just { index, observer } -> A.modifyAt index (\n -> setObserverEmail n observer) st.observers
                  _ -> Nothing
          , observerAction =
            ObserverIdle
              $ Just
              $ ObserverUpdating idx (const unit <$> observerResult)
          }
    H.raise st'.observers
