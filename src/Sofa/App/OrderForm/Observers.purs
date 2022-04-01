-- | The order observers component of the order form.
module Sofa.App.OrderForm.Observers (Slot, Input(..), Output(..), proxy, component) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.App.Requests (deleteOrderObserver, postOrderObserver)
import Sofa.Component.Icon as Icon
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (focusElementByRef)
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

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
    , newObserver :: Maybe String -- ^ Observer currently being built.
    , observerAction :: ObserverAction
    }

-- | An observer action currently being performed.
data ObserverAction
  = ObserverIdle (Maybe ObserverAction) -- ^ No action currently in progress, previous action may be provided.
  | ObserverCreating (Loadable Unit)
  | ObserverDeleting Int (Loadable Unit)
  | ObserverUpdating Int (Loadable Unit)

data Action
  = SetNewEmail String -- ^ Set observer email of new observer.
  | StartNewObserver -- ^ Start adding a new observer.
  | CancelNewObserver -- ^ Cancel the new observer.
  | StopNewObserver Event -- ^ Stop and save the new observer.
  | RemoveObserver Int -- ^ Remove the observer with the given index.

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
  , newObserver: Nothing
  , observerAction: ObserverIdle Nothing
  }

mkObserver :: String -> SS.OrderObserver
mkObserver observerEmail =
  SS.OrderObserver
    { observerId: Nothing, createTime: Nothing, observerEmail
    }

render ::
  forall slots m.
  MonadAff m =>
  State -> H.ComponentHTML Action slots m
render state = case state.newObserver of
  Nothing -> renderSummary state
  Just o -> renderModal state o

renderSummary :: forall slots m. State -> H.ComponentHTML Action slots m
renderSummary st =
  HH.div
    [ HP.classes
        [ Css.c "flex"
        , Css.c "flex-wrap"
        , Css.c "justify-start"
        , Css.c "gap-x-4"
        , Css.c "gap-y-2"
        ]
    ]
    (A.mapWithIndex (renderShowObserver st) st.observers <> [ btn ])
  where
  btn =
    HH.button
      [ HP.classes [ Css.c "nectary-tag" ]
      , HE.onClick $ \_ -> StartNewObserver
      , HPAria.label "Add observer"
      ]
      [ Icon.add
          [ Icon.classes
              [ Css.c "w-4"
              , Css.c "h-4"
              , Css.c "fill-stormy-400"
              ]
          ]
      ]

renderModal ::
  forall slots m.
  MonadAff m =>
  State -> String -> H.ComponentHTML Action slots m
renderModal state observer =
  HH.div_
    [ renderSummary state
    , Modal.render [] $ renderBody
    ]
  where
  actionsAllowed = case state.observerAction of
    ObserverIdle _ -> true
    _ -> false

  renderBody =
    HH.div
      [ HP.classes
          [ Css.c "w-full"
          , Css.c "min-w-96"
          , Css.c "max-w-128"
          , Css.c "flex"
          , Css.c "flex-col"
          , Css.c "gap-y-4"
          ]
      ]
      [ HH.form [ HE.onSubmit StopNewObserver ]
          [ HH.input
              [ HP.ref refEmailInput
              , HP.type_ HP.InputEmail
              , HP.classes [ Css.c "nectary-input", Css.c "w-full" ]
              , HP.placeholder "Observer email address"
              , HP.value observer
              , HE.onValueChange SetNewEmail
              ]
          , HH.div [ HP.class_ (Css.c "text-raspberry-500") ]
              $ maybe [] (\msg -> [ HH.text msg ])
              $ createError
          , HH.div
              [ HP.classes
                  [ Css.c "flex"
                  , Css.c "gap-x-4"
                  , Css.c "mt-3"
                  , Css.c "mb-0.5" -- Avoid clipping of buttons.
                  ]
              ]
              [ HH.div [ HP.class_ (Css.c "grow") ] []
              , HH.button
                  [ HP.class_ (Css.c "sofa-btn-secondary")
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
      ]
    where
    isCreating = case state.observerAction of
      ObserverCreating Loading -> true
      _ -> false

    createError = case state.observerAction of
      ObserverIdle (Just (ObserverCreating (Error msg))) -> Just msg
      _ -> Nothing

renderShowObserver ∷ forall w. State → Int → SS.OrderObserver → HH.HTML w Action
renderShowObserver state idx (SS.OrderObserver o) =
  HH.div [ HP.classes [ Css.c "nectary-tag", Css.c "pl-3", Css.c "pr-0" ] ]
    [ HH.div_ [ HH.text o.observerEmail ]
    , let
        wrapperClasses = [ Css.c "flex", Css.c "w-7", Css.c "h-full", Css.c "pr-1" ]
      in
        if isDeleting then
          HH.div [ HP.classes wrapperClasses ]
            [ Widgets.spinner [ Css.c "w-3", Css.c "h-3", Css.c "m-auto" ]
            ]
        else
          HH.button
            [ HP.classes wrapperClasses
            , HP.enabled actionsAllowed
            , HE.onClick $ \_ -> RemoveObserver idx
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
  where
  isDeleting = case state.observerAction of
    ObserverDeleting didx Loading
      | idx == didx -> true
    _ -> false

  actionsAllowed = case state.observerAction of
    ObserverIdle _ -> true
    _ -> false

refEmailInput :: H.RefLabel
refEmailInput = H.RefLabel "email"

handleAction ::
  forall slots m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  SetNewEmail email -> H.modify_ _ { newObserver = Just email }
  StartNewObserver -> do
    H.modify_ _ { newObserver = Just "" }
    focusElementByRef refEmailInput
  CancelNewObserver -> H.modify_ _ { newObserver = Nothing }
  StopNewObserver event -> do
    H.liftEffect $ Event.preventDefault event
    state <- H.modify _ { observerAction = ObserverCreating Loading }
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
    state <- H.modify _ { observerAction = ObserverDeleting idx Loading }
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
