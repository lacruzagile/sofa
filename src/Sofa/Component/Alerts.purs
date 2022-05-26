-- | A component providing asynchronous management of Nectary style alerts.
module Sofa.Component.Alerts
  ( AlertSink
  , Slot
  , class MonadAlert
  , component
  , getAlertSink
  , mkAlertSink
  , proxy
  , push
  ) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe')
import Data.Time.Duration (Milliseconds(..))
import Data.UUID (UUID, genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Sofa.Component.Alert (Alert, AlertType(..))
import Sofa.Component.Alert as Alert
import Sofa.Css as Css
import Type.Proxy (Proxy(..))

-- | A type class that enriches a monad with the ability to create alert
-- | notifications.
class
  MonadAff m <= MonadAlert m where
  -- | Retrieves the alert sink into which alerts can be pushed. This function
  -- | is typically not called by client code, you would instead use the `push`
  -- | function.
  getAlertSink :: m AlertSink

-- | A plain alert, whose content does not reference any component slots or
-- | actions.
type PlainAlert
  = Alert Void Void

-- | An alert sink that can be used to publish alerts.
newtype AlertSink
  = AlertSink (AVar PlainAlert)

-- | Creates a new alert sink.
mkAlertSink :: Aff AlertSink
mkAlertSink = AlertSink <$> AVar.empty

-- | Publishes a new alert.
push :: forall m. MonadAlert m => PlainAlert -> m Unit
push alert = do
  AlertSink var <- getAlertSink
  liftAff (AVar.put alert var)

type Slot id
  = forall query output. H.Slot query output id

proxy :: Proxy "nectaryAlerts"
proxy = Proxy

type State
  = { alerts :: Array AlertState
    }

type AlertState
  = { id :: UUID
    , alert :: PlainAlert
    , removing :: Boolean
    }

data Action
  = Initialize
  | Push PlainAlert
  | StartRemove UUID
  | FinishRemove UUID

-- | Indicates how long an alert of the given type should be shown until
-- | automatically removed. If `Nothing`, then the alert will be kept until the
-- | user removes it.
typeTtl :: AlertType -> Maybe Milliseconds
typeTtl = case _ of
  Informative -> Just $ Milliseconds 10_000.0
  Success -> Just $ Milliseconds 5_000.0
  Warning -> Just $ Milliseconds 10_000.0
  Error -> Nothing

fadeDuration :: Milliseconds
fadeDuration = Milliseconds 300.0

component ::
  forall query input output m.
  MonadAff m =>
  MonadAlert m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const { alerts: [] }
    , render: render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render :: forall slots m. Monad m => State -> H.ComponentHTML Action slots m
render state =
  HH.div
    [ HP.classes
        [ Css.c "fixed"
        , Css.c "bottom-0"
        , Css.c "left-64" -- Avoid the side menu.
        , Css.c "right-0"
        , Css.c "m-5"
        , Css.c "flex"
        , Css.c "flex-col"
        , Css.c "items-end"
        , Css.c "gap-y-3"
        , Css.c "pointer-events-none"
        ]
    ]
    $ map renderAlert state.alerts
  where
  renderAlert :: AlertState -> H.ComponentHTML Action slots m
  renderAlert { id, alert, removing } =
    Alert.render
      $ alert
          { content =
            HH.div_
              [ Alert.closeBtn (\_ -> StartRemove id)
              , HH.fromPlainHTML alert.content
              ]
          , classes =
            alert.classes
              <> [ Css.c "pointer-events-auto" ]
              <> if removing then
                  [ Css.c "transition"
                  , Css.c "motion-reduce:transition-none"
                  , Css.c "duration-300" -- Must match value of 'fadeDuration'.
                  , Css.c "opacity-0"
                  , Css.c "scale-y-0"
                  ]
                else
                  [ Css.c "opacity-100" ]
          }

handleAction ::
  forall slots output m.
  MonadAff m =>
  MonadAlert m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  Initialize -> do
    alertSink <- H.lift getAlertSink
    startListenLoop alertSink
  Push alert -> do
    id <- H.liftEffect genUUID
    let
      ttl = typeTtl alert.type_

      alertState = { id, alert, removing: false }
    H.modify_ \st -> st { alerts = st.alerts <> [ alertState ] }
    -- If the alert should be removed then we start an asynchronous process to
    -- perform the remove after the given time.
    maybe' pure (oneShotTimer (StartRemove id)) ttl
  StartRemove id -> do
    H.modify_ \st ->
      st
        { alerts =
          let
            markRemoving a
              | a.id == id = a { removing = true }
              | otherwise = a
          in
            map markRemoving st.alerts
        }
    oneShotTimer (FinishRemove id) fadeDuration
  FinishRemove id -> do
    H.modify_ \st -> st { alerts = A.filter (\a -> a.id /= id) st.alerts }

-- Forks off a process that listens for alerts being dropped into the alert
-- sink.
startListenLoop ::
  forall slots output m.
  MonadAff m =>
  AlertSink -> H.HalogenM State Action slots output m Unit
startListenLoop (AlertSink sink) = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff $ forever
      $ do
          alert <- AVar.take sink
          H.liftEffect $ HS.notify listener (Push alert)
  -- Can ignore the result since we want to maintain the subscription for the
  -- entire component life cycle.
  _ <- H.subscribe emitter
  pure unit

-- | Triggers the given action after the given number of milliseconds.
oneShotTimer ::
  forall slots output m.
  MonadAff m =>
  Action -> Milliseconds -> H.HalogenM State Action slots output m Unit
oneShotTimer action delay = do
  { emitter, listener } <- H.liftEffect HS.create
  sid <- H.subscribe emitter
  _ <-
    H.fork do
      H.liftAff $ Aff.delay delay
      H.liftEffect $ HS.notify listener action
      H.unsubscribe sid
  pure unit
