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

class
  MonadAff m <= MonadAlert m where
  getAlertSink :: m AlertSink

type PlainAlert
  = Alert Void Void

newtype AlertSink
  = AlertSink (AVar PlainAlert)

mkAlertSink :: Aff AlertSink
mkAlertSink = AlertSink <$> AVar.empty

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
    }

data Action
  = Initialize
  | Push PlainAlert
  | Remove (Maybe H.SubscriptionId) UUID

typeTtl :: AlertType -> Maybe Milliseconds
typeTtl = case _ of
  Informative -> Just $ Milliseconds 8_000.0
  Success -> Just $ Milliseconds 8_000.0
  Warning -> Just $ Milliseconds 10_000.0
  Error -> Nothing

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
        , Css.c "inset-x-0"
        , Css.c "m-5"
        , Css.c "flex"
        , Css.c "flex-col"
        , Css.c "items-end"
        , Css.c "gap-y-3"
        ]
    ]
    $ map renderAlert state.alerts
  where
  renderAlert :: AlertState -> H.ComponentHTML Action slots m
  renderAlert { id, alert } =
    Alert.render
      $ alert
          { content =
            HH.div_
              [ Alert.closeBtn (\_ -> Remove Nothing id)
              , HH.fromPlainHTML alert.content
              ]
          }

handleAction ::
  forall slots output m.
  MonadAff m =>
  MonadAlert m =>
  Action -> H.HalogenM State Action slots output m Unit
handleAction = case _ of
  Initialize -> do
    alertSink <- H.lift getAlertSink
    _ <- H.subscribe =<< listenLoop alertSink
    pure unit
  Push alert -> do
    id <- H.liftEffect genUUID
    let
      ttl = typeTtl alert.type_
    H.modify_ \st -> st { alerts = st.alerts <> [ { id, alert } ] }
    maybe' pure (timer id) ttl
  Remove sid id -> do
    H.modify_ \st -> st { alerts = A.filter (\a -> a.id /= id) st.alerts }
    maybe' pure H.unsubscribe sid

listenLoop ::
  forall slots output m.
  MonadAff m =>
  AlertSink -> H.HalogenM State Action slots output m (HS.Emitter Action)
listenLoop (AlertSink sink) = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <-
    H.liftAff $ Aff.forkAff $ forever
      $ do
          alert <- AVar.take sink
          H.liftEffect $ HS.notify listener (Push alert)
  pure emitter

timer ::
  forall slots output m.
  MonadAff m =>
  UUID -> Milliseconds -> H.HalogenM State Action slots output m Unit
timer alertId ttl = do
  { emitter, listener } <- H.liftEffect HS.create
  sid <- H.subscribe emitter
  _ <-
    H.liftAff $ Aff.forkAff
      $ do
          Aff.delay ttl
          H.liftEffect $ HS.notify listener (Remove (Just sid) alertId)
  pure unit
