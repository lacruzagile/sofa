-- | The order observers component of the order form.
module App.OrderForm.Observers (Slot, Input(..), Output(..), proxy, component) where

import Prelude
import Css as Css
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.SmartSpec as SS
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Widgets as Widgets

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "observers"
proxy = Proxy

type Input
  = Array SS.OrderObserver

type Output
  = Array SS.OrderObserver

type State
  = { observers :: Array SS.OrderObserver
    , editObserver :: Maybe EditObserver -- ^ Observer currently being edited.
    , newObserver :: Maybe String -- ^ Observer currently being built.
    , open :: Boolean -- ^ Whether the observers modal is open.
    }

-- | A observer with the given index being edited.
type EditObserver
  = { index :: Int, observer :: String }

data Action
  = OpenDetails
  | CloseDetails
  | SetNewEmail String -- ^ Set observer email of new observer.
  | StartNewObserver -- ^ Start adding a new observer.
  | CancelNewObserver -- ^ Cancel the new observer.
  | StopNewObserver -- ^ Stop and save the new observer.
  | RemoveObserver Int -- ^ Remove the observer with the given index.
  | SetEditEmail String -- ^ Set email of current node edit.
  | StartEditObserver Int -- ^ Starts editing the observer with the given index.
  | CancelEditObserver -- ^ Cancel current observer edit.
  | StopEditObserver -- ^ Stop and save current observer edit.

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
  { observers: input
  , editObserver: Nothing
  , newObserver: Nothing
  , open: false
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
    [ Css.tw.block
    , Css.tw.textLeft
    , Css.tw.textLg
    , Css.tw.underline
    , Css.tw.underlineOffset4
    , Css.tw.decorationSky300
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
          [ Css.tw.wFull
          , Css.tw.minW96
          , Css.tw.maxW128
          , Css.tw.flex
          , Css.tw.flexCol
          , Css.tw.spaceY4
          ]
      ]
      [ HH.div
          [ HP.classes
              [ Css.tw.flex
              , Css.tw.flexCol
              , Css.tw.spaceY5
              , Css.tw.maxH128
              , Css.tw.overflowAuto
              ]
          ]
          ( if A.null st.observers then
              [ HH.text "No observers" ]
            else
              A.mapWithIndex renderObserver st.observers
          )
      , HH.hr_
      , renderFooter
      ]

  renderFooter = case st.newObserver of
    Nothing ->
      HH.div [ HP.class_ Css.tw.flex ]
        [ HH.button
            [ HP.classes [ Css.btnSky100 ]
            , HE.onClick \_ -> StartNewObserver
            ]
            [ HH.text "+ Add Observer" ]
        , HH.div [ HP.class_ Css.tw.grow ] []
        , HH.button
            [ HP.classes [ Css.btnSky100 ]
            , HE.onClick \_ -> CloseDetails
            ]
            [ HH.text "Close" ]
        ]
    Just email ->
      HH.form [ HE.onSubmit $ \_ -> StopNewObserver ]
        [ HH.input
            [ HP.type_ HP.InputEmail
            , HP.classes [ Css.tw.p1, Css.tw.border, Css.tw.wFull ]
            , HP.placeholder "Observer email."
            , HP.value email
            , HE.onValueChange SetNewEmail
            ]
        , HH.button
            [ HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.ml2, Css.tw.floatRight ]
            , HE.onClick \_ -> CancelNewObserver
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.type_ HP.ButtonSubmit
            , HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.floatRight ]
            ]
            [ HH.text "Save" ]
        ]

  renderObserver idx n = case st.editObserver of
    Just { index, observer }
      | idx == index -> renderEditObserver observer
    _ -> renderShowObserver idx n

  renderShowObserver idx (SS.OrderObserver n) =
    HH.div_
      [ HH.text n.observerEmail
      , HH.div
          [ HP.classes [ Css.tw.textSm, Css.tw.textGray600, Css.tw.flex ] ]
          [ HH.div_ [ HH.text $ maybe "New" SS.prettyDateTime n.createTime ]
          , HH.div [ HP.class_ Css.tw.grow ] []
          , HH.button
              [ HP.classes [ Css.btnSky100, Css.tw.py0 ]
              , HE.onClick $ \_ -> StartEditObserver idx
              ]
              [ HH.text "Edit" ]
          , HH.button
              [ HP.classes [ Css.btnRed100, Css.tw.py0, Css.tw.ml2 ]
              , HE.onClick $ \_ -> RemoveObserver idx
              ]
              [ HH.text "Remove" ]
          ]
      ]

  renderEditObserver observer =
    HH.div_
      [ HH.input
          [ HP.type_ HP.InputEmail
          , HP.classes [ Css.tw.p1, Css.tw.border, Css.tw.wFull ]
          , HP.placeholder "Observer email."
          , HP.value observer
          , HE.onValueChange SetEditEmail
          ]
      , HH.button
          [ HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.ml2, Css.tw.floatRight ]
          , HE.onClick \_ -> CancelEditObserver
          ]
          [ HH.text "Cancel" ]
      , HH.button
          [ HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.floatRight ]
          , HE.onClick \_ -> StopEditObserver
          ]
          [ HH.text "Save" ]
      ]

handleAction ::
  forall slots m.
  MonadAff m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  OpenDetails -> H.modify_ $ \st -> st { open = true }
  CloseDetails ->
    H.modify_ \st ->
      st { newObserver = Nothing, open = false }
  SetNewEmail email -> H.modify_ \st -> st { newObserver = Just email }
  StartNewObserver -> H.modify_ \st -> st { newObserver = Just "" }
  CancelNewObserver -> H.modify_ \st -> st { newObserver = Nothing }
  StopNewObserver -> do
    st' <-
      H.modify \st ->
        st
          { observers =
            fromMaybe st.observers
              $ (\email -> st.observers <> [ mkObserver email ])
              <$> st.newObserver
          , newObserver = Nothing
          }
    H.raise st'.observers
  RemoveObserver idx -> do
    st' <-
      H.modify \st ->
        st
          { observers = fromMaybe st.observers $ A.deleteAt idx st.observers
          }
    H.raise st'.observers
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
  StopEditObserver -> do
    st' <-
      H.modify \st ->
        st
          { editObserver = Nothing
          , observers =
            fromMaybe st.observers
              $ case st.editObserver of
                  Just { index, observer } -> A.modifyAt index (\n -> setObserverEmail n observer) st.observers
                  _ -> Nothing
          }
    H.raise st'.observers
