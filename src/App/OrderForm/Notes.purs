-- | The order notes component of the order form.
module App.OrderForm.Notes (Slot, Input(..), Output(..), proxy, component) where

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

proxy :: Proxy "notes"
proxy = Proxy

type Input
  = Array SS.OrderNote

type Output
  = Array SS.OrderNote

type State
  = { notes :: Array SS.OrderNote
    , editNote :: Maybe EditNote -- ^ Note currently being edited.
    , newNote :: Maybe String -- ^ Note currently being built.
    , open :: Boolean -- ^ Whether the notes modal is open.
    }

-- | A note with the given index being edited.
type EditNote
  = { index :: Int, note :: String }

data Action
  = OpenDetails
  | CloseDetails
  | SetNewText String -- ^ Set note text of new note.
  | StartNewNote -- ^ Start adding a new note.
  | CancelNewNote -- ^ Cancel the new note.
  | StopNewNote -- ^ Stop and save the new note.
  | RemoveNote Int -- ^ Remove the note with the given index.
  | SetEditText String -- ^ Set text of current node edit.
  | StartEditNote Int -- ^ Starts editing the note with the given index.
  | CancelEditNote -- ^ Cancel current note edit.
  | StopEditNote -- ^ Stop and save current note edit.

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
  { notes: input
  , editNote: Nothing
  , newNote: Nothing
  , open: false
  }

mkNote :: String -> SS.OrderNote
mkNote note = SS.OrderNote { orderNoteId: Nothing, createTime: Nothing, note }

getNoteText :: SS.OrderNote -> String
getNoteText (SS.OrderNote { note }) = note

setNoteText :: SS.OrderNote -> String -> SS.OrderNote
setNoteText (SS.OrderNote note) text = SS.OrderNote $ note { note = text }

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

  label = case A.length st.notes of
    0 -> [ HH.text "No order notes" ]
    1 -> [ HH.text "1 order note" ]
    n -> [ HH.text (show n), HH.text " order notes" ]

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
              , Css.tw.spaceY4
              , Css.tw.maxH128
              , Css.tw.overflowAuto
              ]
          ]
          ( if A.null st.notes then
              [ HH.text "No notes" ]
            else
              A.mapWithIndex renderNote st.notes
          )
      , HH.hr_
      , renderFooter
      ]

  renderFooter = case st.newNote of
    Nothing ->
      HH.div [ HP.class_ Css.tw.flex ]
        [ HH.button
            [ HP.classes [ Css.btnSky100 ]
            , HE.onClick \_ -> StartNewNote
            ]
            [ HH.text "+ Add Note" ]
        , HH.div [ HP.class_ Css.tw.grow ] []
        , HH.button
            [ HP.classes [ Css.btnSky100 ]
            , HE.onClick \_ -> CloseDetails
            ]
            [ HH.text "Close" ]
        ]
    Just text ->
      HH.form [ HE.onSubmit $ \_ -> StopNewNote ]
        [ HH.textarea
            [ HP.classes [ Css.tw.p1, Css.tw.border, Css.tw.wFull ]
            , HP.placeholder "Note text."
            , HP.rows 4
            , HP.value text
            , HE.onValueChange SetNewText
            ]
        , HH.button
            [ HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.ml2, Css.tw.floatRight ]
            , HE.onClick \_ -> CancelNewNote
            ]
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.type_ HP.ButtonSubmit
            , HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.floatRight ]
            ]
            [ HH.text "Save" ]
        ]

  renderNote idx n = case st.editNote of
    Just { index, note }
      | idx == index -> renderEditNote note
    _ -> renderShowNote idx n

  renderShowNote idx (SS.OrderNote n) =
    HH.div [ HP.classes [ Css.tw.group, Css.tw.py3 ] ]
      [ HH.text n.note
      , HH.div
          [ HP.classes [ Css.tw.textSm, Css.tw.textGray600, Css.tw.flex ] ]
          [ HH.div_ [ HH.text $ maybe "New" SS.prettyDateTime n.createTime ]
          , HH.div [ HP.class_ Css.tw.grow ] []
          , HH.button
              [ HP.classes
                  [ Css.btnSky100
                  , Css.tw.py0
                  , Css.tw.hidden
                  , Css.tw.groupHoverBlock
                  ]
              , HE.onClick $ \_ -> StartEditNote idx
              ]
              [ HH.text "Edit" ]
          , HH.button
              [ HP.classes
                  [ Css.btnRed100
                  , Css.tw.py0
                  , Css.tw.ml2
                  , Css.tw.hidden
                  , Css.tw.groupHoverBlock
                  ]
              , HE.onClick $ \_ -> RemoveNote idx
              ]
              [ HH.text "Remove" ]
          ]
      ]

  renderEditNote note =
    HH.div_
      [ HH.textarea
          [ HP.classes [ Css.tw.p1, Css.tw.border, Css.tw.wFull ]
          , HP.placeholder "Note text."
          , HP.rows 4
          , HP.value note
          , HE.onValueChange SetEditText
          ]
      , HH.button
          [ HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.ml2, Css.tw.floatRight ]
          , HE.onClick \_ -> CancelEditNote
          ]
          [ HH.text "Cancel" ]
      , HH.button
          [ HP.classes [ Css.btnSky100, Css.tw.mt1, Css.tw.floatRight ]
          , HE.onClick \_ -> StopEditNote
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
      st { newNote = Nothing, open = false }
  SetNewText text -> H.modify_ \st -> st { newNote = Just text }
  StartNewNote -> H.modify_ \st -> st { newNote = Just "" }
  CancelNewNote -> H.modify_ \st -> st { newNote = Nothing }
  StopNewNote -> do
    st' <-
      H.modify \st ->
        st
          { notes =
            fromMaybe st.notes
              $ (\text -> st.notes <> [ mkNote text ])
              <$> st.newNote
          , newNote = Nothing
          }
    H.raise st'.notes
  RemoveNote idx -> do
    st' <-
      H.modify \st ->
        st
          { notes = fromMaybe st.notes $ A.deleteAt idx st.notes
          }
    H.raise st'.notes
  SetEditText text ->
    H.modify_ \st ->
      st { editNote = (\n -> n { note = text }) <$> st.editNote }
  StartEditNote idx ->
    H.modify_ \st ->
      st
        { editNote =
          let
            mkEditNote n = { index: idx, note: getNoteText n }
          in
            mkEditNote <$> A.index st.notes idx
        }
  CancelEditNote -> H.modify_ \st -> st { editNote = Nothing }
  StopEditNote -> do
    st' <-
      H.modify \st ->
        st
          { editNote = Nothing
          , notes =
            fromMaybe st.notes
              $ case st.editNote of
                  Just { index, note } -> A.modifyAt index (\n -> setNoteText n note) st.notes
                  _ -> Nothing
          }
    H.raise st'.notes
