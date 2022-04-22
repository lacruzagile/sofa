-- | The order notes component of the order form.
module Sofa.App.OrderForm.Notes (Slot, Input(..), Output(..), proxy, component) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.Requests (deleteOrderNote, patchOrderNote, postOrderNote)
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "notes"
proxy = Proxy

type Input
  = { orderId :: Maybe SS.OrderId
    , notes :: Array SS.OrderNote
    }

type Output
  = Array SS.OrderNote

type State
  = { orderId :: Maybe SS.OrderId
    , notes :: Array SS.OrderNote
    , editNote :: Maybe EditNote -- ^ Note currently being edited.
    , newNote :: Maybe String -- ^ Note currently being built.
    , open :: Boolean -- ^ Whether the notes modal is open.
    , noteAction :: NoteAction
    -- ^ Action currently being performed, `NoteIdle` when no action is in progress.
    }

-- | A note action currently being performed.
data NoteAction
  = NoteIdle (Maybe NoteAction) -- ^ No action currently in progress, previous action may be provided.
  | NoteCreating (Loadable Unit)
  | NoteDeleting Int (Loadable Unit)
  | NoteUpdating Int (Loadable Unit)

-- | A note with the given index being edited.
type EditNote
  = { index :: Int, note :: String }

data Action
  = OpenDetails
  | CloseDetails
  | SetNewText String -- ^ Set note text of new note.
  | StartNewNote -- ^ Start adding a new note.
  | CancelNewNote -- ^ Cancel the new note.
  | StopNewNote Event -- ^ Stop and save the new note.
  | RemoveNote Int -- ^ Remove the note with the given index.
  | SetEditText String -- ^ Set text of current node edit.
  | StartEditNote Int -- ^ Starts editing the note with the given index.
  | CancelEditNote -- ^ Cancel current note edit.
  | StopEditNote Int Event -- ^ Stop and save current note edit with the given index.

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
  , notes: input.notes
  , editNote: Nothing
  , newNote: Nothing
  , open: false
  , noteAction: NoteIdle Nothing
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
  btn = HH.button [ Css.classes btnClasses, HE.onClick $ \_ -> OpenDetails ] label

  label = case A.length st.notes of
    0 -> [ HH.text "No order notes" ]
    1 -> [ HH.text "1 order note" ]
    n -> [ HH.text (show n), HH.text " order notes" ]

  btnClasses =
    [ "block"
    , "text-left"
    , "underline"
    , "underline-offset-4"
    , "decoration-honey-500"
    ]

renderDetails ::
  forall slots m.
  MonadAff m => State -> H.ComponentHTML Action slots m
renderDetails st =
  HH.div_
    [ renderSummary st
    , Modal.render
        $ Modal.defaultInput
            { title = HH.text "Notes"
            , closeAction = Just (const CloseDetails)
            , content = renderContent
            }
    ]
  where
  renderContent =
    HH.div
      [ Css.classes
          [ "w-full"
          , "min-w-96"
          , "max-w-128"
          , "flex"
          , "flex-col"
          , "space-y-4"
          ]
      ]
      [ if A.null st.notes then
          HH.div
            [ Css.classes [ "text-lg", "text-gray-600" ] ]
            [ HH.text "No notes available" ]
        else
          HH.div
            [ Css.classes
                [ "flex"
                , "flex-col"
                , "space-y-4"
                , "max-h-128"
                , "overflow-auto"
                ]
            ]
            (A.mapWithIndex renderNote st.notes)
      , renderFooter
      ]

  actionsAllowed = case st.noteAction of
    NoteIdle _ -> true
    _ -> false

  renderFooter = case st.newNote of
    _
      | isJust st.editNote -> HH.text "" -- Skip footer when editing.
    Nothing ->
      HH.div [ Css.classes [ "flex", "space-x-4" ] ]
        [ HH.div [ Css.class_ "grow" ] []
        , HH.button
            [ Css.class_ "nectary-btn-secondary"
            , HP.enabled actionsAllowed
            , HE.onClick \_ -> CloseDetails
            ]
            [ HH.text "Close" ]
        , HH.button
            [ Css.class_ "nectary-btn-primary"
            , HP.enabled actionsAllowed
            , HE.onClick \_ -> StartNewNote
            ]
            [ HH.text "Add" ]
        ]
    Just text ->
      HH.form [ HE.onSubmit StopNewNote ]
        [ HH.textarea
            [ Css.classes [ "nectary-input", "h-36", "w-full" ]
            , HP.placeholder "Note text."
            , HP.value text
            , HE.onValueChange SetNewText
            ]
        , HH.div [ Css.class_ "text-raspberry-500" ]
            $ maybe [] (\msg -> [ HH.text msg ])
            $ createError
        , HH.div
            [ Css.classes
                [ "flex"
                , "space-x-4"
                , "mt-3"
                , "mb-0.5" -- Avoid clipping of buttons.
                ]
            ]
            [ HH.div [ Css.class_ "grow" ] []
            , HH.button
                [ Css.class_ "nectary-btn-secondary"
                , HP.enabled actionsAllowed
                , HE.onClick \_ -> CancelNewNote
                ]
                [ HH.text "Cancel" ]
            , HH.button
                [ HP.type_ HP.ButtonSubmit
                , Css.class_ "nectary-btn-primary"
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
      isCreating = case st.noteAction of
        NoteCreating Loading -> true
        _ -> false

      createError = case st.noteAction of
        NoteIdle (Just (NoteCreating (Error msg))) -> Just msg
        _ -> Nothing

  renderNote idx n = case st.editNote of
    Just { index, note }
      | idx == index -> renderEditNote idx note
    _ -> renderShowNote idx n

  renderShowNote idx (SS.OrderNote n) =
    HH.div [ Css.classes [ "group", "py-3" ] ]
      [ HH.text n.note
      , HH.div
          [ Css.classes
              [ "text-sm"
              , "text-gray-600"
              , "flex"
              , "space-x-2"
              , "mr-0.5" -- Avoid clipping of buttons.
              ]
          ]
          [ HH.div_
              [ maybe
                  (HH.text "New")
                  Widgets.dateWithTimeTooltipRight
                  n.createTime
              ]
          , HH.div [ Css.class_ "grow" ] []
          , HH.button
              [ HP.classes
                  $ Css.cs
                      [ "nectary-btn-destructive"
                      , "h-auto"
                      , "py-0"
                      ]
                  <> hideable isDeleting
              , HP.enabled actionsAllowed
              , HE.onClick $ \_ -> RemoveNote idx
              ]
              [ HH.text "Remove"
              , spinner isDeleting
              ]
          , HH.button
              [ HP.classes
                  $ Css.cs
                      [ "nectary-btn-primary"
                      , "h-auto"
                      , "py-0"
                      ]
                  <> hideable false
              , HP.enabled actionsAllowed
              , HE.onClick $ \_ -> StartEditNote idx
              ]
              [ HH.text "Edit" ]
          ]
      , maybe (HH.text "")
          (\msg -> HH.div [ Css.class_ "text-raspberry-500" ] [ HH.text msg ])
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

    isDeleting = case st.noteAction of
      NoteDeleting didx Loading
        | idx == didx -> true
      _ -> false

    deleteError = case st.noteAction of
      NoteIdle (Just (NoteDeleting didx (Error msg)))
        | idx == didx -> Just msg
      _ -> Nothing

  renderEditNote idx note =
    HH.form [ HE.onSubmit (StopEditNote idx) ]
      [ HH.textarea
          [ Css.classes [ "nectary-input", "h-36", "w-full" ]
          , HP.placeholder "Note text."
          , HP.value note
          , HE.onValueChange SetEditText
          ]
      , HH.div [ Css.classes [ "text-raspberry-500", "w-full" ] ]
          $ maybe [] (\msg -> [ HH.text msg ])
          $ updateError
      , HH.div
          [ Css.classes
              [ "flex"
              , "space-x-4"
              , "mt-2"
              , "mr-0.5" -- Avoid clipping of buttons.
              , "mb-0.5" -- Avoid clipping of buttons.
              ]
          ]
          [ HH.div [ Css.class_ "grow" ] []
          , HH.button
              [ Css.class_ "nectary-btn-secondary"
              , HP.type_ HP.ButtonButton
              , HP.enabled actionsAllowed
              , HE.onClick \_ -> CancelEditNote
              ]
              [ HH.text "Cancel" ]
          , HH.button
              [ Css.class_ "nectary-btn-primary"
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
    isUpdating = case st.noteAction of
      NoteUpdating didx Loading
        | idx == didx -> true
      _ -> false

    updateError = case st.noteAction of
      NoteIdle (Just (NoteUpdating didx (Error msg)))
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
      st { newNote = Nothing, open = false }
  SetNewText text -> H.modify_ \st -> st { newNote = Just text }
  StartNewNote -> H.modify_ \st -> st { newNote = Just "" }
  CancelNewNote -> H.modify_ \st -> st { newNote = Nothing }
  StopNewNote event -> do
    H.liftEffect $ Event.preventDefault event
    state <- H.modify \st -> st { noteAction = NoteCreating Loading }
    case Tuple state.orderId state.newNote of
      Tuple (Just oid) (Just text) -> do
        noteResult <- H.lift $ postOrderNote oid (mkNote text)
        state' <-
          H.modify \st ->
            st
              { notes =
                fromMaybe st.notes
                  $ (\note -> st.notes <> [ note ])
                  <$> Loadable.toMaybe noteResult
              , newNote = Nothing
              , noteAction = NoteIdle (Just st.noteAction)
              }
        H.raise state'.notes
      _ -> pure unit
  RemoveNote idx -> do
    state <- H.modify \st -> st { noteAction = NoteDeleting idx Loading }
    let
      mNote = A.index state.notes idx
    noteResult <- case Tuple state.orderId mNote of
      Tuple
        (Just oid)
        (Just (SS.OrderNote { orderNoteId: Just nid })) -> H.lift $ deleteOrderNote oid nid
      _ -> pure Idle
    state' <-
      H.modify \st ->
        st
          { notes =
            fromMaybe st.notes do
              _ <- Loadable.toMaybe noteResult
              A.deleteAt idx st.notes
          , noteAction = NoteIdle (Just $ NoteDeleting idx noteResult)
          }
    H.raise state'.notes
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
  StopEditNote idx event -> do
    H.liftEffect $ Event.preventDefault event
    state <- H.modify \st -> st { noteAction = NoteUpdating idx Loading }
    let
      mNote = A.index state.notes idx
    noteResult <- case { oid: state.orderId, note: mNote, enote: state.editNote } of
      { oid: Just oid
      , note: Just note@(SS.OrderNote { orderNoteId: Just nid })
      , enote: Just { note: text }
      } -> H.lift $ patchOrderNote oid nid (setNoteText note text)
      _ -> pure Idle
    state' <-
      H.modify \st ->
        st
          { editNote = Nothing
          , notes =
            fromMaybe st.notes do
              newNote <- Loadable.toMaybe noteResult
              A.modifyAt idx (const newNote) st.notes
          , noteAction =
            NoteIdle
              $ Just
              $ NoteUpdating idx (const unit <$> noteResult)
          }
    H.raise state'.notes
