module Sofa.Component.EditableInput (Slot, Input(..), Output(..), Action, proxy, component) where

import Prelude
import DOM.HTML.Indexed as HTMLI
import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (focusElementByRef)
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "editableInput"
proxy = Proxy

type Input
  = { value :: String
    , placeholder :: String
    , classes :: Array HH.ClassName
    -- ^ Extra classes to add to the view and edit elements.
    , inputProps :: Array (HH.IProp HTMLI.HTMLinput Action)
    -- ^ Extra properties to add to the input element.
    }

type Output
  = String

data EditState
  = Editing String
  | Viewing

type State
  = { value :: String
    , placeholder :: String
    , classes :: Array HH.ClassName
    -- ^ Extra classes to add to the view and edit elements.
    , inputProps :: Array (HH.IProp HTMLI.HTMLinput Action)
    -- ^ Extra properties to add to the input element.
    , editState :: EditState
    }

data Action
  = SetEditing
  | SetViewing Event
  | UpdateValue String

inputRef :: H.RefLabel
inputRef = H.RefLabel "input"

component ::
  forall query m.
  MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState input =
  { value: input.value
  , placeholder: input.placeholder
  , classes: input.classes
  , inputProps: input.inputProps
  , editState: Viewing
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = case state.editState of
  Viewing ->
    HH.div
      [ HP.classes
          ( [ Css.c "flex", Css.c "items-center", Css.c "gap-x-3" ]
              <> state.classes
          )
      ]
      [ if state.value == "" then
          HH.span [ HP.class_ (Css.c "text-stormy-300") ] [ HH.text state.placeholder ]
        else
          HH.text state.value
      , HH.button
          [ HE.onClick \_ -> SetEditing
          , HPAria.label "Edit order name"
          ]
          [ Icon.editorMode
              [ Icon.classes
                  [ Css.c "w-[1em]"
                  , Css.c "p-[0.2em]"
                  , Css.c "rounded-full"
                  , Css.c "bg-snow-500"
                  , Css.c "fill-stormy-500"
                  , Css.c "active:bg-stormy-500"
                  , Css.c "active:fill-snow-500"
                  ]
              , Icon.ariaHidden true
              ]
          ]
      ]
  Editing value ->
    HH.form
      [ HP.classes
          ( [ Css.c "nectary-input", Css.c "flex", Css.c "items-center", Css.c "gap-x-3" ]
              <> state.classes
          )
      , HE.onSubmit SetViewing
      ]
      [ HH.input
          ( [ HP.ref inputRef
            , HP.type_ HP.InputText
            , HP.classes [ Css.c "grow", Css.c "outline-none" ]
            , HP.value value
            , HP.placeholder state.placeholder
            , HE.onValueInput UpdateValue
            ]
              <> state.inputProps
          )
      , Icon.keyboardEnter
          [ Icon.classes [ Css.c "w-[1em]" ]
          , HPAria.hidden "true"
          ]
      ]

handleAction ::
  forall m.
  MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  SetEditing -> do
    H.modify_ \st -> st { editState = Editing st.value }
    focusElementByRef inputRef
  SetViewing event -> do
    H.liftEffect $ Event.preventDefault event
    state <-
      H.modify \st ->
        st
          { value =
            case st.editState of
              Viewing -> st.value
              Editing value -> value
          , editState = Viewing
          }
    H.raise state.value
  UpdateValue content ->
    H.modify_ \st ->
      st
        { editState =
          case st.editState of
            Viewing -> Viewing
            Editing _ -> Editing content
        }
