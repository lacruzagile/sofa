module Sofa.Component.EditableInput (Slot, Input(..), Output(..), Action, proxy, component) where

import Prelude
import DOM.HTML.Indexed as HTMLI
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPAria
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Sofa.HtmlUtils (focusElementByRef)
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
    , editButtonProps :: Array (HH.IProp HTMLI.HTMLbutton Action)
    -- ^ Extra properties to add to the edit button.
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
    , editButtonProps :: Array (HH.IProp HTMLI.HTMLbutton Action)
    -- ^ Extra properties to add to the edit button.
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
  , editButtonProps: input.editButtonProps
  , inputProps: input.inputProps
  , editState: Viewing
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = case state.editState of
  Viewing ->
    HH.div
      [ HP.classes
          ( Css.cs [ "flex", "items-center", "gap-x-3" ]
              <> state.classes
          )
      ]
      [ if state.value == "" then
          HH.div
            [ Css.classes [ "truncate", "text-stormy-300" ] ]
            [ HH.text state.placeholder ]
        else
          HH.div
            [ Css.class_ "truncate" ]
            [ HH.text state.value ]
      , HH.button
          ([ HE.onClick \_ -> SetEditing ] <> state.editButtonProps)
          [ Icon.editorMode
              [ Icon.classes
                  $ Css.cs
                      [ "w-[1em]"
                      , "p-[0.2em]"
                      , "rounded-full"
                      , "bg-snow-500"
                      , "fill-stormy-500"
                      , "active:bg-stormy-500"
                      , "active:fill-snow-500"
                      ]
              , Icon.ariaHidden true
              ]
          ]
      ]
  Editing value ->
    HH.form
      [ HP.classes
          ( Css.cs [ "nectary-input", "flex", "items-center", "gap-x-3" ]
              <> state.classes
          )
      , HE.onSubmit SetViewing
      ]
      [ HH.input
          ( [ HP.ref inputRef
            , HP.type_ HP.InputText
            , Css.classes [ "grow", "outline-none" ]
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
