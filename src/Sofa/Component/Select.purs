-- | A component providing a Nectary style select component.
module Sofa.Component.Select
  ( Slot
  , Output
  , proxy
  , component
  , defaultInput
  -- Export rendering for use by other "select" style components.
  , initRenderState
  , render
  ) where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Sel
import Select.Setters as SelSet
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Type.Proxy (Proxy(..))

type Slot id a
  = forall query. H.Slot query (Output a) id

proxy :: Proxy "nectaryDropdown"
proxy = Proxy

type Input a
  = { selected :: Maybe a
    , values :: Array (Tuple HH.PlainHTML a)
    , noSelectionText :: String
    , wrapperClasses :: Array HH.ClassName
    }

defaultInput :: forall a. Input a
defaultInput =
  { selected: Nothing
  , values: []
  , noSelectionText: "Please choose"
  , wrapperClasses: []
  }

type Output :: forall k. k -> k
type Output a
  = a

type State a
  = ( selectedIndex :: Maybe Int
    , values :: Array (Tuple HH.PlainHTML a)
    , noSelectionText :: String
    , wrapperClasses :: Array HH.ClassName
    )

type RenderState
  = { visibility :: Sel.Visibility
    , selectedIndex :: Maybe Int
    , highlightedIndex :: Maybe Int
    , values :: Array HH.PlainHTML
    , noSelectionText :: String
    , loading :: Boolean --  ^ Show loading spinner.
    , wrapperClasses :: Array HH.ClassName
    }

initRenderState ::
  forall props.
  { visibility :: Sel.Visibility
  , highlightedIndex :: Maybe Int
  | props
  } ->
  RenderState
initRenderState st =
  { visibility: st.visibility
  , selectedIndex: Nothing
  , highlightedIndex: st.highlightedIndex
  , values: []
  , noSelectionText: "Please select value"
  , loading: false
  , wrapperClasses: []
  }

component ::
  forall query m a.
  MonadAff m => Eq a => H.Component query (Input a) (Output a) m
component =
  H.mkComponent
    { initialState: identity
    , render: \st -> HH.slot selectLabel unit selectComponent st identity
    , eval: H.mkEval H.defaultEval { handleAction = H.raise }
    }
  where
  selectLabel = Proxy :: Proxy "select"

  selectComponent :: H.Component (Sel.Query query ()) (Input a) (Output a) m
  selectComponent =
    Sel.component mapInput
      $ Sel.defaultSpec
          { handleEvent = handleEvent
          , render = renderInner
          }

  mapInput :: (Input a) -> Sel.Input (State a)
  mapInput input =
    { inputType: Sel.Toggle
    , debounceTime: Nothing
    , search: Nothing
    , getItemCount: \st -> A.length st.values
    , selectedIndex:
        do
          selected <- input.selected
          A.findIndex (\(Tuple _ v) -> v == selected) input.values
    , values: input.values
    , noSelectionText: input.noSelectionText
    , wrapperClasses: input.wrapperClasses
    }

  handleEvent = case _ of
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { visibility = Sel.Off
            , selectedIndex = Just idx
            }
      -- Let the parent component know about the new selection.
      maybe (pure unit) (H.raise <<< snd) (st'.values !! idx)
    _ -> pure unit

  renderInner :: Sel.State (State a) -> H.ComponentHTML _ () m
  renderInner st =
    render
      $ (initRenderState st)
          { selectedIndex = st.selectedIndex
          , values = fst <$> st.values
          , noSelectionText = st.noSelectionText
          , wrapperClasses = st.wrapperClasses
          }

render :: forall act m. RenderState -> H.ComponentHTML (Sel.Action act) () m
render st =
  HH.div
    [ HP.classes st.wrapperClasses ]
    [ HH.div [ Css.class_ "relative" ] [ renderInput, renderResults ]
    ]
  where
  selected = do
    idx <- st.selectedIndex
    st.values !! idx

  renderInput :: H.ComponentHTML _ () m
  renderInput =
    HH.button
      (SelSet.setToggleProps [ Css.classes btnClasses ])
      [ maybe
          (HH.span [ Css.class_ "text-stormy-300" ] [ HH.text st.noSelectionText ])
          HH.fromPlainHTML
          selected
      ]

  btnClasses =
    [ "nectary-input"
    , "nectary-dropdown-icon"
    , "w-full"
    , "text-left"
    , "truncate"
    ]
      <> (if st.visibility == Sel.Off then [] else [ "rounded-b-none" ])

  containerClasses =
    [ "absolute"
    , "bg-white"
    , "-mt-0.5"
    , "w-full"
    , "max-h-72"
    , "overflow-auto"
    , "border"
    , "border-t-0"
    , "border-stormy-500"
    , "rounded-b-sm"
    , "shadow-md"
    , "z-10"
    ]

  renderResults :: H.ComponentHTML _ () m
  renderResults
    | st.visibility == Sel.Off = HH.text ""
    | otherwise =
      HH.ul
        ( SelSet.setContainerProps
            [ Css.classes containerClasses
            , HP.tabIndex (-1)
            ]
        )
        $ if A.null st.values then
            [ HH.div
                [ Css.classes [ "p-2", "text-stormy-300" ] ]
                [ HH.text "Sorry, no option is available" ]
            ]
          else
            A.mapWithIndex renderItem st.values

  renderItem :: Int -> HH.PlainHTML -> H.ComponentHTML _ () m
  renderItem idx item =
    HH.li
      ( SelSet.setItemProps idx
          [ Css.classes $ itemClasses <> selectedClasses <> highlightClasses
          ]
      )
      $ if st.selectedIndex == Just idx then
          [ HH.div
              [ Css.classes [ "truncate", "grow" ] ]
              [ HH.fromPlainHTML item ]
          , Icon.done [ Icon.classes [ Css.c "w-6" ] ]
          ]
        else
          [ HH.fromPlainHTML item ]
    where
    itemClasses = [ "relative", "p-2", "truncate" ]

    highlightClasses
      | st.highlightedIndex == Just idx = [ "bg-snow-500" ]
      | otherwise = []

    selectedClasses
      | st.selectedIndex == Just idx = [ "flex", "gap-x-2" ]
      | otherwise = []
