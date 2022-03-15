-- | A component providing a Nectary style select component.
module Component.Select (Slot, Output, proxy, component, defaultInput) where

import Prelude
import Css as Css
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
  = ( selected :: Maybe (Tuple HH.PlainHTML a)
    , values :: Array (Tuple HH.PlainHTML a)
    , noSelectionText :: String
    , wrapperClasses :: Array HH.ClassName
    )

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
          , render = render
          }

  mapInput :: (Input a) -> Sel.Input (State a)
  mapInput input =
    { inputType: Sel.Toggle
    , debounceTime: Nothing
    , search: Nothing
    , getItemCount: \st -> A.length st.values
    , selected:
        do
          selected <- input.selected
          A.find (\(Tuple _ v) -> v == selected) input.values
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
            , selected = st.values !! idx
            }
      -- Let the parent component know about the new selection.
      maybe (pure unit) (H.raise <<< snd) st'.selected
    _ -> pure unit

  render :: Sel.State (State a) -> H.ComponentHTML _ () m
  render st =
    HH.div
      [ HP.classes $ [ Css.c "w-max" ] <> st.wrapperClasses ]
      [ HH.div [ HP.class_ (Css.c "relative") ] [ renderInput, renderResults ]
      ]
    where
    renderInput :: H.ComponentHTML _ () m
    renderInput =
      HH.button
        (SelSet.setToggleProps [ HP.classes btnClasses ])
        [ maybe
            (HH.span [ HP.class_ (Css.c "text-stormy-300") ] [ HH.text st.noSelectionText ])
            (HH.fromPlainHTML <<< fst)
            st.selected
        ]

    btnClasses
      | st.visibility == Sel.Off =
        [ Css.c "nectary-input"
        , Css.c "nectary-dropdown-icon"
        , Css.c "w-full"
        , Css.c "text-left"
        , Css.c "truncate"
        ]
      | otherwise =
        [ Css.c "nectary-input"
        , Css.c "nectary-dropdown-icon"
        , Css.c "w-full"
        , Css.c "text-left"
        , Css.c "truncate"
        , Css.c "rounded-b-none"
        ]

    containerClasses =
      [ Css.c "absolute"
      , Css.c "bg-white"
      , Css.c "-mt-0.5"
      , Css.c "w-full"
      , Css.c "max-h-72"
      , Css.c "overflow-auto"
      , Css.c "border"
      , Css.c "border-t-0"
      , Css.c "border-stormy-500"
      , Css.c "rounded-b-sm"
      , Css.c "shadow-md"
      , Css.c "z-10"
      ]

    renderResults :: H.ComponentHTML _ () m
    renderResults
      | st.visibility == Sel.Off = HH.text ""
      | otherwise =
        HH.ul (SelSet.setContainerProps [ HP.classes containerClasses ])
          $ A.mapWithIndex renderItem st.values

    renderItem :: Int -> Tuple HH.PlainHTML a -> H.ComponentHTML _ () m
    renderItem idx (Tuple key value) =
      HH.li
        ( SelSet.setItemProps idx
            [ HP.classes $ itemClasses <> selectedClasses <> highlightClasses
            ]
        )
        [ HH.fromPlainHTML key ]
      where
      itemClasses =
        [ Css.c "relative"
        , Css.c "p-2"
        , Css.c "pr-8"
        , Css.c "truncate"
        ]

      highlightClasses
        | st.highlightedIndex == Just idx = [ Css.c "bg-snow-500" ]
        | otherwise = []

      selectedClasses
        | Just value == map snd st.selected = [ Css.c "nectary-icon-check" ]
        | otherwise = []
