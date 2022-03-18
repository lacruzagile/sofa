-- | Provides Nectary style rendering for a type-ahead component driven by
-- | Halogen Select.
module Component.Typeahead (initRenderState, render) where

import Prelude
import Css as Css
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Select as Sel
import Select.Setters as SelSet
import Widgets as Widgets

type RenderState
  = { visibility :: Sel.Visibility
    , selected :: Maybe String
    , selectedIndex :: Maybe Int
    , highlightedIndex :: Maybe Int
    , values :: Array HH.PlainHTML
    , noSelectionText :: String
    , loading :: Boolean --  ^ Show loading spinner.
    , wrapperClasses :: Array HH.ClassName
    , inputClasses :: Array HH.ClassName
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
  , selected: Nothing
  , selectedIndex: Nothing
  , highlightedIndex: st.highlightedIndex
  , values: []
  , noSelectionText: "Please select value"
  , loading: false
  , wrapperClasses: []
  , inputClasses: []
  }

render :: forall act m. RenderState -> H.ComponentHTML (Sel.Action act) () m
render st =
  HH.div [ HP.classes st.wrapperClasses ]
    [ HH.div [ HP.class_ (Css.c "relative") ]
        [ if st.loading then
            Widgets.spinner [ Css.c "absolute", Css.c "right-2.5", Css.c "top-4" ]
          else
            HH.text ""
        , renderInput
        , renderResults
        ]
    ]
  where
  renderInput :: H.ComponentHTML _ () m
  renderInput =
    HH.input
      $ SelSet.setInputProps
      $ [ HP.classes $ inputClasses <> st.inputClasses
        , HP.placeholder st.noSelectionText
        ]
      <> maybe [] (A.singleton <<< HP.value) st.selected

  inputClasses =
    [ Css.c "nectary-input"
    , Css.c "w-full"
    , Css.c "text-left"
    , Css.c "truncate"
    ]
      <> (if st.loading then [ Css.c "pr-8" ] else [ Css.c "nectary-dropdown-icon" ])
      <> (if st.visibility == Sel.Off then [] else [ Css.c "rounded-b-none" ])

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
    | st.visibility == Sel.Off || A.null st.values = HH.text ""
    | otherwise =
      HH.ul
        ( SelSet.setContainerProps
            [ HP.classes containerClasses
            , HP.tabIndex (-1)
            ]
        )
        $ A.mapWithIndex renderItem st.values

  renderItem :: Int -> HH.PlainHTML -> H.ComponentHTML _ () m
  renderItem idx value =
    HH.li
      ( SelSet.setItemProps idx
          [ HP.classes $ itemClasses <> selectedClasses <> highlightClasses
          ]
      )
      [ HH.fromPlainHTML value ]
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
      | st.selectedIndex == Just idx = [ Css.c "nectary-icon-check" ]
      | otherwise = []
