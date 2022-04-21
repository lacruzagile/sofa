-- | Provides Nectary style rendering for a type-ahead component driven by
-- | Halogen Select.
module Sofa.Component.Typeahead (initRenderState, render) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Sel
import Select.Setters as SelSet
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Sofa.Widgets as Widgets
import Web.UIEvent.FocusEvent as Event

type RenderState act
  = { visibility :: Sel.Visibility
    , selected :: Maybe String
    , selectedIndex :: Maybe Int
    , highlightedIndex :: Maybe Int
    , values :: Array HH.PlainHTML
    , noSelectionText :: String
    , loading :: Boolean --  ^ Show loading spinner.
    , wrapperClasses :: Array HH.ClassName
    , inputClasses :: Array HH.ClassName
    , onInputFocus :: Maybe (Event.FocusEvent -> act) -- ^ Action when input is focused.
    }

initRenderState ::
  forall act props.
  { visibility :: Sel.Visibility
  , highlightedIndex :: Maybe Int
  | props
  } ->
  RenderState act
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
  , onInputFocus: Nothing
  }

render :: forall act m. RenderState act -> H.ComponentHTML (Sel.Action act) () m
render st =
  HH.div [ HP.classes st.wrapperClasses ]
    [ HH.div [ HP.class_ (Css.c "relative") ]
        [ if st.loading then
            Widgets.spinner [ Css.c "absolute", Css.c "right-2.5", Css.c "top-4" ]
          else
            HH.text ""
        , HH.div [ HP.classes [ Css.c "absolute", Css.c "left-2.5", Css.c "top-3.5" ] ]
            [ Icon.search
                [ Icon.classes [ Css.c "w-6", Css.c "h-6" ]
                , Icon.ariaHidden true
                ]
            ]
        , renderInput
        , renderResults
        ]
    ]
  where
  renderInput :: H.ComponentHTML _ () m
  renderInput =
    HH.input
      $ SelSet.setInputProps
      $ [ HP.type_ HP.InputSearch
        , HP.classes $ inputClasses <> st.inputClasses
        , HP.placeholder st.noSelectionText
        ]
      <> maybe [] (A.singleton <<< HP.value) st.selected
      <> maybe [] (\act -> [ HE.onFocus (Sel.Action <<< act) ]) st.onInputFocus

  inputClasses =
    [ Css.c "nectary-input"
    , Css.c "w-full"
    , Css.c "text-left"
    , Css.c "truncate"
    , Css.c "pl-11"
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
  renderItem idx item =
    HH.li
      ( SelSet.setItemProps idx
          [ HP.classes $ itemClasses <> selectedClasses <> highlightClasses
          ]
      )
      $ if st.selectedIndex == Just idx then
          [ HH.div
              [ HP.classes [ Css.c "truncate", Css.c "grow" ] ]
              [ HH.fromPlainHTML item ]
          , Icon.done [ Icon.classes [ Css.c "w-6" ] ]
          ]
        else
          [ HH.fromPlainHTML item ]
    where
    itemClasses = [ Css.c "relative", Css.c "p-2", Css.c "truncate" ]

    highlightClasses
      | st.highlightedIndex == Just idx = [ Css.c "bg-snow-500" ]
      | otherwise = []

    selectedClasses
      | st.selectedIndex == Just idx = [ Css.c "flex", Css.c "gap-x-2" ]
      | otherwise = []
