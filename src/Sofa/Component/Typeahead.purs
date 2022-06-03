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
import Sofa.Component.Spinner as Spinner
import Sofa.Css as Css
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
    [ HH.div [ Css.class_ "relative" ]
        [ if st.loading then
            Spinner.render
              $ Spinner.defaults
                  { classes = Css.cs [ "absolute", "right-2.5", "top-4" ]
                  }
          else
            HH.text ""
        , Icon.search
            [ Icon.classes
                [ Css.c "absolute"
                , Css.c "inset-y-0"
                , Css.c "left-2.5"
                , Css.c "w-6"
                , Css.c "h-full"
                ]
            , Icon.ariaHidden true
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
        , HP.classes $ Css.cs inputClasses <> st.inputClasses
        , HP.placeholder st.noSelectionText
        ]
      <> maybe [] (A.singleton <<< HP.value) st.selected
      <> maybe [] (\act -> [ HE.onFocus (Sel.Action <<< act) ]) st.onInputFocus

  inputClasses =
    [ "nectary-input"
    , "w-full"
    , "text-left"
    , "truncate"
    , "pl-11"
    ]
      <> (if st.loading then [ "pr-8" ] else [])
      <> (if st.loading || A.null st.values then [] else [ "nectary-dropdown-icon" ])
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
    | st.visibility == Sel.Off || A.null st.values = HH.text ""
    | otherwise =
      HH.ul
        ( SelSet.setContainerProps
            [ Css.classes containerClasses
            , HP.tabIndex (-1)
            ]
        )
        $ A.mapWithIndex renderItem st.values

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
