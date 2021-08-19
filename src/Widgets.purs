module Widgets (tabbed2, Tab(..), modal) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Css as Css

-- Other stuff.
type Tab slot action
  = { label :: HH.HTML slot action
    , content :: HH.HTML slot action
    }

tabbed2 ::
  forall slot action.
  String ->
  Tab slot action ->
  Tab slot action ->
  HH.HTML slot action
tabbed2 id tab1 tab2 =
  HH.div [ HP.classes [ Css.tabs, Css.two ] ]
    ( input "1" tab1
        <> input "2" tab2
        <> [ HH.div [ HP.class_ Css.row ] [ tab1.content, tab2.content ]
          ]
    )
  where
  input n t =
    [ HH.input
        [ HP.id (id <> n)
        , HP.type_ HP.InputRadio
        , HP.name ("tabgroup-" <> id)
        , HP.checked (n == "1")
        ]
    , HH.label
        [ HP.classes [ Css.pseudo, Css.button, Css.toggle ]
        , HP.for (id <> n)
        ]
        [ t.label ]
    ]

modal ::
  forall slot action.
  String ->
  String ->
  Array (HH.HTML slot action) ->
  Array (HH.HTML slot action) ->
  HH.HTML slot action
modal label title body footer =
  HH.div [ HP.class_ Css.modal ]
    [ HH.input [ HP.id label, HP.type_ HP.InputCheckbox ]
    , HH.label [ HP.for label, HP.class_ Css.overlay ] []
    , HH.article_
        [ HH.header_
            [ HH.h3_ [ HH.text title ]
            , HH.label [ HP.for label, HP.class_ Css.close ] [ HH.text "×" ]
            ]
        , HH.section [ HP.class_ Css.content ] body
        , HH.footer_ footer
        ]
    ]
