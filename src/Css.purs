module Css where

import Halogen (ClassName(..))

navbar :: ClassName
navbar = ClassName "navbar"

hblock :: ClassName
hblock = ClassName "hblock"

-- Picnic CSS
brand :: ClassName
brand = ClassName "brand"

menu :: ClassName
menu = ClassName "menu"

burger :: ClassName
burger = ClassName "burger"

pseudo :: ClassName
pseudo = ClassName "pseudo"

button :: ClassName
button = ClassName "button"

active :: ClassName
active = ClassName "active"

show :: ClassName
show = ClassName "show"

card :: ClassName
card = ClassName "card"

modal :: ClassName
modal = ClassName "modal"

overlay :: ClassName
overlay = ClassName "overlay"

content :: ClassName
content = ClassName "content"

close :: ClassName
close = ClassName "close"

-- modal ::
--   forall slot action.
--   String -> String -> Array (HH.HTML slot action) -> HH.HTML slot action
-- modal label title body =
--   HH.div [ HP.class_ Css.modal ]
--     [ HH.input [ HP.id label, HP.type_ InputCheckbox ]
--     , HH.label [ HP.for label, HP.class_ Css.overlay ] []
--     , HH.article_
--         [ HH.header_
--             [ HH.h3_ [ HH.text title ]
--             , HH.label [ HP.for label, HP.class_ Css.close ] [ HH.text "&times;" ]
--             ]
--         , HH.section [ HP.class_ Css.content ] body
--         ]
--     ]
-- textModal ::
--   forall slot action.
--   String -> String -> String -> HH.HTML slot action
-- textModal label title body =
--   HH.div [ HP.class_ Css.modal ]
--     [ HH.input [ HP.id label, HP.type_ InputCheckbox ]
--     , HH.label [ HP.for label, HP.class_ Css.overlay ] []
--     , HH.article_
--         [ HH.header_
--             [ HH.h3_ [ HH.text title ]
--             , HH.label [ HP.for label, HP.class_ Css.close ] [ HH.text "&times;" ]
--             ]
--         , HH.section [ HP.class_ Css.content ] [HH.p_ [ HH.text body]]
--         ]
--     ]
