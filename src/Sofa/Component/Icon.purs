module Sofa.Component.Icon
  ( add
  , ariaHidden
  , ariaLabel
  , cancel
  , checkmark
  , classes
  , close
  , id
  , longMessage
  , package
  , piggybank
  , puzzle
  , role
  , settings
  , sinchLogo
  , textWithTooltip
  , tooltip
  , upload
  , user
  ) where

import Prelude
import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Safe.Coerce (coerce)
import Sofa.Css as Css

add :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
add attrs =
  svg
    ( [ viewBox "0 0 24 24"
      , role "img"
      ]
        <> attrs
    )
    [ path
        [ d "M18 13H13V18C13 18.55 12.55 19 12 19C11.45 19 11 18.55 11 18V13H6C5.45 13 5 12.55 5 12C5 11.45 5.45 11 6 11H11V6C11 5.45 11.45 5 12 5C12.55 5 13 5.45 13 6V11H18C18.55 11 19 11.45 19 12C19 12.55 18.55 13 18 13Z"
        ]
    ]

-- | A plain text followed by a tooltip icon.
textWithTooltip :: forall w i. String -> HH.HTML w i
textWithTooltip text =
  HH.span
    [ HP.classes [ Css.c "flex", Css.c "items-center" ] ]
    [ HH.text text, tooltip ]

-- | A tooltip indication icon. Rendered as a question mark inside a circle.
tooltip :: forall w i. HH.HTML w i
tooltip =
  svg
    [ viewBox "0 0 16 16"
    , role "img"
    , classes
        [ Css.c "inline-block"
        , Css.c "fill-current"
        , Css.c "ml-1"
        , Css.c "w-4"
        , Css.c "h-4"
        ]
    ]
    [ path [ d "M7.25 12.5h1.5V11h-1.5v1.5ZM8 .5C3.86.5.5 3.86.5 8c0 4.14 3.36 7.5 7.5 7.5 4.14 0 7.5-3.36 7.5-7.5C15.5 3.86 12.14.5 8 .5ZM8 14c-3.308 0-6-2.693-6-6 0-3.308 2.692-6 6-6 3.307 0 6 2.692 6 6 0 3.307-2.693 6-6 6ZM8 3.5a3 3 0 0 0-3 3h1.5C6.5 5.675 7.175 5 8 5s1.5.675 1.5 1.5c0 1.5-2.25 1.313-2.25 3.75h1.5C8.75 8.562 11 8.375 11 6.5a3 3 0 0 0-3-3Z" ]
    ]

cancel :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
cancel attrs =
  svg
    ( [ viewBox "0 0 24 24"
      , role "img"
      ]
        <> attrs
    )
    [ path
        [ d "M12 2C6.47 2 2 6.47 2 12C2 17.53 6.47 22 12 22C17.53 22 22 17.53 22 12C22 6.47 17.53 2 12 2ZM16.3 16.3C15.91 16.69 15.28 16.69 14.89 16.3L12 13.41L9.11 16.3C8.72 16.69 8.09 16.69 7.7 16.3C7.31 15.91 7.31 15.28 7.7 14.89L10.59 12L7.7 9.11C7.31 8.72 7.31 8.09 7.7 7.7C8.09 7.31 8.72 7.31 9.11 7.7L12 10.59L14.89 7.7C15.28 7.31 15.91 7.31 16.3 7.7C16.69 8.09 16.69 8.72 16.3 9.11L13.41 12L16.3 14.89C16.68 15.27 16.68 15.91 16.3 16.3Z"
        ]
    ]

checkmark :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
checkmark attrs =
  svg
    ([ viewBox "0 0 14 14" ] <> attrs)
    [ path [ d "M14 1.99999L12.59 0.579987L4.98995 8.17L1.49997 4.5L0.0799694 5.91L4.98995 11L14 1.99999Z" ]
    ]

close :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
close attrs =
  svg
    ( [ viewBox "0 0 14 14"
      , role "img"
      , classes
          [ Css.c "inline-block"
          , Css.c "fill-current"
          , Css.c "w-3"
          , Css.c "h-3"
          ]
      ]
        <> attrs
    )
    [ path
        [ d "M13.3.71a.996.996 0 0 0-1.41 0L7 5.59 2.11.7A.996.996 0 1 0 .7 2.11L5.59 7 .7 11.89a.996.996 0 1 0 1.41 1.41L7 8.41l4.89 4.89a.996.996 0 1 0 1.41-1.41L8.41 7l4.89-4.89c.38-.38.38-1.02 0-1.4Z"
        ]
    ]

longMessage :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
longMessage attrs =
  svg
    ([ viewBox "0 0 48 48", role "img" ] <> attrs)
    [ path
        [ d "M27.0928 33H15.9353C15.6872 33 15.4493 32.8946 15.2739 32.7071C15.0985 32.5196 15 32.2652 15 32C15 31.7348 15.0985 31.4804 15.2739 31.2929C15.4493 31.1054 15.6872 31 15.9353 31H27.0647C27.3128 31 27.5507 31.1054 27.7261 31.2929C27.9015 31.4804 28 31.7348 28 32C28 32.2652 27.9015 32.5196 27.7261 32.7071C27.5507 32.8946 27.3128 33 27.0647 33H27.0928Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    , path
        [ d "M27.0928 25H15.9353C15.6872 25 15.4493 24.8946 15.2739 24.7071C15.0985 24.5196 15 24.2652 15 24C15 23.7348 15.0985 23.4804 15.2739 23.2929C15.4493 23.1054 15.6872 23 15.9353 23H27.0647C27.3128 23 27.5507 23.1054 27.7261 23.2929C27.9015 23.4804 28 23.7348 28 24C28 24.2652 27.9015 24.5196 27.7261 24.7071C27.5507 24.8946 27.3128 25 27.0647 25H27.0928Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    , path
        [ d "M27.0928 17H15.9353C15.6872 17 15.4493 16.8946 15.2739 16.7071C15.0985 16.5196 15 16.2652 15 16C15 15.7348 15.0985 15.4804 15.2739 15.2929C15.4493 15.1054 15.6872 15 15.9353 15H27.0647C27.3128 15 27.5507 15.1054 27.7261 15.2929C27.9015 15.4804 28 15.7348 28 16C28 16.2652 27.9015 16.5196 27.7261 16.7071C27.5507 16.8946 27.3128 17 27.0647 17H27.0928Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    , path
        [ d "M27.0928 29H15.9353C15.6872 29 15.4493 28.8946 15.2739 28.7071C15.0985 28.5196 15 28.2652 15 28C15 27.7348 15.0985 27.4804 15.2739 27.2929C15.4493 27.1054 15.6872 27 15.9353 27H27.0647C27.3128 27 27.5507 27.1054 27.7261 27.2929C27.9015 27.4804 28 27.7348 28 28C28 28.2652 27.9015 28.5196 27.7261 28.7071C27.5507 28.8946 27.3128 29 27.0647 29H27.0928Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    , path
        [ d "M27.0928 21H15.9353C15.6872 21 15.4493 20.8946 15.2739 20.7071C15.0985 20.5196 15 20.2652 15 20C15 19.7348 15.0985 19.4804 15.2739 19.2929C15.4493 19.1054 15.6872 19 15.9353 19H27.0647C27.3128 19 27.5507 19.1054 27.7261 19.2929C27.9015 19.4804 28 19.7348 28 20C28 20.2652 27.9015 20.5196 27.7261 20.7071C27.5507 20.8946 27.3128 21 27.0647 21H27.0928Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    , path
        [ d "M27.0928 13H15.9353C15.6872 13 15.4493 12.8946 15.2739 12.7071C15.0985 12.5196 15 12.2652 15 12C15 11.7348 15.0985 11.4804 15.2739 11.2929C15.4493 11.1054 15.6872 11 15.9353 11H27.0647C27.3128 11 27.5507 11.1054 27.7261 11.2929C27.9015 11.4804 28 11.7348 28 12C28 12.2652 27.9015 12.5196 27.7261 12.7071C27.5507 12.8946 27.3128 13 27.0647 13H27.0928Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    , path
        [ d "M34.45 16.38H33V9C33 8.20435 32.6839 7.44129 32.1213 6.87868C31.5587 6.31607 30.7956 6 30 6H13C12.2044 6 11.4413 6.31607 10.8787 6.87868C10.3161 7.44129 10 8.20435 10 9V35.92C10 36.7156 10.3161 37.4787 10.8787 38.0413C11.4413 38.6039 12.2044 38.92 13 38.92H31C32.8565 38.92 34.637 38.1825 35.9497 36.8697C37.2625 35.557 38 33.7765 38 31.92V19.87C37.9987 19.407 37.9056 18.9488 37.726 18.522C37.5464 18.0953 37.2839 17.7084 36.9537 17.3838C36.6235 17.0592 36.2322 16.8033 35.8024 16.631C35.3727 16.4587 34.913 16.3734 34.45 16.38ZM36.003 31.92C36.0031 33.2375 35.4301 34.5517 34.5033 35.488C33.5764 36.4242 32.3174 36.9568 31 36.97H13C12.7348 36.97 12.4804 36.8646 12.2929 36.6771C12.1054 36.4896 12 36.2352 12 35.97V9C12 8.73478 12.1054 8.48043 12.2929 8.29289C12.4804 8.10536 12.7348 8 13 8H30C30.2652 8 30.5196 8.10536 30.7071 8.29289C30.8946 8.48043 31 8.73478 31 9V31.2L33 30.05V18.38H34.5033C34.8994 18.38 35.2794 18.5367 35.5604 18.8158C35.8414 19.0949 36.0004 19.4239 36.003 19.82V31.92Z"
        , classes [ Css.c "fill-stormy-500" ]
        ]
    ]

package :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
package attrs =
  svg ([ viewBox "0 0 48 48", role "img" ] <> attrs)
    [ path
        [ d "M17.5 14.5L28.2757 20.3612L30.9274 18.9584L19.7307 13.3199L17.5 14.5Z"
        , classes [ Css.c "fill-stormy-500" ]
        ]
    , path
        [ fillRule "evenodd"
        , clipRule "evenodd"
        , d "M23.5324 9.11607C23.8249 8.96131 24.1751 8.96131 24.4676 9.11607L37.4676 15.9933C37.7951 16.1665 38 16.5067 38 16.8772V31.1228C38 31.4933 37.7951 31.8335 37.4676 32.0067L24.4676 38.8839C24.1546 39.0495 23.7774 39.0372 23.4759 38.8517L10.4759 30.8517C10.1802 30.6697 10 30.3473 10 30V16.8772C10 16.5067 10.2049 16.1665 10.5324 15.9933L23.5324 9.11607ZM12 18.5375L23 24.3567V36.2104L12 29.4412V18.5375ZM25 36.3397L36 30.5205V18.5375L25 24.3567V36.3397ZM24 22.6231L34.8615 16.8772L24 11.1313L13.1385 16.8772L24 22.6231Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    ]

piggybank :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
piggybank attrs =
  svg ([ viewBox "0 0 48 48", role "img" ] <> attrs)
    [ path
        [ fillRule "evenodd"
        , clipRule "evenodd"
        , d "M21.0532 14.197C20.2978 14.3332 19.8282 14.5157 19.6288 14.6594C19.1474 15.0065 18.497 15.1319 18.176 14.6112C17.8551 14.0905 17.9852 13.3871 18.4666 13.04C19.0529 12.6173 19.8928 12.3749 20.7088 12.2278C21.5627 12.0739 22.5384 12 23.5 12C24.4616 12 25.4373 12.0739 26.2912 12.2278C27.1072 12.3749 27.9471 12.6173 28.5334 13.04C29.0148 13.3871 29.1449 14.0905 28.824 14.6112C28.503 15.1319 27.8526 15.0065 27.3712 14.6594C27.1718 14.5157 26.7022 14.3332 25.9468 14.197C25.2293 14.0677 24.3717 14 23.5 14C22.6283 14 21.7707 14.0677 21.0532 14.197Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    , path
        [ d "M15 20C15 19.4477 14.5523 19 14 19C13.4477 19 13 19.4477 13 20V23C13 23.5523 13.4477 24 14 24C14.5523 24 15 23.5523 15 23V20Z"
        , classes [ Css.c "fill-stormy-500" ]
        ]
    , path
        [ fillRule "evenodd"
        , clipRule "evenodd"
        , d "M11.6981 13.8768C16.5529 8.0609 25.0178 6.37209 31.8314 10.2541C36.4526 12.887 39.4629 17.4697 40.0003 22.3812L40.0018 22.383C40.0126 22.3957 40.0329 22.4187 40.0621 22.4488C40.1213 22.5098 40.212 22.5945 40.3287 22.6789C40.5674 22.8515 40.8656 22.9892 41.2058 22.9892C41.546 22.9892 41.8442 22.8515 42.0828 22.6789C42.1995 22.5945 42.2902 22.5098 42.3495 22.4488C42.3787 22.4187 42.399 22.3957 42.4098 22.383L42.4179 22.3733C42.755 21.9419 43.3775 21.8615 43.8134 22.1949C44.252 22.5305 44.3356 23.1581 44 23.5968L43.7844 23.842C43.6616 23.9685 43.4839 24.1338 43.255 24.2994C42.8023 24.6268 42.1005 24.9892 41.2058 24.9892C40.7865 24.9892 40.4096 24.9096 40.0809 24.7913C39.9769 27.2477 39.3016 29.7167 38.0007 32C37.2259 33.3599 36.0745 34.5744 35 35.6096V39C35 39.2652 34.8946 39.5196 34.7071 39.7071C34.5196 39.8946 34.2652 40 34 40L28 40C27.4477 40 27 39.5523 27 39V37H23L23 39C23 39.5523 22.5523 40 22 40L16 40C15.7348 40 15.4804 39.8946 15.2929 39.7071C15.1053 39.5196 15 39.2652 15 39L15 36.6656C13.566 35.9572 12.2354 34.5857 11.005 33L7 33C6.73478 33 6.48043 32.8946 6.29289 32.7071C6.10536 32.5196 6 32.2652 6 32V24C6 23.7348 6.10536 23.4804 6.29289 23.2929C6.48043 23.1053 6.73478 23 7 23L8.50355 23C8.52228 21.3945 8.63919 19.7667 9.3652 18.0711L7.152 14.53C6.93633 14.1849 6.95127 13.7436 7.18979 13.4139C7.42831 13.0841 7.84283 12.9319 8.23806 13.0287L11.6981 13.8768ZM30.8414 11.9919C24.7111 8.49918 17.0493 10.1625 12.8734 15.6085C12.6319 15.9234 12.2271 16.0657 11.8418 15.9712L10.1525 15.5572L11.348 17.47C11.5326 17.7654 11.5502 18.1356 11.3944 18.4472C10.514 20.208 10.5 21.8638 10.5 24C10.5 24.2652 10.3946 24.5196 10.2071 24.7071C10.0196 24.8946 9.76522 25 9.5 25H8L8 31L11.5 31C11.8148 31 12.1111 31.1482 12.3 31.4C13.7946 33.3928 15.1352 34.6576 16.3162 35.0513C16.7246 35.1874 17 35.5696 17 36L17 38H21L21 36C21 35.4477 21.4477 35 22 35H28C28.2652 35 28.5196 35.1053 28.7071 35.2929C28.8946 35.4804 29 35.7348 29 36V38L33 38V35.1761C33 34.8946 33.1187 34.6261 33.3268 34.4366C34.3835 33.4748 35.5158 32.3213 36.2629 31.0099C40.0773 24.315 37.5363 15.8063 30.8414 11.9919Z"
        , classes [ Css.c "fill-stormy-500" ]
        ]
    , path
        [ d "M43.7844 23.842L43.999 23.598L43.998 23.5994L43.9957 23.6024L43.9901 23.6095L43.9754 23.628C43.9639 23.6422 43.9492 23.6602 43.9312 23.6812C43.8953 23.7233 43.8462 23.7783 43.7844 23.842Z"
        , classes [ Css.c "fill-stormy-500" ]
        ]
    ]

puzzle :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
puzzle attrs =
  svg ([ viewBox "0 0 48 48", role "img" ] <> attrs)
    [ path
        [ d "M41.9899 22.07V16C41.9899 15.45 41.536 15 40.9813 15H36.9164C36.8357 14.62 36.7248 14.15 36.5533 13.66C36.3516 13.1 36.049 12.47 35.5951 11.96C35.111 11.43 34.4352 11 33.5375 11C32.6398 11 31.964 11.43 31.4899 11.96C31.036 12.47 30.7334 13.1 30.5317 13.66C30.3501 14.15 30.2392 14.62 30.1585 15H30.0072H28V17H30.0072H31.0159C31.5403 17 31.9741 16.61 32.0245 16.09C32.0245 16.09 32.0245 16.08 32.0245 16.07C32.0245 16.05 32.0346 16.01 32.0346 15.96C32.0447 15.86 32.0648 15.72 32.0951 15.56C32.1556 15.22 32.2565 14.78 32.4179 14.34C32.5793 13.9 32.7709 13.53 32.9928 13.29C33.1945 13.07 33.366 13 33.5375 13C33.7089 13 33.8804 13.07 34.0821 13.29C34.2939 13.53 34.4957 13.9 34.6571 14.34C34.8184 14.77 34.9193 15.21 34.9798 15.56C35.0101 15.73 35.0303 15.87 35.0403 15.96C35.0403 16.01 35.0504 16.04 35.0504 16.07C35.0504 16.08 35.0504 16.09 35.0504 16.09C35.1009 16.61 35.5346 17 36.0591 17H39.9726V21.23C39.5994 21.31 39.1455 21.42 38.6715 21.59C38.1268 21.79 37.5115 22.08 37.0072 22.52C36.4928 22.98 36.0591 23.63 36.0591 24.5C36.0591 25.38 36.4726 26.06 36.9971 26.54C37.5014 27 38.1268 27.3 38.6816 27.5C39.1556 27.67 39.5994 27.77 39.9726 27.85V31H36.2305C35.6859 31 35.232 31.44 35.2219 31.98C35.2219 31.98 35.2219 31.99 35.2219 32C35.2219 32.02 35.2219 32.06 35.2118 32.1C35.2017 32.19 35.1916 32.33 35.1715 32.5C35.1311 32.84 35.0504 33.27 34.9193 33.69C34.6167 34.62 34.2133 35 33.6988 35C33.1643 35 32.7205 34.59 32.3775 33.66C32.2262 33.24 32.1254 32.8 32.0749 32.47C32.0447 32.3 32.0346 32.17 32.0245 32.07C32.0245 32.02 32.0144 31.99 32.0144 31.97C32.0144 31.96 32.0144 31.95 32.0144 31.95C31.9841 31.41 31.5504 31 31.0159 31H30.0072H28V33H30.0072H30.1383C30.2089 33.39 30.3199 33.86 30.5014 34.34C30.8948 35.41 31.8026 37 33.719 37C35.6556 37 36.513 35.38 36.8559 34.31C37.0072 33.84 37.098 33.38 37.1585 33H40.9914C41.5461 33 42 32.55 42 32V27C42 26.48 41.5965 26.05 41.072 26C41.072 26 41.062 26 41.0519 26C41.0317 26 40.9914 25.99 40.951 25.99C40.8602 25.98 40.719 25.96 40.5576 25.93C40.2248 25.87 39.8012 25.78 39.3775 25.63C38.9539 25.48 38.6009 25.29 38.3689 25.08C38.1571 24.89 38.0764 24.71 38.0764 24.5C38.0764 24.37 38.1268 24.22 38.3487 24.03C38.5807 23.82 38.9438 23.64 39.3775 23.48C39.8012 23.33 40.2349 23.22 40.5677 23.16C40.7291 23.13 40.8703 23.11 40.9611 23.09C41.0115 23.08 41.0418 23.08 41.062 23.08C41.072 23.08 41.0821 23.08 41.0821 23.08C41.6066 23 41.9899 22.58 41.9899 22.07Z"
        , classes [ Css.c "fill-stormy-500" ]
        ]
    , path
        [ d "M25 33H20C19.51 33 19.09 32.64 19.01 32.16C18.86 31.23 18.26 29.5 17.5 29.5C16.64 29.5 16.02 31.34 15.87 32.18C15.78 32.65 15.37 33 14.88 33H10C9.45 33 9 32.55 9 32V26.86C7.56 26.57 5 25.75 5 23.5C5 21.25 7.56 20.43 9 20.14V16C9 15.45 9.45 15 10 15H14.07C14.42 13.59 15.36 11 17.5 11C19.64 11 20.51 13.58 20.83 15H25C25.55 15 26 15.45 26 16V20.14C27.44 20.42 30 21.24 30 23.5C30 25.76 27.44 26.58 26 26.86V32C26 32.55 25.55 33 25 33ZM20.8 31H24V26C24 25.48 24.4 25.05 24.91 25C25.99 24.9 28 24.36 28 23.5C28 22.64 25.99 22.1 24.91 22C24.4 21.95 24 21.52 24 21V17H20C19.5 17 19.07 16.63 19.01 16.13C18.84 14.82 18.18 13 17.5 13C16.79 13 16.07 14.84 15.87 16.15C15.8 16.64 15.38 17 14.88 17H11V21C11 21.51 10.61 21.95 10.1 22C9.02 22.1 7 22.65 7 23.5C7 24.35 9.02 24.9 10.1 25C10.61 25.05 11 25.49 11 26V31H14.09C14.46 29.73 15.41 27.5 17.5 27.5C19.56 27.5 20.46 29.72 20.8 31Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    ]

settings :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
settings attrs =
  svg ([ viewBox "0 0 48 48", role "img" ] <> attrs)
    [ path
        [ fillRule "evenodd"
        , clipRule "evenodd"
        , d "M32.2039 13.8311L31.8915 14.0262C29.4305 15.5628 26.229 14.1786 25.5744 11.313C25.1858 9.61205 22.8142 9.61205 22.4256 11.313C21.771 14.1786 18.5695 15.5628 16.1085 14.0262L15.7961 13.8311C14.4478 12.9892 12.8608 14.6056 13.7565 16.0081C15.3508 18.5045 14.0151 21.8727 11.161 22.5101C9.59875 22.8591 9.59875 25.1409 11.161 25.4899C14.0151 26.1273 15.3508 29.4955 13.7565 31.9919C12.8608 33.3944 14.4478 35.0108 15.7961 34.1689L16.1085 33.9738C18.5695 32.4372 21.771 33.8214 22.4256 36.687C22.8142 38.388 25.1858 38.388 25.5744 36.687C26.229 33.8214 29.4305 32.4372 31.8915 33.9738L32.2039 34.1689C33.5522 35.0108 35.1392 33.3944 34.2435 31.9919C32.6492 29.4955 33.9849 26.1273 36.839 25.4899C38.4012 25.1409 38.4012 22.8591 36.839 22.5101C33.9849 21.8727 32.6492 18.5045 34.2435 16.0081C35.1392 14.6056 33.5522 12.9892 32.2039 13.8311ZM27.5114 10.8489C26.6436 7.05036 21.3564 7.05036 20.4886 10.8489C20.1415 12.3682 18.445 13.099 17.1441 12.2867L16.8317 12.0916C13.6485 10.1041 10.051 13.9279 12.0916 17.1232C12.9253 18.4287 12.2253 20.1872 10.7371 20.5196C7.08762 21.3348 7.08762 26.6652 10.7371 27.4804C12.2253 27.8128 12.9253 29.5713 12.0916 30.8768C10.051 34.0721 13.6485 37.8959 16.8317 35.9084L17.1441 35.7133C18.445 34.901 20.1415 35.6318 20.4886 37.1511C21.3564 40.9496 26.6436 40.9496 27.5114 37.1511C27.8585 35.6318 29.555 34.901 30.8559 35.7133L31.1683 35.9084C34.3515 37.8959 37.949 34.0721 35.9084 30.8768C35.0747 29.5713 35.7747 27.8128 37.2629 27.4804C40.9124 26.6652 40.9124 21.3348 37.2629 20.5196C35.7747 20.1872 35.0747 18.4287 35.9084 17.1232C37.949 13.9279 34.3515 10.1041 31.1683 12.0916L30.8559 12.2867C29.555 13.099 27.8585 12.3682 27.5114 10.8489Z"
        , classes [ Css.c "fill-stormy-500" ]
        ]
    , path
        [ fillRule "evenodd"
        , clipRule "evenodd"
        , d "M24 21C22.3431 21 21 22.3431 21 24C21 25.6569 22.3431 27 24 27C25.6569 27 27 25.6569 27 24C27 22.3431 25.6569 21 24 21ZM19 24C19 21.2386 21.2386 19 24 19C26.7614 19 29 21.2386 29 24C29 26.7614 26.7614 29 24 29C21.2386 29 19 26.7614 19 24Z"
        , classes [ Css.c "fill-tropical-500" ]
        ]
    ]

sinchLogo :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
sinchLogo attrs =
  svg
    ([ viewBox "0 0 200 130.31", role "img" ] <> attrs)
    [ path [ d "M199.13,66.25a36.2,36.2,0,0,1-23.46,34.22c-12.17,4.75-26.43,3-40.18-4.77l-10-5.76a35.44,35.44,0,0,1-7.68,6.61l-32,18.72-.06,0V99.12l.06,0L133.9,70.93a35.17,35.17,0,0,1-1.54,6.91l10,5.72c12.84,7.3,22.42,6.15,28.19,3.9a22.78,22.78,0,0,0,13.47-14,23.23,23.23,0,0,0,1.1-7.14,22.24,22.24,0,0,0-14.82-20.94c-5.81-2.19-15.39-3.23-28.14,4.23L64.08,95.32l-.09.06c-8.66,5.07-17.56,7.68-26,7.73a39.15,39.15,0,0,1-14.09-2.47A36.48,36.48,0,0,1,1.75,55.24,36.88,36.88,0,0,1,23.46,32.48c12.17-4.75,26.43-3.06,40.18,4.77l10,5.76a35.26,35.26,0,0,1,7.69-6.61L82,36l3.73-2.18-15.9-9A7,7,0,0,1,76.7,12.67L99.55,25.74l22.39-13A7,7,0,0,1,129,24.77L65.29,61.55a35.26,35.26,0,0,1,1.48-6.44l-10-5.72c-12.83-7.3-22.4-6.15-28.18-3.89a22.38,22.38,0,0,0,.25,42.06c5.8,2.19,15.39,3.23,28.14-4.23l78.21-45.77c8.66-5.07,17.56-7.67,26-7.72a39.14,39.14,0,0,1,14.08,2.47,36.22,36.22,0,0,1,23.87,33.94" ]
    ]

upload :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
upload attrs =
  svg
    ([ viewBox "0 0 25 16" ] <> attrs)
    [ path [ d "M19.725 6.04C19.045 2.59 16.015 0 12.375 0C9.485 0 6.975 1.64 5.725 4.04C2.715 4.36 0.375 6.91 0.375 10C0.375 13.31 3.065 16 6.375 16H19.375C22.135 16 24.375 13.76 24.375 11C24.375 8.36 22.325 6.22 19.725 6.04ZM14.375 9V13H10.375V9H7.375L12.025 4.35C12.225 4.15 12.535 4.15 12.735 4.35L17.375 9H14.375Z" ] ]

user :: forall w i. Array (HH.IProp SVGsvg i) -> HH.HTML w i
user attrs =
  svg
    ([ viewBox "0 0 48 48", role "img" ] <> attrs)
    [ path
        [ classes [ Css.c "fill-tropical-500" ]
        , fillRule "evenodd"
        , clipRule "evenodd"
        , d "M24 13a4 4 0 1 0 0 8 4 4 0 0 0 0-8Zm-6 4a6 6 0 1 1 12 0 6 6 0 0 1-12 0Z"
        ]
    , path
        [ classes [ Css.c "fill-stormy-500" ]
        , fillRule "evenodd"
        , clipRule "evenodd"
        , d "M17.724 31.24C16.111 32.705 15 34.929 15 38a1 1 0 1 1-2 0c0-3.596 1.322-6.371 3.378-8.24 2.04-1.854 4.74-2.76 7.425-2.76 5.385 0 10.909 3.675 11.196 10.96a1 1 0 1 1-1.998.08c-.239-6.05-4.715-9.04-9.198-9.04-2.25 0-4.451.76-6.08 2.24Z"
        ]
    ]

type CoreAttributes r
  = ( id :: String, "class" :: String, style :: String | r )

type SVGsvg
  = CoreAttributes
      ( ariaHidden :: String
      , ariaLabel :: String
      , role :: String
      , viewBox :: String
      )

type SVGpath
  = CoreAttributes
      ( d :: String
      , clipRule :: String
      , fillRule :: String
      )

svgElement ::
  forall r w i.
  HH.ElemName ->
  Array (HH.IProp r i) ->
  Array (HH.HTML w i) ->
  HH.HTML w i
svgElement = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg")

svg :: forall w i. HH.Node SVGsvg w i
svg = svgElement (HH.ElemName "svg")

path :: forall w i. HH.Leaf SVGpath w i
path props = svgElement (HH.ElemName "path") props []

ariaHidden :: forall r i. Boolean -> HH.IProp ( ariaHidden :: String | r ) i
ariaHidden b = HH.attr (HH.AttrName "aria-hidden") (if b then "true" else "false")

ariaLabel :: forall r i. String -> HH.IProp ( ariaLabel :: String | r ) i
ariaLabel = HH.attr (HH.AttrName "aria-label")

clipRule :: forall r i. String -> HH.IProp ( clipRule :: String | r ) i
clipRule = HH.attr (HH.AttrName "clip-rule")

id :: forall r i. String -> HH.IProp ( id :: String | r ) i
id = HH.attr (HH.AttrName "id")

fillRule :: forall r i. String -> HH.IProp ( fillRule :: String | r ) i
fillRule = HH.attr (HH.AttrName "fill-rule")

role :: forall r i. String -> HH.IProp ( role :: String | r ) i
role = HH.attr (HH.AttrName "role")

viewBox :: forall r i. String -> HH.IProp ( viewBox :: String | r ) i
viewBox = HH.attr (HH.AttrName "viewBox")

d :: forall r i. String -> HH.IProp ( d :: String | r ) i
d = HH.attr (HH.AttrName "d")

classes :: forall r i. Array HH.ClassName -> HH.IProp ( "class" :: String | r ) i
classes cs = HH.attr (HH.AttrName "class") (joinWith " " (coerce cs))
