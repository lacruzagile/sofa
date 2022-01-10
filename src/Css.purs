module Css where

import Halogen (ClassName(..))

-- SOFA layer classes.
btnRed100 :: ClassName
btnRed100 = ClassName "sofa-btn-red-100"

btnSky100 :: ClassName
btnSky100 = ClassName "sofa-btn-sky-100"

smallTitle :: ClassName
smallTitle = ClassName "sofa-small-title"

spinner :: ClassName
spinner = ClassName "sofa-spinner"

-- | Tailwind CSS classes.
tw :: _
tw =
  { absolute: ClassName "absolute"
  , activeBgSky300: ClassName "active:bg-sky-300"
  , alignMiddle: ClassName "align-middle"
  , alignTextBottom: ClassName "align-text-bottom"
  , animatePulse: ClassName "animate-pulse"
  , appearanceNone: ClassName "appearance-none"
  , bgBlack_60: ClassName "bg-black/60"
  , bgBlue600: ClassName "bg-blue-600"
  , bgGray100: ClassName "bg-gray-100"
  , bgGray200: ClassName "bg-gray-200"
  , bgRed100: ClassName "bg-red-100"
  , bgSky100: ClassName "bg-sky-100"
  , bgSky200: ClassName "bg-sky-200"
  , bgTransparent: ClassName "bg-transparent"
  , bgWhite: ClassName "bg-white"
  , block: ClassName "block"
  , border: ClassName "border"
  , borderB: ClassName "border-b"
  , borderGray100: ClassName "border-gray-100"
  , borderL8: ClassName "border-l-8"
  , borderNone: ClassName "border-none"
  , borderRed400: ClassName "border-red-400"
  , borderSky300: ClassName "border-sky-300"
  , borderT: ClassName "border-t"
  , cursorPointer: ClassName "cursor-pointer"
  , decoration2: ClassName "decoration-2"
  , decorationSky300: ClassName "decoration-sky-300"
  , decorationSky500: ClassName "decoration-sky-500"
  , decorationSky500_30: ClassName "decoration-sky-500/30"
  , fixed: ClassName "fixed"
  , flex: ClassName "flex"
  , flexCol: ClassName "flex-col"
  , flexWrap: ClassName "flex-wrap"
  , flexWrapReverse: ClassName "flex-wrap-reverse"
  , floatRight: ClassName "float-right"
  , focusOutline: ClassName "focus-outline"
  , fontNormal: ClassName "font-normal"
  , grow: ClassName "grow"
  , hFull: ClassName "h-full"
  , hoverBgGray100: ClassName "hover:bg-gray-100"
  , hoverBgSky200: ClassName "hover:bg-sky-200"
  , hoverCursorPointer: ClassName "hover:cursor-pointer"
  , hover_underline: ClassName "hover:underline"
  , inline: ClassName "inline"
  , inset0: ClassName "inset-0"
  , itemsCenter: ClassName "items-center"
  , justifyBetween: ClassName "justify-between"
  , m5: ClassName "m-5"
  , mAuto: ClassName "m-auto"
  , maxH48: ClassName "max-h-48"
  , maxH72: ClassName "max-h-72"
  , maxH96: ClassName "max-h-96"
  , mb2: ClassName "mb-2"
  , mb3: ClassName "mb-3"
  , minW128: ClassName "min-w-128"
  , minW96: ClassName "min-w-96"
  , minusM5: ClassName "-m-5"
  , ml2: ClassName "ml-2"
  , mr10: ClassName "mr-10"
  , mr5: ClassName "mr-5"
  , mt1: ClassName "mt-1"
  , mt5: ClassName "mt-5"
  , mx5: ClassName "mx-5"
  , mxAuto: ClassName "mx-auto"
  , my2: ClassName "my-2"
  , my5: ClassName "my-5"
  , myAuto: ClassName "my-auto"
  , outline1: ClassName "outline-1"
  , outlineGray300: ClassName "outline-gray-300"
  , outlineNone: ClassName "outline-none"
  , overflowAuto: ClassName "overflow-auto"
  , overflowHidden: ClassName "overflow-hidden"
  , overflowYAuto: ClassName "overflow-y-auto"
  , p2: ClassName "p-2"
  , p3: ClassName "p-3"
  , p5: ClassName "p-5"
  , placeholderItalic: ClassName "placeholder:italic"
  , placeholderTextGray400: ClassName "placeholder:text-gray-400"
  , pt5: ClassName "pt-5"
  , px3: ClassName "px-3"
  , px5: ClassName "px-5"
  , py0: ClassName "py-0"
  , py2: ClassName "py-2"
  , py3: ClassName "py-3"
  , relative: ClassName "relative"
  , right0: ClassName "right-0"
  , roundedMd: ClassName "rounded-md"
  , roundedSm: ClassName "rounded-sm"
  , shadowMd: ClassName "shadow-md"
  , shadowSm: ClassName "shadow-sm"
  , smallCaps: ClassName "small-caps"
  , spaceX5: ClassName "space-x-5"
  , spaceY4: ClassName "space-y-4"
  , table: ClassName "table"
  , tableAuto: ClassName "table-auto"
  , tableCell: ClassName "table-cell"
  , tableHeaderGroup: ClassName "table-header-group"
  , tableRow: ClassName "table-row"
  , tableRowGroup: ClassName "table-row-group"
  , text2Xl: ClassName "text-2xl"
  , textCenter: ClassName "text-center"
  , textEllipsis: ClassName "text-ellipsis"
  , textGray400: ClassName "text-gray-400"
  , textGray600: ClassName "text-gray-600"
  , textGray700: ClassName "text-gray-700"
  , textLeft: ClassName "text-left"
  , textLg: ClassName "text-lg"
  , textRed700: ClassName "text-red-700"
  , textRight: ClassName "text-right"
  , textSm: ClassName "text-sm"
  , textWhite: ClassName "text-white"
  , truncate: ClassName "truncate"
  , underline: ClassName "underline"
  , underlineOffset4: ClassName "underline-offset-4"
  , underlineOffset8: ClassName "underline-offset-8"
  , uppercase: ClassName "uppercase"
  , w1_2: ClassName "w-1/2"
  , w1_3: ClassName "w-1/3"
  , w1_5: ClassName "w-1/5"
  , w2_3: ClassName "w-2/3"
  , w3_5: ClassName "w-3/5"
  , w72: ClassName "w-72"
  , w96: ClassName "w-96"
  , wFull: ClassName "w-full"
  , z10: ClassName "z-10"
  }
