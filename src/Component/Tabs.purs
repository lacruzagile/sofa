-- | A component providing Nectary style tabs.
module Component.Tabs (Slot, Output, proxy, component) where

import Prelude
import Css as Css
import Data.Array as A
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "nectaryTabs"
proxy = Proxy

type TabButton
  = { disabled :: Boolean -- ^ Whether the button is disabled.
    , content :: HH.PlainHTML -- ^ The button content.
    }

type Input
  = { selected :: Int
    -- ^ Index of currently selected tab.
    , tabs :: Array TabButton
    -- ^ The content of the tabs.
    }

-- | The currently selected tab.
type Output
  = Int

type State
  = { selected :: Int
    , tabs :: Array TabButton
    }

data Action
  = Select Int

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
initialState input = input

render ::
  forall slots m.
  MonadAff m =>
  State -> H.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.div [ tabsWrapperClasses ] $ A.mapWithIndex renderTab state.tabs
    ]
  where
  tabsWrapperClasses =
    HP.classes
      $ Css.c
      <$> [ "flex"
        , "space-x-3"
        , "w-full"
        , "mb-3"
        , "border-b"
        , "border-stormy-100"
        , "overflow-x-auto"
        ]

  tabWrapperClasses =
    HP.classes $ Css.c
      <$> [ "flex"
        , "relative"
        , "h-11"
        , "px-5"
        , "py-2"
        ]

  contentClasses =
    HP.classes $ Css.c
      <$> [ "mx-auto"
        , "my-auto"
        , "shrink"
        , "overflow-hidden"
        , "whitespace-nowrap"
        , "text-ellipsis"
        , "peer-checked:font-semibold"
        ]

  inputClasses =
    HP.classes $ Css.c
      <$> [ "peer"
        , "appearance-none"
        , "absolute"
        , "inset-0"
        , "cursor-pointer"
        , "checked:border-b-2"
        , "checked:border-stormy-500"
        ]

  renderTab idx c =
    HH.div
      [ tabWrapperClasses
      ]
      [ HH.input
          [ HP.type_ HP.InputRadio
          , inputClasses
          , HP.checked $ idx == state.selected
          , HE.onChange \_ -> Select idx
          ]
      , HH.div [ contentClasses ] [ HH.fromPlainHTML c.content ]
      ]

handleAction ::
  forall slots m.
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Select idx -> do
    H.modify_ \st -> st { selected = idx }
    H.raise idx
