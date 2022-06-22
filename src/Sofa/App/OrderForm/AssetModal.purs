-- | The asset modal component of the order line.
module Sofa.App.OrderForm.AssetModal (Slot, Input(..), component) where

import Prelude
import Data.Array as A
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Sofa.Component.Icon as Icon
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.MouseEvent (MouseEvent, toEvent) as Event

data Action
  = OpenModal Event.MouseEvent
  | CloseModal Event.MouseEvent

type Slot id
  = forall query output.
    H.Slot query output id

type Input
  = { statusReason :: String }

type State
  = { statusReason :: Map String String, open :: Boolean }

initialState :: Input -> State
initialState input =
  { statusReason: parseStatusReason input.statusReason
  , open: false
  }

parseStatusReason :: String -> Map String String
parseStatusReason s =
  let
    entries = S.split (S.Pattern ",") s

    keyValue = map (\entry -> makeTuple (S.split (S.Pattern ":") entry)) entries

    makeTuple = case _ of
      [ k, v ] -> Tuple (S.trim k) (S.trim v)
      value -> Tuple "Raw Value" $ show value
  in
    Map.fromFoldable keyValue

component ::
  forall query output f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction }
    }

render ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  State -> H.ComponentHTML Action () m
render state
  | state.open = renderModal state.statusReason
  | otherwise = renderIcon

renderModal ::
  forall m.
  Map String String -> H.ComponentHTML Action () m
renderModal statusReason =
  HH.div_
    [ renderIcon
    , Modal.render
        $ Modal.defaultInput
            { title = HH.text "Asset Information"
            , closeAction = Just CloseModal
            , backgroundClickAction = Just CloseModal
            , content = renderContent statusReason
            }
    ]

renderIcon ::
  forall m.
  H.ComponentHTML Action () m
renderIcon =
  HH.button
    [ Css.classes [ "p-2", "fill-snow-700", "hover:fill-snow-900" ]
    , HE.onClick OpenModal
    ]
    [ Icon.info
        [ Icon.classes [ Css.c "w-5" ]
        , Icon.ariaLabel "Asset"
        ]
    ]

renderContent ::
  forall m.
  Map String String -> H.ComponentHTML Action () m
renderContent statusReason =
  HH.div [ Css.classes [ "flex", "flex-col", "gap-y-4", "text-left" ] ]
    [ HH.div
        [ Css.classes
            [ "w-full"
            , "min-w-96"
            , "p-8"
            , "grid"
            , "grid-cols-[12rem_auto]"
            , "gap-4"
            , "rounded"
            , "bg-snow-500"
            ]
        ]
        $ renderEntries statusReason
    , HH.div [ Css.classes [ "flex", "space-x-5" ] ] closeButton
    ]

renderEntries :: forall m. Map String String -> Array (H.ComponentHTML Action () m)
renderEntries entries = A.concatMap renderEntry $ Map.toUnfoldable entries

renderEntry :: forall m. Tuple String String -> Array (H.ComponentHTML Action () m)
renderEntry (Tuple k v) =
  [ HH.h4_ [ HH.text k ]
  , HH.div_
      [ HH.text v
      --   , Icon.upload
      --       [ Icon.classes [ Css.c "w-5", Css.c "sofa-icon-inline" ]
      --       , Icon.ariaLabel "copy"
      --       ]
      ]
  ]

closeButton ∷ forall w. Array (HH.HTML w Action)
closeButton =
  [ HH.div [ Css.class_ "grow" ] []
  , HH.button
      [ Css.class_ "nectary-btn-secondary"
      , HE.onClick CloseModal
      ]
      [ HH.text "Close" ]
  ]

handleAction ::
  forall output m.
  MonadAff m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  OpenModal event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    H.modify_ $ \st -> st { open = true }
  CloseModal event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    H.modify_ $ \st -> st { open = false }
