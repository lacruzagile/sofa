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
import Halogen.HTML.Properties as HHP
import Halogen.HTML.Events as HE
import Sofa.App.Requests (getAsset)
import Sofa.Component.Icon as Icon
import Sofa.Component.Modal as Modal
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (copyToClipboard)
import Web.Event.Event (stopPropagation) as Event
import Web.UIEvent.MouseEvent (MouseEvent, toEvent) as Event

data Action
  = OpenModal Event.MouseEvent
  | CloseModal Event.MouseEvent
  | Copy String Event.MouseEvent
  | StopPropagation Event.MouseEvent

type Slot id
  = forall query output.
    H.Slot query output id

type Input
  = { statusReason :: String, orderId :: SS.OrderId }

type State
  = { statusReason :: Map String String, open :: Boolean, orderId :: SS.OrderId, assets :: Loadable (Array SS.AssetConfig) }

initialState :: Input -> State
initialState input =
  { statusReason: parseStatusReason input.statusReason
  , orderId: input.orderId
  , open: false
  , assets: Loading
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
  | state.open = renderModal state
  | otherwise = renderIcon

renderModal ::
  forall m.
  State -> H.ComponentHTML Action () m
renderModal state =
  HH.div_
    [ renderIcon
    , Modal.render
        $ Modal.defaultInput
            { title = HH.text "Asset Information"
            , backgroundClickAction = Just StopPropagation
            , content = renderContent state
            }
    ]

renderIcon ::
  forall m.
  H.ComponentHTML Action () m
renderIcon =
  HH.button
    [ Css.classes [ "p-2", "fill-informative-700" ]
    , HE.onClick OpenModal
    ]
    [ Icon.info
        [ Icon.classes [ Css.c "w-5" ]
        , Icon.ariaLabel "Asset"
        ]
    ]

renderContent ::
  forall m.
  State -> H.ComponentHTML Action () m
renderContent state =
  HH.div [ Css.classes [ "flex", "flex-col", "gap-y-4", "text-left" ] ]
    [ HH.div
        [ Css.classes
            [ "w-full"
            , "min-w-96"
            , "p-8"
            , "flex"
            , "flex-col"
            , "gap-4"
            , "max-h-128"
            , "overflow-auto"
            ]
        ]
        $ renderAssets state.assets
    , HH.div [ Css.classes [ "flex", "space-x-5" ] ] closeButton
    ]

renderAssets :: forall m. Loadable (Array SS.AssetConfig) -> Array (H.ComponentHTML Action () m)
renderAssets = case _ of
  Idle -> []
  Loaded assets -> map renderAssetConfig (assets)
  Loading -> [ HH.text "Loading..." ]
  Error message -> [ HH.text message ]

renderAssetConfig :: forall m. SS.AssetConfig -> H.ComponentHTML Action () m
renderAssetConfig (SS.AssetConfig { assetConfig }) =
  HH.div
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
    $ A.concatMap renderEntry (Map.toUnfoldable assetConfig)

renderEntry :: forall m. Tuple String SS.ConfigValue -> Array (H.ComponentHTML Action () m)
renderEntry (Tuple k v) =
  [ HH.h4_ [ HH.text k ]
  , HH.div_
      [ HH.text value
      , HH.button
          [ Css.classes [ "p-2", "fill-stormy-500", "sofa-icon-float-right" ]
          , HE.onClick (Copy value)
          ]
          [ Icon.copy
              [ Icon.classes [ Css.c "w-5"]
              , Icon.ariaLabel "Copy"
              ]
          ]
      ]
  ]
  where
  value = (show v)

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
  forall output f m.
  MonadAff m =>
  CredentialStore f m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  OpenModal event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    { orderId } <- H.modify $ \st -> st { assets = Loading, open = true }
    -- Fetch the assets.
    assets <- H.lift $ getAsset orderId
    H.modify_ _ { assets = assets }
  CloseModal event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    H.modify_ $ \st -> st { open = false }
  Copy value event -> do
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
    H.liftEffect $ copyToClipboard value
  StopPropagation event -> do
    -- Don't propagate the click to the underlying table row.
    H.liftEffect $ Event.stopPropagation $ Event.toEvent event
