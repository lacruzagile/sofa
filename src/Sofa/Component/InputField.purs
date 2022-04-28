-- | A component providing rendering of Nectary style input fields.
module Sofa.Component.InputField
  ( defaultInput
  , render
  ) where

import Prelude
import DOM.HTML.Indexed (HTMLinput)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..))
import Halogen.HTML.Properties as HP
import Sofa.Component.Icon as Icon
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Unsafe.Coerce (unsafeCoerce)

-- | Input parameters for input field rendering.
-- |
-- | - `props` – properties to apply on the input element
-- |
-- | - `label` – the input label
-- |
-- | - `optionalText` – optional descriptive text
-- |
-- | - `errorText` – optional error text
-- |
-- | - `additionalText` – optional additional text, shown only when no error is
-- |     given
-- |
-- | - `tooltipText` – optional tooltip text to show when label is hovered
type Input i
  = { props :: Array (HP.IProp HTMLinput i)
    , label :: String
    , optionalText :: Maybe String
    , errorText :: Maybe String
    , additionalText :: Maybe String
    , tooltipText :: Maybe String
    }

defaultInput :: forall i. Input i
defaultInput =
  { props: []
  , label: ""
  , errorText: Nothing
  , optionalText: Nothing
  , additionalText: Nothing
  , tooltipText: Nothing
  }

-- | Render a Nectary styled input field.
render :: forall w i. Input i -> HH.HTML w i
render input =
  HH.div [ Css.class_ "w-full" ]
    [ HH.label_
        [ renderTop
        , HH.input ([ Css.classes [ "nectary-input", "w-full" ] ] <> input.props)
        ]
    , renderBottom
    ]
  where
  extractDisabledProp = case _ of
    HP.IProp (Property "disabled" p) -> Just $ unsafeCoerce p
    _ -> Nothing

  disabled =
    fromMaybe false
      $ A.findMap extractDisabledProp
      $ input.props

  subduedColor
    | disabled = "text-stormy-100"
    | otherwise = "text-stormy-300"

  renderLabelContent :: HH.HTML w i
  renderLabelContent = case input.tooltipText of
    Nothing -> HH.text input.label
    Just text
      | disabled ->
        HH.div
          [ Css.classes [ "text-stormy-100", "fill-stormy-100" ] ]
          [ Icon.textWithTooltip input.label ]
      | otherwise ->
        Tooltip.render
          ( Tooltip.defaultInput
              { text = text
              , width = Just "20rem"
              }
          )
          (Icon.textWithTooltip input.label)

  renderTop =
    HH.div [ Css.classes [ "flex", "items-baseline" ] ]
      [ HH.span
          [ Css.classes [ "grow", "font-semibold" ] ]
          [ renderLabelContent ]
      , case input.optionalText of
          Nothing -> HH.text ""
          Just txt ->
            HH.span
              [ Css.classes [ "text-sm", subduedColor ] ]
              [ HH.text txt ]
      ]

  renderBottom = case input.errorText of
    Just txt
      | not disabled ->
        HH.div
          [ Css.classes [ "leading-5", "text-xs", "text-raspberry-500" ] ]
          [ HH.text txt ]
    _ -> case input.additionalText of
      Nothing -> HH.div [ Css.class_ "h-5" ] []
      Just txt ->
        HH.div
          [ Css.classes [ "leading-5", "text-xs", "text-right", subduedColor ] ]
          [ HH.text txt ]
