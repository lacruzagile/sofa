-- | A component providing rendering of Nectary style input fields and text areas.
module Sofa.Component.InputField
  ( InputCommon
  -- * Single line input
  , Input
  , defaultInput
  , render
  -- * Textarea
  , InputTextarea
  , defaultTextarea
  , renderTextarea
  ) where

import Prelude
import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Core (Prop(..))
import Halogen.HTML.Properties as HP
import Sofa.Component.Icon as Icon
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Unsafe.Coerce (unsafeCoerce)

type InputCommon el i
  = { props :: Array (HP.IProp el i)
    , label :: String
    , optionalText :: Maybe String
    , errorText :: Maybe String
    , additionalText :: Maybe String
    , tooltipText :: Maybe String
    , wrapperClasses :: Array HH.ClassName
    }

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
-- |
-- | - `wrapperClasses` – additional classes to add to the wrapper element
type Input i
  = InputCommon HTMLinput i

defaultInput :: forall i. Input i
defaultInput =
  { props: []
  , label: ""
  , errorText: Nothing
  , optionalText: Nothing
  , additionalText: Nothing
  , tooltipText: Nothing
  , wrapperClasses: []
  }

type InputTextarea i
  = InputCommon HTMLtextarea i

defaultTextarea :: forall i. InputTextarea i
defaultTextarea =
  { props: []
  , label: ""
  , errorText: Nothing
  , optionalText: Nothing
  , additionalText: Nothing
  , tooltipText: Nothing
  , wrapperClasses: []
  }

renderCommon :: forall w el i. InputCommon el i -> HH.HTML w i -> HH.HTML w i
renderCommon input inputElement =
  HH.div [ HP.classes $ [ Css.c "w-full" ] <> input.wrapperClasses ]
    [ HH.label_
        [ renderTop
        , inputElement
        ]
    , renderBottom
    ]
  where
  disabled = fromMaybe false $ A.findMap extractDisabledProp input.props

  extractDisabledProp = case _ of
    HP.IProp (Property "disabled" p) -> Just $ unsafeCoerce p
    _ -> Nothing

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
      Nothing -> HH.div [ Css.class_ "h-2" ] []
      Just txt ->
        HH.div
          [ Css.classes [ "leading-5", "text-xs", "text-right", subduedColor ] ]
          [ HH.text txt ]

-- | Render a Nectary styled input field.
render :: forall w i. Input i -> HH.HTML w i
render input =
  renderCommon input
    $ HH.input ([ Css.classes [ "nectary-input", "w-full" ] ] <> input.props)

-- | Render a Nectary styled text are.
renderTextarea :: forall w i. InputTextarea i -> HH.HTML w i
renderTextarea input =
  renderCommon input
    $ HH.textarea ([ Css.classes [ "nectary-textarea", "w-full" ] ] <> input.props)
