-- | The configuration schema component of the order form.
module Sofa.App.OrderForm.ConfigSchema
  ( Input(..)
  , Output(..)
  , Slot
  , component
  , proxy
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Array as A
import Data.Int as Int
import Data.List as SList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.App.OrderForm.Widget.AssetConfigLink (SkuConfigs)
import Sofa.App.OrderForm.Widget.AssetConfigLink as WAssetConfigLink
import Sofa.App.OrderForm.Widget.Checkbox as WCheckbox
import Sofa.App.OrderForm.Widget.Dropdown as WDropdown
import Sofa.App.OrderForm.Widget.FileAttachment as WFileAttachment
import Sofa.App.OrderForm.Widget.Radio as WRadio
import Sofa.App.OrderForm.Widget.Textarea as WTextarea
import Sofa.App.OrderForm.Widget.Typeahead as WTypeahead
import Sofa.App.SchemaDataSource (DataSourceEnumResult, DataSourceVars, getDataSourceEnum)
import Sofa.Component.Alerts (class MonadAlert)
import Sofa.Component.Icon as Icon
import Sofa.Component.InputField as InputField
import Sofa.Component.Select as Select
import Sofa.Component.Tabs as Tabs
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Schema (mkDefaultConfig)
import Sofa.Data.Schema as Schema
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))
import Web.Event.Event as WebEvent
import Web.HTML.HTMLInputElement as HtmlInputElement
import Web.HTML.ValidityState as Validity

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "configSchema"
proxy = Proxy

type Slots
  = ( selectEnum :: Select.Slot ConfigEntryIndex Int -- Output is selected value index.
    , widgetAssetConfigLink :: WAssetConfigLink.Slot ConfigEntryIndex
    , widgetCheckbox :: WCheckbox.Slot ConfigEntryIndex
    , widgetDropdown :: WDropdown.Slot ConfigEntryIndex
    , widgetFileAttachment :: WFileAttachment.Slot ConfigEntryIndex
    , widgetRadio :: WRadio.Slot ConfigEntryIndex
    , widgetTextarea :: WTextarea.Slot ConfigEntryIndex
    , widgetTypeahead :: WTypeahead.Slot ConfigEntryIndex
    , nectaryTabs :: Tabs.Slot ConfigEntryIndex
    )

type Input
  = { orderLineId :: Maybe SS.OrderLineId
    , configValue :: SS.ConfigValue
    , schemaEntry :: SS.ConfigSchemaEntry
    , readOnly :: Boolean
    , dataSourceVars :: DataSourceVars
    , getConfigs :: Unit -> Array SkuConfigs
    }

type Output
  = Maybe SS.ConfigValue

type State
  = { orderLineId :: Maybe SS.OrderLineId
    , configValue :: SS.ConfigValue
    , schemaEntry :: SS.ConfigSchemaEntry
    , readOnly :: Boolean
    , errors :: Map ConfigEntryIndex String
    -- ^ Whether there is an invalid input. The label points to the erroneous
    -- configuration field.
    , configTabs :: Map ConfigEntryIndex Int
    -- ^ The currently selected tab of a `oneOf` configuration entry.
    , dataSourceVars :: DataSourceVars
    , getConfigs :: Unit -> Array SkuConfigs
    }

type ConfigEntryIndex
  = { entryIndex :: SList.List Int -- ^ Index within array or object.
    }

data Action
  = NoOp
  | Receive Input
  | SetConfigTab ConfigEntryIndex Int
  | CheckInput ConfigEntryIndex WebEvent.Event
  | UpdateValue (Maybe SS.ConfigValue -> SS.ConfigValue)

component ::
  forall query f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
    }

initialState :: Input -> State
initialState input =
  { orderLineId: input.orderLineId
  , configValue: input.configValue
  , schemaEntry: input.schemaEntry
  , readOnly: input.readOnly
  , errors: Map.empty
  , configTabs: Map.empty
  , dataSourceVars: input.dataSourceVars
  , getConfigs: input.getConfigs
  }

render ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  MonadAlert m =>
  State -> H.ComponentHTML Action Slots m
render state@{ orderLineId } =
  renderEntry
    rootEntryIdx
    UpdateValue
    ""
    (Just state.configValue)
    state.schemaEntry
  where
  rootEntryIdx = { entryIndex: SList.Nil }

  mact :: forall a. (a -> Action) -> Maybe a -> Action
  mact = maybe NoOp

  opt :: forall a b. (a -> b) -> Maybe a -> Array b
  opt f = maybe [] (\x -> [ f x ])

  renderEntry ::
    ConfigEntryIndex ->
    ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
    String ->
    Maybe SS.ConfigValue ->
    SS.ConfigSchemaEntry ->
    H.ComponentHTML Action Slots m
  renderEntry entryIdx act fallbackTitle value schemaEntry = case schemaEntry of
    SS.CseBoolean _ ->
      let
        checked = case value of
          Just (SS.CvBoolean b) -> b
          _ -> false
      in
        renderCheckbox fallbackTitle schemaEntry
          [ HP.checked checked
          , HE.onChecked (act <<< const <<< SS.CvBoolean)
          ]
    SS.CseInteger { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
    SS.CseInteger c
      | not (A.null c.enum) ->
        renderEnumEntry
          entryIdx
          act
          fallbackTitle
          value
          schemaEntry
          c
          SS.CvInteger
          show
    SS.CseInteger c ->
      InputField.render
        $ InputField.defaultInput
            { props =
              [ Css.classes
                  $ if state.readOnly then
                      readOnlyInputClasses
                    else
                      [ "nectary-input", "nectary-input-number", "w-full" ]
              , HP.type_ HP.InputNumber
              , HP.placeholder "Integer"
              , HP.disabled state.readOnly
              , HE.onInput $ CheckInput entryIdx
              , HE.onValueChange (mact (act <<< const <<< SS.CvInteger) <<< Int.fromString)
              ]
                <> opt (HP.value <<< show) value
                <> opt (HP.min <<< Int.toNumber) c.minimum
                <> opt (HP.max <<< Int.toNumber) c.maximum
            , label = fromMaybe fallbackTitle $ SS.configSchemaEntryTitle schemaEntry
            , errorText = Map.lookup entryIdx state.errors
            , tooltipText = SS.configSchemaEntryDescription schemaEntry
            , wrapperClasses = [ Css.c "w-96" ]
            }
    SS.CseString { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
    SS.CseString c
      | not (A.null c.enum) ->
        renderEnumEntry
          entryIdx
          act
          fallbackTitle
          value
          schemaEntry
          c
          SS.CvString
          identity
    SS.CseString c ->
      InputField.render
        $ InputField.defaultInput
            { props =
              let
                -- Can be removed once
                -- https://github.com/purescript-halogen/purescript-dom-indexed/pull/28
                -- has been released.
                pMinLength = HP.prop (HH.PropName "minLength")

                pMaxLength = HP.prop (HH.PropName "maxLength")

                mi = maybe "0" show c.minLength

                ma = maybe "" show c.maxLength

                pat = case c.pattern of
                  Just pattern -> [ HP.pattern pattern ]
                  Nothing -> []

                placeholder = case c.pattern of
                  Just pattern -> "String matching " <> pattern
                  Nothing -> case Tuple mi ma of
                    Tuple "0" "" -> "String"
                    Tuple "0" ma' -> "String with at most " <> ma' <> " characters"
                    Tuple mi' "" -> "String with at least " <> mi' <> " characters"
                    Tuple mi' ma'
                      | mi' == ma' -> "String with " <> mi' <> " characters"
                      | otherwise -> "String between " <> mi' <> " and " <> ma' <> " characters"
              in
                [ HP.type_ HP.InputText
                , Css.classes
                    $ if state.readOnly then
                        readOnlyInputClasses
                      else
                        [ "nectary-input", "w-full" ]
                , HP.placeholder placeholder
                , HP.disabled state.readOnly
                , HP.required $ maybe false (_ > 0) c.minLength
                -- ^ Basic heuristic. Should support proper JSON Schema `required`.
                , HE.onInput $ CheckInput entryIdx
                , HE.onValueChange (act <<< const <<< SS.CvString)
                ]
                  <> opt pMinLength c.minLength
                  <> opt pMaxLength c.maxLength
                  <> opt HP.value (maybe c.default (Just <<< show) value)
                  <> pat
            , label = fromMaybe fallbackTitle $ SS.configSchemaEntryTitle schemaEntry
            , errorText = Map.lookup entryIdx state.errors
            , tooltipText = SS.configSchemaEntryDescription schemaEntry
            , wrapperClasses = [ Css.c "w-96" ]
            }
    SS.CseRegex { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
    SS.CseRegex c ->
      InputField.render
        $ InputField.defaultInput
            { props =
              [ HP.type_ HP.InputText
              , Css.classes
                  $ if state.readOnly then
                      readOnlyInputClasses
                    else
                      [ "nectary-input", "w-full" ]
              , HP.placeholder $ "String matching " <> c.pattern
              , HP.disabled state.readOnly
              , HP.pattern c.pattern
              , HE.onInput $ CheckInput entryIdx
              , HE.onValueChange (act <<< const <<< SS.CvString)
              ]
                <> opt HP.value (maybe c.default (Just <<< show) value)
            , label = fromMaybe fallbackTitle $ SS.configSchemaEntryTitle schemaEntry
            , errorText = Map.lookup entryIdx state.errors
            , tooltipText = SS.configSchemaEntryDescription schemaEntry
            , wrapperClasses = [ Css.c "w-96" ]
            }
    SS.CseConst _c ->
      renderEntry' fallbackTitle schemaEntry
        $ HH.input [ HP.type_ HP.InputText, HP.value "const", HP.disabled true ]
    SS.CseArray { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
    SS.CseArray c ->
      let
        entries = case value of
          Just (SS.CvArray vals) -> vals
          _ -> []

        toVal = case _ of
          Just (SS.CvArray arr) -> arr
          _ -> []

        act' idx = \f -> act (SS.CvArray <<< fromMaybe [] <<< A.modifyAt idx (f <<< Just) <<< toVal)

        removeAct idx = \_ -> act (SS.CvArray <<< fromMaybe [] <<< A.deleteAt idx <<< toVal)

        mkElement content =
          if S.null fallbackTitle then
            HH.div
              [ Css.classes [ "flex", "flex-col" ] ]
              (content <> [ renderAddListEntry c.items act ])
          else
            HH.fieldset [ Css.classes [ "my-2", "flex", "flex-col", "border" ] ]
              ( [ HH.legend
                    [ Css.classes [ "ml-2", "px-3" ] ]
                    [ withDescription fallbackTitle schemaEntry ]
                ]
                  <> content
                  <> [ renderAddListEntry c.items act ]
              )
      in
        mkElement
          $ A.mapWithIndex
              (\i -> renderListEntry (pushEntryIndex entryIdx i) (act' i) (removeAct i) c.items)
              entries
    SS.CseObject { widget: Just w } -> renderWidget entryIdx fallbackTitle value schemaEntry act w
    SS.CseObject c ->
      let
        findVal k = Map.lookup k $ toVal value

        toVal = case _ of
          Just (SS.CvObject m) -> m
          _ -> Map.empty

        act' k = \f -> act (SS.CvObject <<< Map.alter (Just <<< f) k <<< toVal)

        renderFields =
          A.mapWithIndex
            ( \i (Tuple k schema) ->
                renderEntry (pushEntryIndex entryIdx i) (act' k) k (findVal k) schema
            )
            $ FO.toUnfoldable c.properties
      in
        renderSectionEntry fallbackTitle schemaEntry renderFields
    SS.CseOneOf c ->
      let
        withTabs content =
          HH.div_
            [ HH.slot Tabs.proxy entryIdx Tabs.component
                { selected:
                    fromMaybe 0 do
                      v <- value
                      A.findIndex (\schema -> Schema.isValidValue schema v) c.oneOf
                , tabs:
                    A.mapWithIndex
                      ( \i schema ->
                          { disabled: false
                          , content: HH.text $ fromMaybe (show i) (Schema.getTitle schema)
                          }
                      )
                      c.oneOf
                }
                (SetConfigTab entryIdx)
            , content
            ]

        -- The user has explicitly selected a tab so we try hard to show that
        -- tab, even ignoring the current configuration value if necessary.
        selectedTab = do
          idx <- Map.lookup entryIdx state.configTabs
          schema <- A.index c.oneOf idx
          let
            value' = do
              v <- value
              if Schema.isValidValue schema v then value else Nothing
          pure $ renderEntry entryIdx act "" value' schema

        -- The user has not selected a tab but we have a value so show the
        -- first matching tab.
        matchingValue = do
          v <- value
          schema <- A.find (\schema -> Schema.isValidValue schema v) c.oneOf
          pure $ renderEntry entryIdx act "" value schema

        -- Just show the first tab.
        firstValue = renderEntry entryIdx act "" value <$> A.head c.oneOf

        -- The rendered alternatives. Note, if there is only one alternative
        -- then we immediately render that as if it was the field. Otherwise we
        -- render the different alternatives as tabs.
        inner = case c.oneOf of
          [ elem ] -> renderEntry entryIdx act fallbackTitle value elem
          _ ->
            maybe (HH.text $ "No oneOf schema matches the value: " <> show value) withTabs
              $ selectedTab
              <|> matchingValue
              <|> firstValue
      in
        renderSectionEntry fallbackTitle schemaEntry [ inner ]

  readOnlyInputClasses =
    [ "w-96"
    , "h-12"
    , "px-3"
    , "py-2"
    , "my-0.5"
    , "rounded"
    , "bg-snow-100"
    , "flex"
    , "items-center"
    ]

  pushEntryIndex :: ConfigEntryIndex -> Int -> ConfigEntryIndex
  pushEntryIndex oldIdx idx = oldIdx { entryIndex = idx SList.: oldIdx.entryIndex }

  renderCheckbox fallbackTitle schemaEntry props =
    HH.label [ Css.classes [ "flex", "items-center", "mb-4" ] ]
      [ HH.input
          $ [ HP.type_ HP.InputCheckbox
            , Css.classes [ "nectary-input-checkbox", "mr-5" ]
            ]
          <> props
      , withDescription fallbackTitle schemaEntry
      ]

  renderWidget entryIdx fallbackTitle value schemaEntry act = case _ of
    SS.SwTextarea ->
      labelled
        $ HH.slot
            WTextarea.proxy
            entryIdx
            WTextarea.component
            { value:
                case value of
                  Just (SS.CvString string) -> Just string
                  _ -> Nothing
            , readOnly: state.readOnly
            }
            (mact (act <<< const <<< SS.CvString))
    SS.SwCheckbox { dataSource } ->
      unlabelled
        $ maybe
            insufficientDataError
            ( \getEnumData ->
                HH.slot
                  WCheckbox.proxy
                  entryIdx
                  WCheckbox.component
                  { value:
                      case value of
                        Just (SS.CvArray vs) -> vs
                        _ -> []
                  , getEnumData: getEnumData
                  , readOnly: state.readOnly
                  }
                  (mact (act <<< const <<< SS.CvArray) <<< Just)
            )
            (mkGetEnumData <$> dataSourceWithFallback dataSource)
    SS.SwDropdown { dataSource } ->
      labelled
        $ maybe
            insufficientDataError
            ( \getEnumData ->
                HH.slot
                  WDropdown.proxy
                  entryIdx
                  WDropdown.component
                  { value
                  , getEnumData: getEnumData
                  , readOnly: state.readOnly
                  }
                  (mact (act <<< const))
            )
            (mkGetEnumData <$> dataSourceWithFallback dataSource)
    SS.SwRadio { dataSource } ->
      unlabelled
        $ maybe
            insufficientDataError
            ( \getEnumData ->
                HH.slot
                  WRadio.proxy
                  entryIdx
                  WRadio.component
                  { value, getEnumData: getEnumData }
                  (mact (act <<< const))
            )
            (mkGetEnumData <$> dataSourceWithFallback dataSource)
    SS.SwTypeahead { minInputLength, debounceMs, dataSource } ->
      labelled
        $ maybe
            insufficientDataError
            ( \getEnumData ->
                HH.slot
                  WTypeahead.proxy
                  entryIdx
                  WTypeahead.component
                  { value
                  , minInputLength
                  , debounceMs
                  , getEnumData: getEnumData
                  , readOnly: state.readOnly
                  }
                  (mact (act <<< const))
            )
            (mkGetEnumData <$> dataSourceWithFallback dataSource)
    SS.SwAssetConfigLink { sku } ->
      labelled
        $ HH.slot
            WAssetConfigLink.proxy
            entryIdx
            WAssetConfigLink.component
            { value: maybe' (\_ -> mkDefaultConfig schemaEntry) Just value
            , skuPattern: sku
            , configs: state.getConfigs unit
            , readOnly: state.readOnly
            }
            (mact (act <<< const))
    SS.SwFileAttachment { maxSize, mediaTypes } ->
      unlabelled
        $ case orderLineId of
            Nothing -> HH.text "Please save the order first."
            Just olid ->
              HH.slot
                WFileAttachment.proxy
                entryIdx
                WFileAttachment.component
                { orderLineId: olid
                , value: maybe' (\_ -> mkDefaultConfig schemaEntry) Just value
                , maxSize
                , mediaTypes
                }
                (mact (act <<< const))
    where
    insufficientDataError =
      HH.span
        [ Css.class_ "text-raspberry-500" ]
        [ HH.text "Insufficient data…" ]

    labelled = renderEntry' fallbackTitle schemaEntry

    unlabelled = renderEntryUnlabelled fallbackTitle schemaEntry

    mkGetEnumData :: SS.SchemaDataSourceEnum -> Maybe String -> m DataSourceEnumResult
    mkGetEnumData dataSource = getDataSourceEnum state.dataSourceVars dataSource

    -- Endow a data source with fallback to schema entry enum values. Takes as
    -- input a maybe data source, which is preferred, otherwise uses the enum
    -- values of the current schema entry, and if no enum is available then
    -- nothing is returned.
    dataSourceWithFallback :: Maybe SS.SchemaDataSourceEnum -> Maybe SS.SchemaDataSourceEnum
    dataSourceWithFallback = case _ of
      Just ds -> Just ds
      Nothing -> case schemaEntry of
        SS.CseInteger { enum }
          | not (A.null enum) ->
            Just
              $ SS.SdsEnumMap
                  { entries:
                      Map.fromFoldable
                        $ map (\i -> Tuple (show i) (SS.CvInteger i)) enum
                  }
        SS.CseString { enum }
          | not (A.null enum) ->
            Just
              $ SS.SdsEnumMap
                  { entries:
                      Map.fromFoldable
                        $ map (\s -> Tuple s (SS.CvString s)) enum
                  }
        _ -> Nothing

  renderEntry' fallbackTitle schemaEntry inner =
    if S.null fallbackTitle then
      inner
    else
      HH.label [ Css.classes [ "flex", "flex-col", "mb-4" ] ]
        [ withDescription fallbackTitle schemaEntry
        , inner
        ]

  renderEntryUnlabelled fallbackTitle schemaEntry inner =
    if S.null fallbackTitle then
      inner
    else
      HH.div [ Css.classes [ "flex", "flex-col" ] ]
        [ withDescription fallbackTitle schemaEntry
        , inner
        ]

  renderSectionEntry fallbackTitle schemaEntry fields
    | S.null fallbackTitle =
      HH.div
        [ Css.classes [ "flex", "flex-col", "gap-4" ] ]
        fields
    | otherwise =
      HH.fieldset
        [ Css.classes [ "flex", "flex-col", "gap-4" ] ]
        $ case mTitle of
            Nothing -> fields
            Just title ->
              [ HH.legend
                  [ Css.classes [ "mb-4", "text-xl", "font-semibold" ] ]
                  [ HH.text title ]
              , case SS.configSchemaEntryDescription schemaEntry of
                  Nothing -> HH.text ""
                  Just description -> HH.p_ [ HH.text description ]
              ]
                <> fields
      where
      mTitle = case SS.configSchemaEntryTitle schemaEntry of
        Nothing
          | S.null fallbackTitle -> Nothing
          | otherwise -> Just fallbackTitle
        Just title -> Just title

  renderEnumEntry ::
    forall a r.
    Eq a =>
    ConfigEntryIndex ->
    ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
    String ->
    Maybe SS.ConfigValue ->
    SS.ConfigSchemaEntry ->
    { default :: Maybe a, enum :: Array a | r } ->
    (a -> SS.ConfigValue) ->
    (a -> String) ->
    H.ComponentHTML Action Slots m
  renderEnumEntry entryIdx act fallbackTitle value schemaEntry c mkValue showValue =
    renderEntry' fallbackTitle schemaEntry
      $ if state.readOnly then
          HH.div
            [ Css.classes readOnlyInputClasses ]
            [ HH.text
                $ fromMaybe "" do
                    selVal <- value <|> (mkValue <$> c.default)
                    selected <- A.find (\v -> mkValue v == selVal) c.enum
                    pure $ showValue selected
            ]
        else
          let
            onIndexChange i = mact (act <<< const <<< mkValue) $ A.index c.enum i
          in
            HH.slot
              (Proxy :: Proxy "selectEnum")
              entryIdx
              Select.component
              ( Select.defaultInput
                  { selected =
                    do
                      selVal <- value <|> (mkValue <$> c.default)
                      A.findIndex (\v -> mkValue v == selVal) c.enum
                  , values = A.mapWithIndex (\i e -> Tuple (HH.text $ showValue e) i) c.enum
                  , wrapperClasses = [ Css.c "inline-block", Css.c "w-96" ]
                  }
              )
              onIndexChange

  withDescription fallbackTitle schemaEntry = case SS.configSchemaEntryDescription schemaEntry of
    Nothing -> body false
    Just description ->
      Tooltip.render
        (Tooltip.defaultInput { text = description, width = Just "20rem" })
        (body true)
    where
    body tt =
      HH.div
        [ Css.classes
            [ "font-semibold"
            , "flex"
            , "items-center"
            ]
        ]
        [ HH.text $ fromMaybe fallbackTitle $ SS.configSchemaEntryTitle schemaEntry
        , if tt then Icon.tooltip else HH.text ""
        ]

  renderListEntry ::
    ConfigEntryIndex ->
    ((Maybe SS.ConfigValue -> SS.ConfigValue) -> Action) ->
    (Unit -> Action) ->
    SS.ConfigSchemaEntry ->
    SS.ConfigValue ->
    H.ComponentHTML Action Slots m
  renderListEntry entryIdx act removeAct entry value =
    HH.div [ Css.classes [ "p-3", "border-b", "group" ] ]
      [ renderRemoveListEntry removeAct
      , renderEntry entryIdx act "" (Just value) entry
      ]

  renderRemoveListEntry :: (Unit -> Action) -> H.ComponentHTML Action Slots m
  renderRemoveListEntry removeAct
    | state.readOnly = HH.text ""
    | otherwise =
      HH.button
        [ Css.classes
            [ "nectary-btn-destructive"
            , "h-auto"
            , "relative"
            , "float-right"
            , "py-0"
            , "invisible"
            , "group-hover:visible"
            ]
        , HE.onClick \_ -> removeAct unit
        ]
        [ HH.text "- Remove" ]

  renderAddListEntry schemaEntry act
    | state.readOnly = HH.text ""
    | otherwise =
      HH.div_
        [ HH.button
            [ Css.classes
                [ "nectary-btn-secondary"
                , "h-8"
                , "m-5"
                , "py-0"
                ]
            , HE.onClick \_ ->
                let
                  toVal = case _ of
                    Just (SS.CvArray vals) -> vals
                    _ -> []

                  defValue = maybe [ SS.CvNull ] A.singleton (mkDefaultConfig schemaEntry)

                  addEntry v = v <> defValue
                in
                  act (SS.CvArray <<< addEntry <<< toVal)
            ]
            [ HH.text "Add New" ]
        ]

handleAction ::
  forall slots m.
  MonadAff m => Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  NoOp -> pure unit
  Receive input ->
    H.modify_
      _
        { dataSourceVars = input.dataSourceVars
        , getConfigs = input.getConfigs
        }
  SetConfigTab entryIdx tabIdx ->
    H.modify_ \st ->
      st { configTabs = Map.insert entryIdx tabIdx st.configTabs }
  -- Check that the given input entry is valid. If invalid, then the error
  -- message is added to the state.
  CheckInput entryIdx event -> do
    errorMsg <-
      H.liftEffect
        $ case WebEvent.target event >>= HtmlInputElement.fromEventTarget of
            Nothing -> pure Nothing
            Just target -> do
              validity <- HtmlInputElement.validity target
              let
                -- Checks the validity state using the given function `f`. If
                -- the check fails then return the given error message.
                --
                -- The result of this function is suitable as the `previous`
                -- parameter for a new call to `check`.
                check f msg previous = do
                  p <- previous
                  case p of
                    Just _ -> pure p
                    Nothing ->
                      f validity
                        >>= case _ of
                            false -> pure Nothing
                            true -> pure (Just msg)
              isValid <- Validity.valid validity
              if isValid then
                pure Nothing
              else
                check Validity.valueMissing "Value missing"
                  $ check Validity.typeMismatch "Type mismatch"
                  $ check Validity.patternMismatch "Pattern mismatch"
                  $ check Validity.tooLong "Too long"
                  $ check Validity.tooShort "Too short"
                  $ check Validity.rangeUnderflow "Range underflow"
                  $ check Validity.rangeOverflow "Range overflow"
                  $ check Validity.stepMismatch "Step mismatch"
                  $ check Validity.badInput "Bad input"
                  $ check Validity.customError "Custom error"
                  $ pure Nothing
    H.modify_ \st ->
      st
        { errors =
          maybe'
            (\_ -> Map.delete entryIdx st.errors)
            (\msg -> Map.insert entryIdx msg st.errors)
            errorMsg
        }
  UpdateValue update -> do
    state <- H.modify \st -> st { configValue = update (Just st.configValue) }
    H.raise $ Just state.configValue
