module Widgets
  ( Tab(..)
  , TooltipDirection(..)
  , address
  , dateWithTimeTooltip
  , modal
  , modalCloseBtn
  , monetaryAmount
  , spinner
  , subTotalTable
  , withMaybeTooltip
  , withMaybeTooltip_
  , withTooltip
  , withTooltip_
  ) where

import Prelude
import Css as Css
import Data.Array as A
import Data.BigNumber as BN
import Data.Currency (Currency)
import Data.Currency as Currency
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Iso3166 (countryForCode, subdivisionForCode)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Set as Set
import Data.SmartSpec as SS
import Data.SubTotal (SubTotal(..))
import Data.SubTotal as SubTotal
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data TooltipDirection
  = Top
  | Bottom
  | Left
  | Right

withTooltip_ ::
  forall slot action.
  TooltipDirection ->
  String ->
  HH.HTML slot action ->
  HH.HTML slot action
withTooltip_ = withTooltip []

-- | Creates a span with a tooltip text.
--
-- TODO: Create full tooltip, not just using a title attribute.
withTooltip ::
  forall slot action.
  Array HH.ClassName ->
  TooltipDirection ->
  String ->
  HH.HTML slot action ->
  HH.HTML slot action
withTooltip _classes _direction tooltipText body = HH.span [ HP.title tooltipText ] [ body ]

-- | Creates a span with an optional tooltip text.
withMaybeTooltip_ ::
  forall slot action.
  TooltipDirection ->
  Maybe String ->
  HH.HTML slot action ->
  HH.HTML slot action
withMaybeTooltip_ direction mTooltipText body =
  maybe
    body
    (\tooltipText -> withTooltip_ direction tooltipText body)
    mTooltipText

-- | Creates a span with an optional tooltip text.
withMaybeTooltip ::
  forall slot action.
  Array HH.ClassName ->
  TooltipDirection ->
  Maybe String ->
  HH.HTML slot action ->
  HH.HTML slot action
withMaybeTooltip classes direction mTooltipText body =
  maybe
    (if A.null classes then body else HH.span [ HP.classes classes ] [ body ])
    (\tooltipText -> withTooltip classes direction tooltipText body)
    mTooltipText

-- Other stuff.
type Tab slot action
  = { label :: HH.HTML slot action
    , content :: HH.HTML slot action
    }

dateWithTimeTooltip :: forall slot action. DateTime -> HH.HTML slot action
dateWithTimeTooltip t =
  withTooltip_
    Top
    (SS.prettyDateTime t)
    (HH.text $ SS.prettyDate (DateTime.date t))

modalCloseBtn :: forall slot action. (Unit -> action) -> HH.HTML slot action
modalCloseBtn closeAction =
  HH.button
    [ HP.classes
        [ Css.c "text-lg"
        , Css.c "cursor-pointer"
        , Css.c "px-3"
        , Css.c "py-2"
        ]
    , HE.onClick $ \_ -> closeAction unit
    ]
    [ HH.text "×" ]

modal ::
  forall slot action.
  Array (HH.HTML slot action) ->
  HH.HTML slot action ->
  HH.HTML slot action
modal toolbarContent body =
  faded
    [ wrapper
        $ [ if A.null toolbarContent then empty else toolbar toolbarContent
          , body
          ]
    ]
  where
  empty = HH.span_ []

  faded =
    HH.div
      [ HP.classes
          [ Css.c "fixed"
          , Css.c "inset-0"
          , Css.c "w-full"
          , Css.c "h-full"
          , Css.c "overflow-y-auto"
          , Css.c "z-10"
          , Css.c "bg-black/60"
          , Css.c "flex"
          ]
      ]

  wrapper =
    HH.div
      [ HP.classes
          [ Css.c "mx-auto"
          , Css.c "my-auto"
          , Css.c "p-8"
          , Css.c "bg-white"
          , Css.c "shadow-md"
          , Css.c "rounded-md"
          ]
      ]

  toolbar =
    HH.div
      [ HP.classes
          [ Css.c "relative"
          , Css.c "float-right"
          , Css.c "-m-8"
          ]
      ]

address :: forall slot action. SS.Address -> HH.HTML slot action
address (SS.Address addr) =
  let
    entryRaw title value = case value of
      Nothing -> []
      Just v ->
        [ HH.div [ HP.classes [ Css.c "text-sm", Css.c "text-gray-700" ] ] [ HH.text title ]
        , HH.div [ HP.classes [ Css.c "ml-2", Css.c "text-lg" ] ] v
        ]

    entry title value = entryRaw title ((\v -> [ HH.text v ]) <$> value)

    maybeArray = case _ of
      [] -> Nothing
      xs -> Just xs
  in
    HH.div [ HP.classes [ Css.c "ml-2", Css.c "flex", Css.c "flex-col" ] ]
      $ entryRaw "Street"
          ( maybeArray
              $ A.intersperse HH.br_
              $ map HH.text
              $ A.catMaybes
                  [ addr.line1
                  , addr.line2
                  , addr.line3
                  ]
          )
      <> entry "P/O Box" addr.postOfficeBox
      <> entry "Postal Code" addr.postalCode
      <> entry "City" addr.city
      <> entry "County" addr.county
      <> entry "State or Province"
          ( do
              SS.Country cCode <- addr.country
              SS.Subdivision sCode <- addr.stateOrProvince
              subdiv <- subdivisionForCode cCode sCode
              pure subdiv.name
          )
      <> entry "Country"
          ( do
              SS.Country cCode <- addr.country
              country <- countryForCode cCode
              pure country.name
          )

monetaryAmount :: forall slot action. Currency -> Number -> Array (HH.HTML slot action)
monetaryAmount currency amount = format <$> Currency.formatToParts currency amount
  where
  format = case _ of
    { "type": "currency", value } ->
      HH.span
        [ HP.classes [ Css.c "text-sm", Css.c "text-gray-600" ] ]
        [ HH.text value ]
    { value } -> HH.text value

-- | Render a sub-total using a simple table.
subTotalTable :: forall w i. String -> SubTotal -> HH.HTML w i
subTotalTable title (SubTotal summary) =
  HH.table [ HP.classes [ Css.c "p-5", Css.c "table-auto" ] ]
    $ [ HH.tr [ HP.classes [ Css.c "uppercase", Css.c "text-sm", Css.c "text-gray-600" ] ]
          $ th summary.usage (title <> "Usage")
          <> th summary.monthly (title <> "Monthly")
          <> th summary.quarterly (title <> "Quarterly")
          <> th summary.onetime (title <> "Onetime")
      ]
    <> map renderRow currencies
  where
  currencies =
    A.fromFoldable
      $ Set.unions
          [ SubTotal.toCurrencies summary.usage
          , SubTotal.toCurrencies summary.monthly
          , SubTotal.toCurrencies summary.quarterly
          , SubTotal.toCurrencies summary.onetime
          ]

  th sumry name =
    if SubTotal.isEmpty sumry then
      []
    else
      [ HH.th [ HP.classes [ Css.c "px-5", Css.c "font-normal" ] ] [ HH.text name ] ]

  td sumry =
    if SubTotal.isEmpty sumry then
      const []
    else
      let
        td' currency s =
          [ HH.td
              [ HP.classes [ Css.c "px-5", Css.c "text-right" ] ]
              [ renderSubTotalEntry currency s ]
          ]
      in
        \currency ->
          td' currency
            $ fromMaybe mempty
            $ SubTotal.toSubTotalEntry currency sumry

  renderRow currency =
    HH.tr_
      $ []
      <> td summary.usage currency
      <> td summary.monthly currency
      <> td summary.quarterly currency
      <> td summary.onetime currency

  -- | Render a sub-total entry.
  renderSubTotalEntry :: SS.ChargeCurrency -> SubTotal.SubTotalEntry -> HH.HTML w i
  renderSubTotalEntry (SS.ChargeCurrency currency) amount =
    if amount.price == amount.listPrice then
      renderPrice listPriceClasses amount.price
    else
      HH.span
        [ HP.title ("Without discounts: " <> showMonetary amount.listPrice) ]
        [ renderPrice discountPriceClasses amount.price ]
    where
    listPriceClasses = [ Css.c "px-3", Css.c "text-lg", Css.c "text-right" ]

    discountPriceClasses = [ Css.c "px-3", Css.c "text-lg", Css.c "text-right", Css.c "text-raspberry-500" ]

    renderPrice classes (Additive n) =
      HH.span
        [ HP.classes classes ]
        (monetaryAmount currency (BN.toNumber n))

    showMonetary (Additive n) = Currency.formatter currency (BN.toNumber n)

spinner :: forall w i. Array HH.ClassName -> HH.HTML w i
spinner classes =
  HH.div
    [ HP.classes $ [ Css.c "sofa-spinner" ] <> classes ]
    []
