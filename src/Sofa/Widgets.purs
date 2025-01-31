module Sofa.Widgets
  ( address
  , dateWithTimeTooltip
  , dateWithTimeTooltipRight
  , monetaryAmount
  , subTotalTable
  ) where

import Prelude
import Data.Array as A
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Sofa.Component.Tooltip as TT
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.BigNumber as BN
import Sofa.Data.Currency (Currency)
import Sofa.Data.Currency as Currency
import Sofa.Data.Iso3166 (countryForCode)
import Sofa.Data.SmartSpec as SS
import Sofa.Data.SubTotal (SubTotal(..))
import Sofa.Data.SubTotal as SubTotal

dateWithTimeTooltip :: forall slot action. DateTime -> HH.HTML slot action
dateWithTimeTooltip t =
  TT.render
    (TT.defaultInput { text = SS.prettyDateTime t })
    (HH.text $ SS.prettyDateTimeWithoutTimeZone t)

dateWithTimeTooltipRight :: forall slot action. DateTime -> HH.HTML slot action
dateWithTimeTooltipRight t =
  TT.render
    ( TT.defaultInput
        { text = SS.prettyDateTime t
        , orientation = TT.Right
        }
    )
    (HH.text $ SS.prettyDate (DateTime.date t))

address :: forall w i. SS.Address -> Array (HH.HTML w i)
address (SS.Address addr) =
  let
    entryRaw title value = case value of
      Nothing -> []
      Just v ->
        [ HH.h4_ [ HH.text title ]
        , HH.div_ v
        ]

    entry title value = entryRaw title ((\v -> [ HH.text v ]) <$> value)

    maybeArray = case _ of
      [] -> Nothing
      xs -> Just xs
  in
    entryRaw "Street"
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
      <> entry "State or Province" addr.stateOrProvince
      <> entry "Country"
          ( do
              SS.Country cCode <- addr.country
              country <- countryForCode cCode
              pure country.name
          )

-- | Render a monetary amount. The rendering will follow the US locale style
-- | with the actual currency shown using a smaller font in a subdued color.
monetaryAmount :: forall slot action. Currency -> Number -> Array (HH.HTML slot action)
monetaryAmount currency amount = format <$> Currency.formatToParts currency amount
  where
  format = case _ of
    { "type": "currency", value } ->
      HH.span
        [ Css.classes [ "text-sm", "text-gray-600" ] ]
        [ HH.text value ]
    { value } -> HH.text value

-- | Render a sub-total using a simple table.
subTotalTable :: forall w i. String -> SubTotal -> HH.HTML w i
subTotalTable title (SubTotal summary) =
  HH.table [ Css.classes [ "p-5", "table-auto" ] ]
    $ A.mapMaybe renderRow
        [ Tuple "Usage" summary.usage
        , Tuple "Monthly" summary.monthly
        , Tuple "Quarterly" summary.quarterly
        , Tuple "Onetime" summary.onetime
        ]
  where
  currencies =
    A.fromFoldable
      $ Set.unions
          [ SubTotal.toCurrencies summary.usage
          , SubTotal.toCurrencies summary.monthly
          , SubTotal.toCurrencies summary.quarterly
          , SubTotal.toCurrencies summary.onetime
          ]

  renderRow (Tuple label sumry)
    | SubTotal.isEmpty sumry = Nothing
    | otherwise =
      Just
        $ HH.tr_
        $ [ th (title <> label) ]
        <> map (renderCell sumry) currencies

  renderCell sumry currency =
    td currency
      $ fromMaybe mempty
      $ SubTotal.toSubTotalEntry currency sumry

  th label =
    HH.th
      [ Css.classes [ "text-right", "font-semibold" ] ]
      [ HH.text label ]

  td currency s =
    HH.td
      [ Css.classes [ "text-right", "text-lg" ] ]
      [ renderSubTotalEntry currency s ]

  -- | Render a sub-total entry.
  renderSubTotalEntry :: SS.ChargeCurrency -> SubTotal.SubTotalEntry -> HH.HTML w i
  renderSubTotalEntry (SS.ChargeCurrency currency) amount =
    if amount.price == amount.listPrice then
      renderPrice listPriceClasses amount.price
    else
      Tooltip.render
        ( Tooltip.defaultInput
            { text = "Without discounts: " <> showMonetary amount.listPrice
            }
        )
        $ renderPrice discountPriceClasses amount.price
    where
    listPriceClasses = [ "pl-5" ]

    discountPriceClasses = [ "pl-5", "text-raspberry-500" ]

    renderPrice classes (Additive n) =
      HH.span
        [ Css.classes classes ]
        (monetaryAmount currency (BN.toNumber n))

    showMonetary (Additive n) = Currency.formatter currency (BN.toNumber n)
