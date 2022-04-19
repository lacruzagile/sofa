module Sofa.Widgets
  ( Tab(..)
  , address
  , dateWithTimeTooltip
  , dateWithTimeTooltipRight
  , monetaryAmount
  , spinner
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
import Halogen.HTML.Properties as HP
import Sofa.Component.Tooltip as TT
import Sofa.Component.Tooltip as Tooltip
import Sofa.Css as Css
import Sofa.Data.BigNumber as BN
import Sofa.Data.Currency (Currency)
import Sofa.Data.Currency as Currency
import Sofa.Data.Iso3166 (countryForCode, subdivisionForCode)
import Sofa.Data.SmartSpec as SS
import Sofa.Data.SubTotal (SubTotal(..))
import Sofa.Data.SubTotal as SubTotal

-- Other stuff.
type Tab slot action
  = { label :: HH.HTML slot action
    , content :: HH.HTML slot action
    }

dateWithTimeTooltip :: forall slot action. DateTime -> HH.HTML slot action
dateWithTimeTooltip t =
  TT.render
    (TT.defaultInput { text = SS.prettyDateTime t })
    (HH.text $ SS.prettyDate (DateTime.date t))

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
      [ HP.classes
          [ Css.c "text-right", Css.c "font-semibold"
          ]
      ]
      [ HH.text label ]

  td currency s =
    HH.td
      [ HP.classes [ Css.c "text-right", Css.c "text-lg" ] ]
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
    listPriceClasses = [ Css.c "pl-5" ]

    discountPriceClasses = [ Css.c "pl-5", Css.c "text-raspberry-500" ]

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
