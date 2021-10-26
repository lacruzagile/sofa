-- A collection of data types and functions to help calculate and render
-- sub-totals.
module Data.SubTotal
  ( IndexedSubTotalEntry(..)
  , SubTotal(..)
  , SubTotalEntry(..)
  , calcSubTotal
  , isEmpty
  , renderSubTotalTable
  , renderSubTotalText
  , toCurrencies
  , toSubTotalEntry
  ) where

import Prelude
import Css as Css
import Data.Array as A
import Data.Charge (ChargeUnitMap)
import Data.Either (Either(..))
import Data.Estimate (Estimate(..))
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Number.Format (toStringWith, fixed)
import Data.Quantity (Quantity, QuantityMap)
import Data.Set (Set)
import Data.Set as Set
import Data.SmartSpec (Charge(..), ChargeCurrency, ChargeCurrencyPerUnit(..), ChargeSingleUnit(..), ChargeType(..), ChargeUnit(..), ChargeUnitID, DefaultPricePerUnit(..), DimValue, Price(..), PricePerDim(..), PricePerDimSeg(..), PricePerDimUnit(..), PricePerDimUnitOptSeg(..), PricePerDimUnitSeg(..), PricePerSeg(..), PricePerUnit(..), PricePerUnitSeg(..), Segmentation(..), SegmentationDim(..), SegmentationDimPerUnit(..), SegmentationModel(..), SegmentationOptDim(..), SegmentationOptDimPerUnit(..), SegmentationPerUnit(..))
import Data.Tuple (Tuple(..), uncurry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Widgets as Widgets

type SubTotalEntry
  = Estimate
      { price :: Additive Number
      , listPrice :: Additive Number
      }

newtype IndexedSubTotalEntry
  = IndexedSubTotalEntry (Map ChargeCurrency SubTotalEntry)

derive newtype instance eqIndexedSubTotalEntry :: Eq IndexedSubTotalEntry

derive newtype instance showIndexedSubTotalEntry :: Show IndexedSubTotalEntry

instance semigroupIndexedSubTotalEntry :: Semigroup IndexedSubTotalEntry where
  append (IndexedSubTotalEntry a) (IndexedSubTotalEntry b) = IndexedSubTotalEntry $ Map.unionWith (<>) a b

instance monoidIndexedSubTotalEntry :: Monoid IndexedSubTotalEntry where
  mempty = IndexedSubTotalEntry Map.empty

-- | A sub-total price.
newtype SubTotal
  = SubTotal
  { onetime :: IndexedSubTotalEntry -- ^ Onetime price.
  , monthly :: IndexedSubTotalEntry -- ^ Monthly price.
  , quarterly :: IndexedSubTotalEntry -- ^ Quarterly price.
  , usage :: IndexedSubTotalEntry -- ^ Usage price.
  , segment :: IndexedSubTotalEntry -- ^ Monthly segment price.
  }

derive newtype instance eqSubTotal :: Eq SubTotal

derive newtype instance showSubTotal :: Show SubTotal

derive newtype instance semigroupSubTotal :: Semigroup SubTotal

derive newtype instance monoidSubTotal :: Monoid SubTotal

-- | Calculates the sub-total using the given information.
calcSubTotal :: QuantityMap -> ChargeUnitMap -> ChargeCurrency -> Charge -> SubTotal
calcSubTotal quantityMap unitMap defaultCurrency = case _ of
  ChargeSingleUnit c -> fromSingleUnit c
  ChargeList c -> A.foldl (\a b -> a <> fromSingleUnit b) mempty c.charges
  ChargeDimUnitOptSeg c -> fromMultiUnit c
  where
  fromSingleUnit =
    fromMaybe mempty
      <<< case _ of
          ChargeSimple c@{ unit, currency } -> calcSingleEntrySubTotal unit currency $ \q -> calcForPrice q c
          ChargeDim { unit, currency, priceByDim } -> calcSingleEntrySubTotalDim unit currency $ \dimQ -> calcForPricePerDim dimQ priceByDim
          ChargeSeg { unit, currency, segmentation: Segmentation { model }, priceBySegment } -> calcSingleEntrySubTotal unit currency $ \q -> calcForPricePerSeg q model priceBySegment
          ChargeDimSeg { unit, currency, segmentation, priceBySegmentByDim } ->
            calcSingleEntrySubTotalDim unit currency
              $ \dimQ ->
                  let
                    model = getSegmentationModel segmentation
                  in
                    calcForPricePerDimSeg dimQ model priceBySegmentByDim

  calcSingleEntrySubTotal unitID currency go = do
    quantity <- Map.lookup unitID quantityMap
    ChargeUnit chargeUnit <- Map.lookup unitID unitMap
    case quantity of
      Left q -> pure $ mkSubTotal chargeUnit.chargeType $ mkIndexed currency $ go q
      Right _dimQ -> Nothing

  calcSingleEntrySubTotalDim unitID currency go = do
    quantity <- Map.lookup unitID quantityMap
    ChargeUnit chargeUnit <- Map.lookup unitID unitMap
    case quantity of
      Left _q -> Nothing
      Right dimQ -> pure $ mkSubTotal chargeUnit.chargeType $ mkIndexed currency $ go dimQ

  mkIndexed currency entry =
    IndexedSubTotalEntry
      $ Map.singleton (fromMaybe defaultCurrency currency) entry

  getSegmentationModel = case _ of
    SegmentationOptUndim (Segmentation { model }) -> model
    SegmentationOptDim (SegmentationDim { model }) -> model

  getSegmentationUnit :: SegmentationOptDimPerUnit -> SegmentationOptDim
  getSegmentationUnit = case _ of
    SegmentationOptUndimPerUnit (SegmentationPerUnit { segmentation }) -> SegmentationOptUndim segmentation
    SegmentationOptDimPerUnit (SegmentationDimPerUnit { segmentation }) -> SegmentationOptDim segmentation

  getUnitID :: SegmentationOptDimPerUnit -> ChargeUnitID
  getUnitID = case _ of
    SegmentationOptUndimPerUnit (SegmentationPerUnit { unit }) -> unit
    SegmentationOptDimPerUnit (SegmentationDimPerUnit { unit }) -> unit

  getCurrency unitID (ChargeCurrencyPerUnit x)
    | x.unit == unitID = Just x.currency
    | otherwise = Nothing

  getDefaultPrice unitID (DefaultPricePerUnit x)
    | x.unit == unitID = Just $ Price { price: x.price, listPrice: x.listPrice, discount: x.discount }
    | otherwise = Nothing

  fromMultiUnit charge = A.foldl (\a unitID -> a <> forUnit unitID) mempty $ charge.units
    where
    bubbled = bubblePricePerDimUnitOptSeg charge.priceByUnitByDim

    forUnit unitID =
      fromMaybe mempty do
        pricePerDimOptSeg <- Map.lookup unitID bubbled
        ChargeUnit chargeUnit <- Map.lookup unitID unitMap
        let
          currency = A.findMap (getCurrency unitID) charge.currencyByUnit

          defaultPrice = A.findMap (getDefaultPrice unitID) charge.defaultPriceByUnit

          finish = pure <<< mkSubTotal chargeUnit.chargeType <<< mkIndexed currency
        quantity <- Map.lookup unitID quantityMap
        case pricePerDimOptSeg of
          PricePerDimOptSeg ps -> do
            segmentation <-
              A.findMap
                ( \seg ->
                    if getUnitID seg == unitID then
                      Just (getSegmentationUnit seg)
                    else
                      Nothing
                )
                charge.segmentationByUnit
            let
              model = getSegmentationModel segmentation
            case quantity of
              Left _q -> pure mempty -- Should probably be some error.
              Right dimQ -> finish $ calcForPricePerDimSeg dimQ model ps
          PricePerDimOptNoSeg ps -> case quantity of
            Left _q -> pure mempty -- Should probably be some error.
            Right dimQ -> finish $ calcForPricePerDim dimQ ps
          _ -> mempty

-- Move unit to top-level. This essentially swaps places between unit and
-- dimension to make the structure more convenient.
bubblePricePerDimUnitOptSeg :: Array PricePerDimUnitOptSeg -> Map ChargeUnitID PricePerDimOptSeg
bubblePricePerDimUnitOptSeg = Map.fromFoldableWith (<>) <<< List.concatMap foo <<< toList
  where
  toList :: forall a. Array a -> List a
  toList = List.fromFoldable

  bar ppdu (PricePerUnitSeg ppu) =
    Tuple ppu.unit $ PricePerDimOptSeg $ A.singleton
      $ PricePerDimSeg
          { dim: ppdu.dim
          , priceBySegment: ppu.priceBySegment
          , periodMinimum: ppdu.periodMinimum
          }

  baz ppdu (PricePerUnit ppu) = do
    Tuple ppu.unit $ PricePerDimOptNoSeg $ A.singleton
      $ PricePerDim
          { dim: ppdu.dim
          , price: ppu.price
          , listPrice: ppu.listPrice
          , discount: ppu.discount
          }

  foo :: PricePerDimUnitOptSeg -> List (Tuple ChargeUnitID PricePerDimOptSeg)
  foo = case _ of
    PricePerDimUnitOptSeg (PricePerDimUnitSeg ppdu) -> bar ppdu <$> toList ppdu.priceBySegmentByUnit
    PricePerDimUnitOptNoSeg (PricePerDimUnit ppdu) -> baz ppdu <$> toList ppdu.priceByUnit

data PricePerDimOptSeg
  = PricePerDimOptSeg (Array PricePerDimSeg)
  | PricePerDimOptNoSeg (Array PricePerDim)
  | PricePerDimOptSegNil
  | PricePerDimOptSegMismatch

instance semigroupPricePerDimOptSeg :: Semigroup PricePerDimOptSeg where
  append (PricePerDimOptSeg as) (PricePerDimOptSeg bs) = PricePerDimOptSeg (as <> bs)
  append (PricePerDimOptNoSeg as) (PricePerDimOptNoSeg bs) = PricePerDimOptNoSeg (as <> bs)
  append PricePerDimOptSegNil b = b
  append a PricePerDimOptSegNil = a
  append _ _ = PricePerDimOptSegMismatch

instance monoidPricePerDimOptSeg :: Monoid PricePerDimOptSeg where
  mempty = PricePerDimOptSegNil

-- Create a `SubTotal` value for the given entry using the given charge type.
mkSubTotal :: ChargeType -> IndexedSubTotalEntry -> SubTotal
mkSubTotal chargeType entry = case chargeType of
  ChargeTypeOnetime -> SubTotal $ zero { onetime = entry }
  ChargeTypeMonthly -> SubTotal $ zero { monthly = entry }
  ChargeTypeQuarterly -> SubTotal $ zero { quarterly = entry }
  ChargeTypeUsage -> SubTotal $ zero { usage = entry }
  ChargeTypeSegment -> SubTotal $ zero { segment = entry }
  where
  SubTotal zero = mempty

calcForPrice ::
  forall r.
  Quantity ->
  { price :: Number
  , listPrice :: Number
  | r
  } ->
  SubTotalEntry
calcForPrice quantity p = scaleBy p <$> quantity

calcForPricePerDim :: Map DimValue Quantity -> Array PricePerDim -> SubTotalEntry
calcForPricePerDim quantities = A.foldl (\a b -> a <> lookup b) mempty
  where
  lookup (PricePerDim p@{ dim }) = fromMaybe mempty $ map (scaleBy p) <$> Map.lookup dim quantities

calcForPricePerSeg :: Quantity -> SegmentationModel -> Array PricePerSeg -> SubTotalEntry
calcForPricePerSeg quantity segmentationModel priceBySeg = case segmentationModel of
  SegmentationModelTiered -> calcTieredEntry quantity priceBySeg
  SegmentationModelVolume -> calcVolumeEntry quantity priceBySeg
  _ -> calcTieredEntry quantity priceBySeg

-- | Calculates a sub-total entry using the "tiered" segmentation model.
-- |
-- | Specifically, this finds the price segment that covers the given quantity
-- | and uses that segment's price for the entire quantity.
calcVolumeEntry :: Quantity -> Array PricePerSeg -> SubTotalEntry
calcVolumeEntry quantity priceBySeg = calc <$> quantity
  where
  calc q = maybe mempty (calc' q) $ A.find (inSegment q) priceBySeg

  inSegment q (PricePerSeg p) = q >= p.minimum && maybe true (q < _) p.exclusiveMaximum

  calc' q (PricePerSeg p) = scaleBy p q

-- | Calculates a sub-total entry using the "tiered" segmentation model.
-- |
-- | Specifically, this deducts quantity for each segment in rising order and
-- | calculates sub-totals for each segment, which are then summed.
calcTieredEntry :: Quantity -> Array PricePerSeg -> SubTotalEntry
calcTieredEntry quantity priceBySeg = calc <$> quantity
  where
  calc q = A.foldl (accTot q) mempty $ A.takeWhile (ge q) priceBySeg

  ge q (PricePerSeg p) = q >= p.minimum

  accTot q acc (PricePerSeg p) = case p.exclusiveMaximum of
    Just exmax
      | exmax <= q ->
        let
          segmentSize = (fromMaybe 0 p.exclusiveMaximum) - (max 1 p.minimum)
        in
          acc <> scaleBy p segmentSize
    _ ->
      let
        qInSegment = if p.minimum == 0 then q else q - p.minimum + 1
      in
        acc <> scaleBy p qInSegment

-- | Scales the given price entry by the given quantity. Note, for performance
-- | reasons this works on non-estimated values. Estimation is expected to be
-- | handled separately.
scaleBy ::
  forall r.
  { price :: Number, listPrice :: Number | r } ->
  Int ->
  { price :: Additive Number
  , listPrice :: Additive Number
  }
scaleBy pps quantity =
  { price: Additive $ q * pps.price
  , listPrice: Additive $ q * pps.listPrice
  }
  where
  q = Int.toNumber quantity

calcForPricePerDimSeg :: Map DimValue Quantity -> SegmentationModel -> Array PricePerDimSeg -> SubTotalEntry
calcForPricePerDimSeg quantities segmentationModel = A.foldl (\a b -> a <> lookup b) mempty
  where
  lookup (PricePerDimSeg { dim, priceBySegment }) =
    fromMaybe mempty
      $ calcSeg priceBySegment
      <$> Map.lookup dim quantities

  calcSeg priceBySegment q = calcForPricePerSeg q segmentationModel priceBySegment

toCurrencies :: IndexedSubTotalEntry -> Set ChargeCurrency
toCurrencies (IndexedSubTotalEntry es) = Map.keys es

isEmpty :: IndexedSubTotalEntry -> Boolean
isEmpty (IndexedSubTotalEntry es) = Map.isEmpty es

toSubTotalEntry :: ChargeCurrency -> IndexedSubTotalEntry -> Maybe SubTotalEntry
toSubTotalEntry currency (IndexedSubTotalEntry es) = Map.lookup currency es

-- | Render a sub-total as a single text entry.
renderSubTotalText ::
  forall action slots m.
  Monad m =>
  SubTotal ->
  H.ComponentHTML action slots m
renderSubTotalText (SubTotal subTotal) =
  HH.span_ $ handleEmpty
    $ renderIndexedSubTotalEntry "" subTotal.onetime
    <> renderIndexedSubTotalEntry " per month" subTotal.monthly
    <> renderIndexedSubTotalEntry " per quarter" subTotal.quarterly
    <> renderIndexedSubTotalEntry " per month" subTotal.usage
    <> renderIndexedSubTotalEntry "" subTotal.segment
  where
  handleEmpty = case _ of
    [] -> [ HH.text "N/A" ]
    xs -> xs

  renderIndexedSubTotalEntry suffix (IndexedSubTotalEntry entry)
    | Map.isEmpty entry = []
    | otherwise =
      A.intercalate [ HH.text ", " ]
        $ map (\r -> [ r, HH.text suffix ])
        $ renderSubTotalEntries entry

  renderSubTotalEntries = map (uncurry renderSubTotalEntry) <<< Map.toUnfoldable

-- | Render a sub-total using a simple table.
renderSubTotalTable :: forall action slots m. Monad m => String -> SubTotal -> H.ComponentHTML action slots m
renderSubTotalTable title (SubTotal summary) =
  let
    currencies =
      A.fromFoldable
        $ Set.unions
            [ toCurrencies summary.usage
            , toCurrencies summary.monthly
            , toCurrencies summary.quarterly
            , toCurrencies summary.onetime
            ]

    th sumry name = if isEmpty sumry then [] else [ HH.th_ [ HH.text name ] ]

    td sumry =
      if isEmpty sumry then
        const []
      else
        let
          td' currency s = [ HH.td_ [ renderSubTotalEntry currency s ] ]
        in
          \currency -> td' currency $ fromMaybe mempty $ toSubTotalEntry currency sumry

    renderRow currency =
      HH.tr_
        $ []
        <> td summary.usage currency
        <> td summary.monthly currency
        <> td summary.quarterly currency
        <> td summary.onetime currency
  in
    HH.table [ HP.class_ Css.subTotal ]
      $ [ HH.tr_
            $ [ HH.th [ HP.rowSpan (1 + A.length currencies) ] [ HH.text title ] ]
            <> th summary.usage "Usage"
            <> th summary.monthly "Monthly"
            <> th summary.quarterly "Quarterly"
            <> th summary.onetime "Onetime"
        ]
      <> map renderRow currencies

-- | Render a sub-total entry.
renderSubTotalEntry ::
  forall action slots m.
  Monad m =>
  ChargeCurrency ->
  SubTotalEntry ->
  H.ComponentHTML action slots m
renderSubTotalEntry currencyCode amount =
  let
    amount' = case amount of
      Exact a -> { price: Exact a.price, listPrice: Exact a.listPrice }
      Estimate a -> { price: Estimate a.price, listPrice: Estimate a.listPrice }
  in
    if amount'.price == amount'.listPrice then
      HH.text $ showMonetary amount'.price <> " " <> show currencyCode
    else
      Widgets.withTooltip Widgets.Top ("Without discounts: " <> showMonetary amount'.listPrice)
        $ HH.span_
            [ HH.span [ HP.style "color:red" ] [ HH.text $ showMonetary amount'.price ]
            , HH.text " "
            , HH.text (show currencyCode)
            ]

showMonetary :: Estimate (Additive Number) -> String
showMonetary = showEst <<< map (\(Additive n) -> toStringWith (fixed 2) n)
  where
  showEst = case _ of
    Exact s -> s
    Estimate s -> "~" <> s
