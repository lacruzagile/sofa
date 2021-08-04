module App.SolVis (Slot, proxy, component) where

import Prelude
import Affjax (printError)
import Control.Monad.State (class MonadState)
import Css as Css
import Data.Argonaut (class DecodeJson, printJsonDecodeError)
import Data.Array (concatMap, mapWithIndex, modifyAt, (!!))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
import Data.Tuple (uncurry)
import Data.Variant (default, on)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.Ajax (_affjaxError, _notFound, _parseError)
import Simple.Ajax as AJX
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "solVis"
proxy = Proxy

type State
  = StateMeta

data SubState a
  = Idle
  | Success a
  | Loading
  | Error String

type StateMeta
  = SubState { meta :: SS.Meta, solutions :: Array StateSolution }

type StateSolution
  = { name :: String
    , expanded :: Boolean
    , solution :: SubState SS.Solution
    }

data Action
  = GetMeta
  | ToggleSolution Int

component ::
  forall query input output m.
  MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just GetMeta }
    }

initialState :: forall input. input -> State
initialState _ = Idle

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div_
    ( [ HH.h1_
          [ HH.text "Solution Visualizer" ]
      ]
        <> content
    )
  where
  opt :: forall a b. (a -> Array b) -> Maybe a -> Array b
  opt = maybe []

  blockList = HH.ul [ HP.class_ Css.blocklist ]

  dataItem label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ HH.text value ]
    ]

  dataItemRaw label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ value ]
    ]

  billingUnit :: SS.BillingUnit -> H.ComponentHTML Action slots m
  billingUnit (SS.BillingUnit bu) =
    HH.li_
      [ HH.dl_
          ( opt (dataItem "ID") bu.id
              <> opt (dataItem "Name") bu.name
              <> dataItem "Charge Type" (show bu.chargeType)
              <> opt (dataItem "Description") bu.description
          )
      ]

  dimTypeSchema :: Map String SS.ConfigSchemaEntry -> Array (H.ComponentHTML Action slots m)
  dimTypeSchema = html <<< HH.dl_ <<< concatMap (uncurry configSchemaEntry) <<< Map.toUnfoldable
    where
    html x =
      [ HH.dt_ [ HH.text "Schema" ]
      , HH.dd_ [ x ]
      ]

  dimType :: SS.DimType -> H.ComponentHTML Action slots m
  dimType (SS.DimType dt) =
    HH.li_
      [ HH.dl_
          ( opt (dataItem "ID") dt.id
              <> opt (dataItem "Name") dt.name
              <> opt (dataItem "Description") dt.description
              <> dimTypeSchema dt.schema
          )
      ]

  dimTypeRef :: SS.DimTypeRef -> Array (H.ComponentHTML Action slots m)
  dimTypeRef (SS.DimTypeRef r) =
    dataItemRaw "Dimension Type Reference"
      $ HH.dl_
          ( dataItem "Dimension Type ID" r.dimTypeId
              <> opt (dataItem "Solution URI") r.solutionUri
          )

  sku :: SS.Sku -> H.ComponentHTML Action slots m
  sku (SS.SkuCode s) = HH.text s

  sku (SS.Sku s) =
    HH.dl [ HP.class_ Css.hblock ]
      ( dataItem "Code" s.code
          <> opt (dataItem "Description") s.description
          <> opt (dataItem "Name") s.name
          <> opt (dataItem "Platform") (show <$> s.platform)
          <> opt (dataItem "Product Category") (show <$> s.productCategory)
          <> opt (dataItem "Account Product") (show <$> s.accountProduct)
          <> opt (dataItem "Billable Product") (show <$> s.billableProduct)
          <> opt (dataItem "Commercial Product") (show <$> s.commercialProduct)
      )

  productOption :: SS.ProductOption -> H.ComponentHTML Action slots m
  productOption (SS.ProdOptSkuCode s) = HH.li_ [ HH.text s ]

  productOption (SS.ProductOption po) =
    HH.li_
      [ HH.dl_
          ( dataItemRaw "SKU" (sku po.sku)
              <> opt (dataItem "Name") po.name
              <> dataItem "Required" (show po.required)
              <> dataItem "Quote Line Visible" (show po.quoteLineVisible)
              <> dataItem "Quantity" (show po.quantity)
              <> dataItem "Min Quantity" (show po.minQuantity)
              <> dataItem "Max Quantity" (show po.maxQuantity)
              {- <> dataItem "Required Options" (show po.quantity)
                 <> dataItem "Exclude Options" (show po.quantity) -}
              
              <> dataItem "Selected by Default" (show po.selectedByDefault)
              <> dataItem "Type" (show po.type_)
          )
      ]

  productOptions :: Maybe (Array SS.ProductOption) -> Array (H.ComponentHTML Action slots m)
  productOptions = maybe [] (html <<< blockList <<< map productOption)
    where
    html x =
      [ HH.dt_ [ HH.text "Product Options" ]
      , HH.dd_ [ x ]
      ]

  configSchemaEntry :: String -> SS.ConfigSchemaEntry -> Array (H.ComponentHTML Action slots m)
  configSchemaEntry k = case _ of
    SS.CseInteger v ->
      [ HH.dt_ [ HH.text k, HH.text " (integer)" ]
      , HH.dd_
          [ HH.dl_
              ( opt (dataItem "Minimum" <<< show) v.minimum
                  <> opt (dataItem "Maximum" <<< show) v.maximum
                  <> opt (dataItem "Default" <<< show) v.default
              )
          ]
      ]
    SS.CseString v ->
      [ HH.dt_ [ HH.text k, HH.text " (string)" ]
      , HH.dd_
          [ HH.dl_
              ( opt (dataItem "Minimum Length" <<< show) v.minLength
                  <> opt (dataItem "Maximum Length" <<< show) v.maxLength
                  <> opt (dataItem "Default") v.default
              )
          ]
      ]
    SS.CseRegex v ->
      [ HH.dt_ [ HH.text k, HH.text " (regex)" ]
      , HH.dd_
          [ HH.dl_
              ( dataItem "Pattern" v.pattern
                  <> opt (dataItem "Default") v.default
              )
          ]
      ]

  configSchema :: Maybe (Map String SS.ConfigSchemaEntry) -> Array (H.ComponentHTML Action slots m)
  configSchema = maybe [] (html <<< HH.dl_ <<< concatMap (uncurry configSchemaEntry) <<< Map.toUnfoldable)
    where
    html x =
      [ HH.dt_ [ HH.text "Configuration Schema" ]
      , HH.dd_ [ x ]
      ]

  product :: SS.Product -> H.ComponentHTML Action slots m
  product (SS.Product p) =
    HH.li_
      [ HH.dl_
          ( dataItem "SKU" p.sku
              <> dataItem "Description" p.description
              <> productOptions p.options
              <> configSchema p.configSchema
          )
      ]

  billingUnitRef :: SS.BillingUnitRef -> String
  billingUnitRef (SS.BillingUnitRef bur) =
    bur.billingUnitId
      <> (maybe "" (\x -> " [" <> show x <> "]") bur.solutionUri)

  rateElementOnetime :: SS.RateElementOnetime -> Array (H.ComponentHTML Action slots m)
  rateElementOnetime (SS.RateElementOnetime r) =
    dataItem "Onetime Charge"
      $ billingUnitRef r.billingUnitRef
      <> " "
      <> show r.price

  onetimeCharges :: Array SS.OnetimeCharge -> Array (H.ComponentHTML Action slots m)
  onetimeCharges xs =
    dataItemRaw "Onetime Charges"
      ( blockList
          $ map (\(SS.OnetimeCharge x) -> HH.li_ [ HH.dl_ $ dataItem "ID" x.id <> rateElementOnetime x.element ]) xs
      )

  rateElementMonthly :: SS.RateElementMonthly -> Array (H.ComponentHTML Action slots m)
  rateElementMonthly (SS.RateElementMonthly r) =
    dataItem "Monthly Charge"
      $ billingUnitRef r.billingUnitRef
      <> " "
      <> show r.price

  monthlyCharges :: Array SS.MonthlyCharge -> Array (H.ComponentHTML Action slots m)
  monthlyCharges xs =
    dataItemRaw "Monthly Charges"
      ( blockList
          $ map (\(SS.MonthlyCharge x) -> HH.li_ [ HH.dl_ $ dataItem "ID" x.id <> rateElementMonthly x.element ]) xs
      )

  segmentedPrice :: SS.SegmentedPrice -> String
  segmentedPrice = case _ of
    SS.FixedPrice p -> show p
    SS.SegmentedPrice p ->
      show p.price
        <> " ["
        <> show p.minimum
        <> ", "
        <> show p.exclusiveMaximum
        <> ")"

  unitPriceByBillingUnit :: SS.UnitPriceByBillingUnit -> H.ComponentHTML Action slots m
  unitPriceByBillingUnit (SS.UnitPriceByBillingUnit r) =
    HH.li_
      $ dataItem "Billing Unit Reference" (billingUnitRef r.billingUnitRef)
      <> dataItem "Price" (segmentedPrice r.price)

  configValues :: Map String SS.ConfigValue -> H.ComponentHTML Action slots m
  configValues = HH.dl_ <<< concatMap (uncurry entry) <<< Map.toUnfoldable
    where
    entry k v = dataItem k (show v)

  unitPricePerDimByBillingUnit :: SS.UnitPricePerDimByBillingUnit -> H.ComponentHTML Action slots m
  unitPricePerDimByBillingUnit (SS.UnitPricePerDimByBillingUnit r) =
    HH.li_
      $ dataItemRaw "Dimensions" (configValues r.dim)
      <> dataItem "Monthly Minimum" (show r.monthlyMinimum)
      <> dataItemRaw "Unit Prices by Billing Unit" unitPricesPerBillingUnit
    where
    unitPricesPerBillingUnit = blockList (map unitPriceByBillingUnit r.unitPricesByBillingUnit)

  unitPricesPerDimByBillingUnit :: Array SS.UnitPricePerDimByBillingUnit -> H.ComponentHTML Action slots m
  unitPricesPerDimByBillingUnit elems = blockList (map unitPricePerDimByBillingUnit elems)

  rateElementUsage :: SS.RateElementUsage -> Array (H.ComponentHTML Action slots m)
  rateElementUsage (SS.RateElementUsage r) =
    maybe [] dimTypeRef r.dimTypeRef
      <> dataItem "Term of Price Change in Days" (show r.termOfPriceChangeInDays)
      <> dataItem "Monthly Minimum" (show r.monthlyMinimum)
      <> dataItemRaw "Unit Price per Dimension by Billing Unit" (unitPricesPerDimByBillingUnit r.unitPricePerDimByBillingUnit)

  usageCharges :: Array SS.UsageCharge -> Array (H.ComponentHTML Action slots m)
  usageCharges xs =
    dataItemRaw "Usage Charges"
      ( blockList
          $ map (\(SS.UsageCharge x) -> HH.li_ [ HH.dl_ $ dataItem "ID" x.id <> rateElementUsage x.element ]) xs
      )

  rateCardCharge :: SS.RateCardCharge -> Array (H.ComponentHTML Action slots m)
  rateCardCharge = case _ of
    SS.RateCardCharge1 r ->
      rateElementOnetime r.onetimeCharge
        <> rateElementMonthly r.monthlyCharge
    SS.RateCardCharge2 r ->
      rateElementOnetime r.onetimeCharge
        <> monthlyCharges r.monthlyCharges
    SS.RateCardCharge3 r ->
      onetimeCharges r.onetimeCharges
        <> rateElementMonthly r.monthlyCharge
    SS.RateCardCharge4 r ->
      onetimeCharges r.onetimeCharges
        <> monthlyCharges r.monthlyCharges
    SS.RateCardCharge5 r -> rateElementUsage r.usageCharge
    SS.RateCardCharge6 r -> usageCharges r.usageCharges

  rateCard :: SS.RateCard -> H.ComponentHTML Action slots m
  rateCard = case _ of
    SS.RateCardPath p -> HH.li_ [ HH.text p ]
    SS.RateCard r ->
      HH.li_
        ( dataItemRaw "SKU" (sku r.sku)
            <> opt (dataItem "Name") r.name
            <> dataItem "Currency" (currency r.currency)
            <> rateCardCharge r.charge
        )

  rateCards :: Array SS.RateCard -> H.ComponentHTML Action slots m
  rateCards = blockList <<< map rateCard

  currency :: SS.Currency -> String
  currency (SS.Currency c) =
    c.code
      <> maybe "" (\c' -> "\"" <> c' <> "\"") c.country

  price :: SS.Price -> H.ComponentHTML Action slots m
  price (SS.Price p) =
    HH.li_
      [ HH.dl_
          ( dataItem "Name" p.name
              <> dataItem "Currency" (currency p.currency)
              <> opt (dataItemRaw "Rate Cards" <<< rateCards) p.rateCards
          )
      ]

  error err =
    [ HH.div [ HP.class_ Css.card ]
        [ HH.header_
            [ HH.h3_ [ HH.text "Error" ]
            ]
        , HH.footer_
            [ HH.text err
            ]
        ]
    ]

  loading = [ HH.p_ [ HH.text "Loading …" ] ]

  defRender ::
    forall a.
    SubState a ->
    (a -> Array (H.ComponentHTML Action slots m)) ->
    Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> []
    Loading -> loading
    Success dat -> rend dat
    Error err -> error err

  solution :: Int -> StateSolution -> H.ComponentHTML Action slots m
  solution iSol sol =
    HH.li_
      $ [ HH.button [ HE.onClick \_ -> ToggleSolution iSol ] [ HH.text sol.name ]
        ]
      <> if sol.expanded then defRender sol.solution exp else []
    where
    exp (SS.Solution s) =
      [ HH.h4_ [ HH.text "Description" ]
      , HH.p_ [ HH.text s.description ]
      ]
        <> [ HH.details_
              [ HH.summary [ HP.class_ Css.button ] [ HH.text "Dimension Types" ]
              , blockList (map dimType s.dimTypes)
              ]
          ]
        <> [ HH.details_
              [ HH.summary [ HP.class_ Css.button ] [ HH.text "Products" ]
              , blockList (map product s.products)
              ]
          ]
        <> [ HH.details_
              [ HH.summary [ HP.class_ Css.button ] [ HH.text "Prices" ]
              , blockList (map price s.prices)
              ]
          ]
        <> [ HH.details_
              [ HH.summary [ HP.class_ Css.button ] [ HH.text "Billing Units" ]
              , blockList (map billingUnit s.billingUnits)
              ]
          ]

  meta m =
    [ HH.h2_ [ HH.text "Solutions" ]
    , blockList (mapWithIndex (\i -> solution i) m.solutions)
    ]

  content = defRender state meta

baseUrl :: String
baseUrl = "v1alpha1/solutions"

metaUrl :: String
metaUrl = baseUrl <> "/meta.json"

solutionUrl :: String -> String
solutionUrl solName = baseUrl <> "/" <> solName <> "/nsolution.json"

-- | Modifies the solution at the given index.
modifySolution_ :: forall m. MonadState State m => Int -> (StateSolution -> StateSolution) -> m Unit
modifySolution_ id f =
  H.modify_
    ( case _ of
        Success st ->
          Success
            ( st
                { solutions = maybe st.solutions identity $ modifyAt id f st.solutions
                }
            )
        o -> o
    )

getSolution :: forall m. MonadState State m => Int -> m (Maybe StateSolution)
getSolution iSol =
  H.gets
    ( case _ of
        Success meta -> meta.solutions !! iSol
        _ -> Nothing
    )

handleAction ::
  forall o m.
  MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  GetMeta -> do
    H.modify_ \_ -> Loading
    res <-
      getJson metaUrl
        ( \meta ->
            { meta
            , solutions: map (\x -> { name: x, expanded: false, solution: Idle }) meta.solutions
            }
        )
    H.modify_ \_ -> res
  ToggleSolution iSol -> do
    msolution <- getSolution iSol
    case msolution of
      Just solution ->
        if solution.expanded then
          modifySolution_ iSol $ _ { expanded = false }
        else do
          modifySolution_ iSol $ _ { solution = Loading }
          res <- getJson (solutionUrl solution.name) identity
          modifySolution_ iSol $ _ { expanded = true, solution = res }
      _ -> pure unit

getJson ::
  forall m a b.
  Bind m => MonadAff m => DecodeJson a => String -> (a -> b) -> m (SubState b)
getJson url handle = do
  res <- H.liftAff (AJX.get url)
  case res of
    Left error ->
      let
        errorStr =
          default "Generic error"
            # on _affjaxError printError
            # on _notFound (const "Not found")
            # on _parseError printJsonDecodeError
            $ error
      in
        pure $ Error errorStr
    Right content -> pure $ Success $ handle content
