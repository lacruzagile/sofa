module App.SolVis (Slot, proxy, component) where

import Prelude
import Affjax (printError)
import Control.Monad.State (class MonadState)
import Css as Css
import Data.Argonaut (class DecodeJson, printJsonDecodeError, stringifyWithIndent)
import Data.Array (mapWithIndex, modifyAt, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.SmartSpec as SS
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

proxy :: Proxy "solvis"
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
    , products :: Array StateProduct
    }

type StateProduct
  = { name :: String
    , expanded :: Boolean
    , product :: SubState SS.Product
    }

type SolWithProd
  = { solution :: StateSolution
    , product :: StateProduct
    }

data Action
  = GetMeta
  | ToggleSolution Int
  | ToggleProduct Int Int

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

  defRender :: forall a. SubState a -> (a -> Array (H.ComponentHTML Action slots m)) -> Array (H.ComponentHTML Action slots m)
  defRender s rend = case s of
    Idle -> []
    Loading -> loading
    Success dat -> rend dat
    Error err -> error err

  dataItem label value =
    [ HH.dt_ [ HH.text label ]
    , HH.dd_ [ HH.text value ]
    ]

  dataItem' label =
    maybe [] \v ->
      [ HH.dt_ [ HH.text label ]
      , HH.dd_ [ HH.text v ]
      ]

  dataItemCode' label =
    maybe [] \v ->
      [ HH.dt_ [ HH.text label ]
      , HH.dd_ [ HH.pre_ [ HH.code_ [ HH.text v ] ] ]
      ]

  billingUnit :: SS.BillingUnit -> H.ComponentHTML Action slots m
  billingUnit bu =
    HH.li [ HP.class_ Css.hblock ]
      [ HH.dl_
          ( dataItem' "ID" bu.id
              <> dataItem' "Name" bu.name
              <> dataItem "Charge Type" (show bu.chargeType)
              <> dataItem' "Description" bu.description
          )
      ]

  dimType :: SS.DimType -> H.ComponentHTML Action slots m
  dimType dt =
    HH.li [ HP.class_ Css.hblock ]
      [ HH.dl_
          ( dataItem' "ID" dt.id
              <> dataItem' "Name" dt.name
              <> dataItem' "Description" dt.description
              <> dataItemCode' "Schema" (stringifyWithIndent 2 <$> dt.schema)
          )
      ]

  product :: Int -> Int -> StateProduct -> H.ComponentHTML Action slots m
  product iSol iProd prod =
    HH.li [ HP.class_ Css.hblock ]
      $ [ HH.button [ HE.onClick \_ -> ToggleProduct iSol iProd ] [ HH.text prod.name ]
        ]
      <> if prod.expanded then defRender prod.product exp else []
    where
    exp p =
      [ HH.dl_
          ( dataItem "SKU" p.sku
              <> dataItem "Description" p.description
          )
      ]

  price :: SS.Price -> H.ComponentHTML Action slots m
  price p =
    HH.li [ HP.class_ Css.hblock ]
      [ HH.dl_
          ( dataItem "Name" p.name
              <> dataItem "Currency" p.currency.code
          )
      ]

  solution :: Int -> StateSolution -> H.ComponentHTML Action slots m
  solution iSol sol =
    HH.li [ HP.class_ Css.hblock ]
      $ [ HH.button [ HE.onClick \_ -> ToggleSolution iSol ] [ HH.text sol.name ]
        ]
      <> if sol.expanded then defRender sol.solution exp else []
    where
    exp s =
      [ HH.h2_ [ HH.text "Description" ]
      , HH.p_ [ HH.text s.description ]
      ]
        <> [ HH.h2_ [ HH.text "Dimension Types" ] ]
        <> [ HH.ul_ (map dimType s.dimTypes) ]
        <> [ HH.h2_ [ HH.text "Products" ] ]
        <> [ HH.ul_ (mapWithIndex (\iProd -> product iSol iProd) sol.products) ]
        <> [ HH.h2_ [ HH.text "Prices" ] ]
        <> [ HH.ul_ (map price s.prices) ]
        <> [ HH.h2_ [ HH.text "Billing Units" ] ]
        <> [ HH.ul_ (map billingUnit s.billingUnits) ]

  meta m =
    [ HH.h2_ [ HH.text "Solutions" ]
    , HH.ul_ (mapWithIndex (\i -> solution i) m.solutions)
    ]

  content = defRender state meta

baseUrl :: String
baseUrl = "v1alpha1/solutions"

metaUrl :: String
metaUrl = baseUrl <> "/meta.json"

-- url :: String
-- url = "v1alpha1/solutions/example-messaging-sms/solution.json"
solutionUrl :: String -> String
solutionUrl solName = baseUrl <> "/" <> solName <> "/solution.json"

productUrl :: String -> String -> String
productUrl solName prodName = baseUrl <> "/" <> solName <> "/products/" <> prodName <> ".json"

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

-- | Modifies the product at the given index.
modifyProduct_ :: forall m. MonadState State m => Int -> Int -> (StateProduct -> StateProduct) -> m Unit
modifyProduct_ iSol iProd f =
  modifySolution_ iSol \sol ->
    sol
      { products =
        maybe sol.products identity $ modifyAt iProd f $ sol.products
      }

getProduct :: forall m. MonadState State m => Int -> Int -> m (Maybe SolWithProd)
getProduct iSol iProd = do
  msol <- getSolution iSol
  pure do
    solution <- msol
    product <- solution.products !! iProd
    pure $ { solution, product }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  GetMeta -> do
    H.modify_ \_ -> Loading
    res <-
      getJson metaUrl
        ( \meta ->
            { meta
            , solutions: map (\x -> { name: x, expanded: false, solution: Idle, products: [] }) meta.solutions
            }
        )
    H.modify_ \_ -> res
  ToggleSolution iSol -> do
    msolution <- getSolution iSol
    case msolution of
      Just solution ->
        if solution.expanded then
          modifySolution_ iSol \sol -> sol { expanded = false }
        else do
          modifySolution_ iSol \sol -> sol { solution = Loading }
          res <- getJson (solutionUrl solution.name) (\sol -> sol)
          modifySolution_ iSol \sol ->
            sol
              { expanded = true
              , solution = res
              , products =
                case res of
                  Success r -> map (\x -> { name: x, expanded: false, product: Idle }) r.products
                  _ -> []
              }
      _ -> pure unit
  ToggleProduct iSol iProd -> do
    mproduction <- getProduct iSol iProd
    case mproduction of
      Just product ->
        if product.product.expanded then
          modifyProduct_ iSol iProd \prod -> prod { expanded = false }
        else do
          modifyProduct_ iSol iProd \prod -> prod { product = Loading }
          res <- getJson (productUrl product.solution.name product.product.name) (\prod -> prod)
          modifyProduct_ iSol iProd \prod ->
            prod
              { expanded = true
              , product = res
              }
      _ -> pure unit

getJson :: forall m a b. Bind m => MonadAff m => DecodeJson a => String -> (a -> b) -> m (SubState b)
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
