module App.SolVis (Slot, proxy, component) where

import Prelude
import Css as Css
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.SmartSpec as SS
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.Ajax as AJX
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "solvis"
proxy = Proxy

data State
  = Initial
  | Loading
  | Solution SS.Solution
  | Error String

data Action
  = GetSolution

component ::
  forall query input output m.
  MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just GetSolution }
    }

initialState :: forall input. input -> State
initialState _ = Initial

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

  solution sol =
    [ HH.h2_ [ HH.text "Description" ]
    , HH.p_ [ HH.text sol.description ]
    ]
      <> [ HH.h2_ [ HH.text "Products" ] ]
      <> map (\prod -> HH.p_ [ HH.text prod ]) sol.products
      <> [ HH.h2_ [ HH.text "Prices" ] ]
      <> map (\price -> HH.p_ [ HH.text price.name ]) sol.prices
      <> [ HH.h2_ [ HH.text "Billing Units" ] ]
      <> map (\bu -> HH.p_ [ HH.text bu.id ]) sol.billingUnits

  content = case state of
    Initial -> [ HH.p_ [ HH.text "No solution loaded yet" ] ]
    Loading -> [ HH.p_ [ HH.text "Loading …" ] ]
    Solution sol -> solution sol
    Error err -> error err

url :: String
url = "v1alpha1/solutions/example-messaging-sms/solution.json"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  GetSolution -> do
    H.modify_ (const Loading)
    res <- H.liftAff (AJX.get url)
    case res of
      Left _ -> H.modify_ $ \_ -> Error "Could not fetch solution"
      Right (sol :: SS.Solution) -> H.modify_ $ \_ -> Solution sol
