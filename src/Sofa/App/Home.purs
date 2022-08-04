module Sofa.App.Home (OrderFilter(..), Slot, proxy, component) where

import Prelude
import Data.String (Pattern(..), Replacement(..), replaceAll) as S
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Sofa.Css as Css
import Sofa.Data.Route as Route
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Void id

proxy :: Proxy "home"
proxy = Proxy

type Input
  = OrderFilter

type State
  = { orderFilter :: OrderFilter }

data OrderFilter
  = HomeAllAccessibleOrder
  | HomeCustomerOrders SS.CrmAccountId


component ::
  forall query output f m.
  MonadEffect m=>
  H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }


initialState :: Input -> State
initialState input =
  {  orderFilter: input}

render ::
  forall action cs m.
  State -> H.ComponentHTML action cs m
render state = do
  HH.div_
    [ HH.h1_
        [ HH.text "SOFA" ]
    , HH.div [ Css.classes [ "bg-white", "p-3" ] ]
        [ HH.p [ Css.class_ "my-2" ]
            [ HH.text "This is a basic tool to experiment with Smart Spec." ]
        , HH.p [ Css.class_ "my-2" ]
            [ HH.text "Currently a product catalog visualizer and order form is available."
            , HH.text " Note, both are work in progress."
            ]
        ]
    ,  renderButton
    
    ]

    where 
    renderButton = case state.orderFilter of
        HomeAllAccessibleOrder ->
                HH.div
                  [ Css.classes
                      [ "my-5"
                      , "flex"
                      , "flex-wrap"
                      , "items-center"
                      , "gap-4"
                      ]
                  ]
                  [ HH.a
                    [ 
                      Route.href Route.Orders
                    , Css.class_ "nectary-btn-primary"
                    ]
                    [ HH.text "Go to order list" ]
                  ]
        HomeCustomerOrders crmAccountId -> 
           HH.div
                  [ Css.classes
                      [ "my-5"
                      , "flex"
                      , "flex-wrap"
                      , "items-center"
                      , "gap-4"
                      ]
                  ]
                  [ HH.a
                    [ 
                      Route.href (Route.OrdersCrmAccountId crmAccountId)
                    , Css.class_ "nectary-btn-primary"
                    ]
                    [ HH.text $ "Go to order list" ]
                  ]
