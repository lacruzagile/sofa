module Sofa.App.OrderForm.SelectOrderStatus (Slot, Output(..), proxy, component) where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Sel
import Select.Setters as SelSet
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "selectOrderStatus"
proxy = Proxy

type Input
  = SS.OrderStatus

type Output
  = SS.OrderStatus

type State
  = ( selected :: SS.OrderStatus
    )

component ::
  forall query m.
  MonadAff m => CredentialStore m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render: \st -> HH.slot selectLabel unit selectComponent st identity
    , eval: H.mkEval H.defaultEval { handleAction = H.raise }
    }
  where
  selectLabel = Proxy :: Proxy "select"

selectComponent ::
  forall query m.
  MonadAff m =>
  H.Component (Sel.Query query ()) Input Output m
selectComponent =
  Sel.component input
    $ Sel.defaultSpec
        { handleEvent = handleEvent
        , render = render
        }
  where
  input :: Input -> Sel.Input State
  input selected =
    { inputType: Sel.Toggle
    , search: Nothing
    , debounceTime: Nothing
    , getItemCount: let n = A.length SS.orderStatuses in const n
    , selected: selected
    }

  handleEvent :: Sel.Event -> H.HalogenM (Sel.State State) _ () Output m Unit
  handleEvent = case _ of
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { selected = fromMaybe st.selected $ SS.orderStatuses !! idx
            , visibility = Sel.Off
            }
      -- Let the parent component know about the new selection.
      H.raise st'.selected
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML _ () m
  render st = HH.span_ [ renderInput, renderResults ]
    where
    colorClass =
      Css.c case st.selected of
        SS.OsInDraft -> "bg-snow-600"
        SS.OsInReview -> "bg-informative-200"
        SS.OsInApproval -> "bg-informative-200"
        SS.OsInSignature -> "bg-informative-200"
        SS.OsInConfiguration -> "bg-informative-200"
        SS.OsInFulfillment -> "bg-informative-200"
        SS.OsFulfilled -> "bg-success-200"
        SS.OsCancelled -> "bg-warning-200"

    renderInput :: H.ComponentHTML _ () m
    renderInput =
      HH.button
        ( SelSet.setToggleProps
            [ HP.classes
                [ Css.c "nectary-tag"
                , Css.c "nectary-dropdown-icon"
                , colorClass
                , Css.c case st.visibility of
                    Sel.On -> "invisible"
                    Sel.Off -> "visible"
                ]
            , HE.onMouseEnter \_ -> Sel.SetVisibility Sel.On
            ]
        )
        [ HH.text (SS.prettyOrderStatus st.selected) ]

    renderResults :: H.ComponentHTML _ () m
    renderResults
      | st.visibility == Sel.Off = HH.text ""
      | otherwise =
        HH.div
          ( SelSet.setContainerProps
              [ HP.classes containerClasses
              , HE.onMouseLeave \_ -> Sel.SetVisibility Sel.Off
              ]
          )
          $ A.mapWithIndex renderItem SS.orderStatuses
        where
        containerClasses =
          [ Css.c "absolute"
          , Css.c "-mt-6"
          , Css.c "flex"
          , Css.c "flex-col"
          , Css.c "bg-white"
          , Css.c "text-xs"
          , Css.c "overflow-auto"
          , Css.c "border"
          , Css.c "rounded-sm"
          , Css.c "divide-y"
          ]

    renderItem idx status =
      HH.div
        ( SelSet.setItemProps idx
            [ HP.classes $ itemClasses <> selectedClasses <> highlightClasses
            ]
        )
        (renderOrderStatus status)
      where
      itemClasses = [ Css.c "p-2", Css.c "pr-8" ]

      highlightClasses
        | st.highlightedIndex == Just idx = [ Css.c "bg-snow-500" ]
        | otherwise = []

      selectedClasses
        | status == st.selected = [ Css.c "nectary-icon-check" ]
        | otherwise = []

    renderOrderStatus status = [ HH.text (SS.prettyOrderStatus status) ]
