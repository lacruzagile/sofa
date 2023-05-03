module Sofa.HtmlUtils
  ( addClassToElement
  , focusElementByRef
  , scrollToBottom
  , scrollToElement
  , selectInputText
  , setInputText
  , copyToClipboard
  , clearCopyButtonsOnLoadMoreOrders
  , back
  , uncheck
  , uncheckall
  , removeClassToElement
  , reload
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Web.DOM.Element as DomElement
import Web.HTML.HTMLElement as HtmlElement
import Web.HTML.HTMLInputElement as HTMLInputElement

foreign import addClassToElement :: String -> String -> Effect Unit

-- | Move current scroll position to the bottom of the page.
foreign import scrollToBottom :: Effect Unit

foreign import scrollIntoView :: DomElement.Element -> Effect Unit

foreign import clearCopyButtonsOnLoadMoreOrders :: Effect Unit

foreign import copyToClipboard :: String -> Effect Unit

foreign import back :: Effect Unit

foreign import reload :: Effect Unit

foreign import uncheck :: String -> Effect Unit


foreign import uncheckall :: String -> Effect Unit

foreign import removeClassToElement :: String -> String -> Effect Unit

-- | Smoothly scroll to the top of the referenced element.
scrollToElement ::
  forall state action slots output m.
  MonadEffect m =>
  H.RefLabel -> H.HalogenM state action slots output m Unit
scrollToElement ref = do
  element <- H.getHTMLElementRef ref
  for_ element \el ->
    H.liftEffect do
      scrollIntoView (HtmlElement.toElement el)

-- | Places the focus on the element having the given label.
focusElementByRef ::
  forall state action slots output m.
  MonadEffect m =>
  H.RefLabel ->
  H.HalogenM state action slots output m Unit
focusElementByRef ref = do
  element <- H.getHTMLElementRef ref
  for_ element \el ->
    H.liftEffect $ HtmlElement.focus el

-- | Select all text in the input field having the given label.
selectInputText ∷
  forall state action slots output m.
  MonadEffect m =>
  String -> H.HalogenM state action slots output m Unit
selectInputText refLabel = do
  inputElement <- H.getHTMLElementRef $ H.RefLabel refLabel
  for_ inputElement
    $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.select)
    <<< HTMLInputElement.fromHTMLElement

-- | Set the text in the input field having the given label.
setInputText ∷
  forall state action slots output m.
  MonadEffect m =>
  String -> String -> H.HalogenM state action slots output m Unit
setInputText refLabel text = do
  inputElement <- H.getHTMLElementRef $ H.RefLabel refLabel
  for_ inputElement
    $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.setValue text)
    <<< HTMLInputElement.fromHTMLElement
