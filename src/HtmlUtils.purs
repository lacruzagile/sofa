module HtmlUtils
  ( focusElementByRef
  , focusElementByQuery
  , scrollToBottom
  , selectInputText
  , setInputText
  ) where

import Prelude
import Data.Maybe (maybe)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement as HtmlElement
import Web.HTML.HTMLInputElement as HTMLInputElement

foreign import scrollToBottom :: Effect Unit

focusElementByRef ::
  forall state action slots output m.
  MonadEffect m =>
  H.RefLabel ->
  H.HalogenM state action slots output m Unit
focusElementByRef ref = do
  element <- H.getHTMLElementRef ref
  for_ element \el ->
    H.liftEffect $ HtmlElement.focus el

focusElementByQuery :: forall m. Bind m => MonadAff m => String -> m Unit
focusElementByQuery query = do
  element <- H.liftAff $ HA.selectElement (QuerySelector query)
  H.liftEffect $ maybe (pure unit) HtmlElement.focus element

selectInputText ∷
  forall state action slots output m.
  MonadEffect m =>
  String -> H.HalogenM state action slots output m Unit
selectInputText refLabel = do
  inputElement <- H.getHTMLElementRef $ H.RefLabel refLabel
  for_ inputElement
    $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.select)
    <<< HTMLInputElement.fromHTMLElement

setInputText ∷
  forall state action slots output m.
  MonadEffect m =>
  String -> String -> H.HalogenM state action slots output m Unit
setInputText refLabel text = do
  inputElement <- H.getHTMLElementRef $ H.RefLabel refLabel
  for_ inputElement
    $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.setValue text)
    <<< HTMLInputElement.fromHTMLElement
