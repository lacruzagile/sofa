module HtmlUtils (focusElementByQuery) where

import Prelude
import Data.Maybe (maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement as HtmlElement

focusElementByQuery :: forall m. Bind m => MonadAff m => String -> m Unit
focusElementByQuery query = do
  element <- H.liftAff $ HA.selectElement (QuerySelector query)
  H.liftEffect $ maybe (pure unit) HtmlElement.focus element
