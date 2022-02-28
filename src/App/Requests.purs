-- | Contains various AJAX requests.
module App.Requests
  ( appendPathPiece
  , getBillingAccount
  , getBillingAccounts
  , getBuyer
  , getBuyerContacts
  , getBuyers
  , getDataSourceEnum
  , getLegalEntities
  , getOrder
  , getOrders
  , getProductCatalog
  , patchOrder
  , postOrder
  , postOrderFulfillment
  -- * Order Notes
  , deleteOrderNote
  , patchOrderNote
  , postOrderNote
  -- * Order Observers
  , deleteOrderObserver
  , patchOrderObserver
  , postOrderObserver
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Array as A
import Data.Auth (class CredentialStore)
import Data.Loadable (Loadable, deleteR_, getJson, getRJson, patchRJson, postRJson, postRJson_)
import Data.Maybe (fromMaybe)
import Data.SmartSpec (BillingAccount, BillingAccountId(..), Buyer, ConfigValue, Contact, CrmAccountId(..), LegalEntity, OrderForm(..), OrderId, OrderNote, OrderNoteId, OrderObserver, OrderObserverId, ProductCatalog, Uri)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import JSURI (encodeURIComponent)

-- | Base URL to use for the ordering service.
foreign import orderingBaseUrl :: String

-- | Base URL to use for the Smart Spec repository.
foreign import smartSpecBaseUrl :: String

-- | Name of the Smart Spec product catalog file that should be loaded.
foreign import smartSpecProdCatalogFilename :: String

ordersUrl :: String
ordersUrl = orderingBaseUrl </> "v1alpha1" </> "orders"

buyersUrl :: String
buyersUrl = orderingBaseUrl </> "v1alpha1" </> "buyers"

appendPathPiece :: String -> String -> String
appendPathPiece a b = a <> "/" <> b

infixr 5 appendPathPiece as </>

-- | Fetches buyers that match the given query string.
getBuyers :: forall m. MonadAff m => CredentialStore m => String -> m (Loadable (Array Buyer))
getBuyers query =
  map (map conv) $ getRJson
    $ buyersUrl
    <> "?pageSize=5&corporateName="
    <> encodedQuery
  where
  encodedQuery = fromMaybe "" $ encodeURIComponent query

  conv :: { buyers :: Array Buyer } -> Array Buyer
  conv { buyers } = buyers

-- | Fetches a specific buyer that match the given CRM account ID.
getBuyer :: forall m. MonadAff m => CredentialStore m => CrmAccountId -> m (Loadable Buyer)
getBuyer (CrmAccountId id) = getRJson $ buyersUrl </> idEncoded
  where
  idEncoded = fromMaybe "" $ encodeURIComponent id

getBillingAccounts ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  CrmAccountId ->
  m (Loadable (Array BillingAccount))
getBillingAccounts (CrmAccountId id) =
  map (map conv)
    $ getRJson
    $ buyersUrl
    </> idEncoded
    </> "billing-accounts"
  where
  idEncoded = fromMaybe "" $ encodeURIComponent id

  conv :: { billingAccount :: Array BillingAccount } -> Array BillingAccount
  conv { billingAccount } = billingAccount

getBillingAccount ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  CrmAccountId ->
  BillingAccountId ->
  m (Loadable BillingAccount)
getBillingAccount (CrmAccountId aId) (BillingAccountId bId) =
  getRJson
    $ buyersUrl
    </> aIdEncoded
    </> "billing-accounts"
    </> bIdEncoded
  where
  aIdEncoded = fromMaybe "" $ encodeURIComponent aId

  bIdEncoded = fromMaybe "" $ encodeURIComponent bId

getBuyerContacts ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  CrmAccountId ->
  m (Loadable (Array Contact))
getBuyerContacts (CrmAccountId id) =
  map (map conv)
    $ getRJson
    $ buyersUrl
    </> idEncoded
    </> "contacts"
  where
  idEncoded = fromMaybe "" $ encodeURIComponent id

  conv :: { contacts :: Array Contact } -> Array Contact
  conv { contacts } = contacts

getLegalEntities :: forall m. MonadAff m => m (Loadable (Array LegalEntity))
getLegalEntities = map (map conv) $ getJson url
  where
  url = smartSpecBaseUrl </> "v1alpha1" </> "examples" </> "legalentities.json"

  conv :: { legalEntities :: Array LegalEntity } -> Array LegalEntity
  conv { legalEntities } = legalEntities

getOrders :: forall m. MonadAff m => CredentialStore m => m (Loadable (Array OrderForm))
getOrders = map (map conv) $ getRJson ordersUrl
  where
  conv :: { orders :: Array OrderForm } -> Array OrderForm
  conv { orders } = A.sortBy (comparing createTime) orders

  createTime (OrderForm { createTime: t }) = t

getOrder :: forall m. MonadAff m => CredentialStore m => OrderId -> m (Loadable OrderForm)
getOrder orderId = getRJson url
  where
  url = ordersUrl </> show orderId

patchOrder :: forall m. MonadAff m => CredentialStore m => OrderId -> OrderForm -> m (Loadable OrderForm)
patchOrder orderId orderForm = patchRJson url orderForm
  where
  url = ordersUrl </> show orderId

postOrder :: forall m. MonadAff m => CredentialStore m => OrderForm -> m (Loadable OrderForm)
postOrder orderForm = postRJson ordersUrl orderForm

postOrderFulfillment :: forall m. MonadAff m => CredentialStore m => OrderId -> m (Loadable OrderForm)
postOrderFulfillment orderId = postRJson_ url
  where
  url = ordersUrl </> show orderId <> ":fulfillment"

-- | Creates a new order note.
postOrderNote ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderNote ->
  m (Loadable OrderNote)
postOrderNote orderId = postRJson url
  where
  url = ordersUrl </> show orderId </> "order-notes"

-- | Deletes an existing order note.
deleteOrderNote ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderNoteId ->
  m (Loadable Unit)
deleteOrderNote orderId noteId = deleteR_ url
  where
  url = ordersUrl </> show orderId </> "order-notes" </> show noteId

-- | Updates an existing order.
patchOrderNote ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderNoteId ->
  OrderNote ->
  m (Loadable OrderNote)
patchOrderNote orderId noteId = patchRJson url
  where
  url = ordersUrl </> show orderId </> "order-notes" </> show noteId

-- | Creates a new order observer.
postOrderObserver ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderObserver ->
  m (Loadable OrderObserver)
postOrderObserver orderId = postRJson url
  where
  url = ordersUrl </> show orderId </> "order-observers"

-- | Deletes an existing order observer.
deleteOrderObserver ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderObserverId ->
  m (Loadable Unit)
deleteOrderObserver orderId observerId = deleteR_ url
  where
  url = ordersUrl </> show orderId </> "order-observers" </> show observerId

-- | Updates an existing order.
patchOrderObserver ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderObserverId ->
  OrderObserver ->
  m (Loadable OrderObserver)
patchOrderObserver orderId observerId = patchRJson url
  where
  url = ordersUrl </> show orderId </> "order-observers" </> show observerId

-- | Fetches the product catalog. Note, we expect a _normalized_ product
-- | catalog.
getProductCatalog :: forall m. MonadAff m => m (Loadable ProductCatalog)
getProductCatalog = getJson url
  where
  -- This URL is "virtual" in the sense that we fetch this URL but the reverse
  -- proxy will redirect the request to the product catalog suitable for the
  -- current deployment.
  url = smartSpecBaseUrl </> "v1alpha1" </> "examples" </> smartSpecProdCatalogFilename

newtype DataSourceResponse
  = DataSourceResponse (FO.Object ConfigValue)

instance decodeJsonDataSourceResponse :: DecodeJson DataSourceResponse where
  -- Temporarily allows plain map or map wrapped in data field.
  decodeJson json = d1 <|> d2
    where
    d1 = do
      o <- decodeJson json
      d <- o .: "data"
      pure $ DataSourceResponse d

    d2 = DataSourceResponse <$> decodeJson json

-- | Fetches data source key/value pairs from the given URL.
getDataSourceEnum ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Uri ->
  Boolean ->
  m (Loadable (Array (Tuple String ConfigValue)))
getDataSourceEnum url authenticate = map parse <$> result
  where
  parse (DataSourceResponse o) = FO.toUnfoldable o

  result
    | authenticate = getRJson url
    | otherwise = getJson url
