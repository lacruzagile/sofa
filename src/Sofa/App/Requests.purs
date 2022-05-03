-- | Contains various AJAX requests.
module Sofa.App.Requests
  ( FileStatus(..)
  , appendPathPiece
  , deleteFile
  , deleteOrder
  , deleteOrderLine
  , deleteOrderNote
  , deleteOrderObserver
  , deleteOrderSection
  , getBillingAccount
  , getBillingAccounts
  , getBuyer
  , getBuyerContacts
  , getBuyers
  , getDataSourceEnum
  , getFileContent
  , getFileMetadata
  , getLegalEntities
  , getOrder
  , getOrderForQuote
  , getOrders
  , getProductCatalog
  , patchOrder
  , patchOrderNote
  , patchOrderObserver
  , postFile
  , postOrder
  , postOrderFulfillment
  , postOrderNote
  , postOrderObserver
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import JSURI (encodeURIComponent)
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..), deleteR_, getJson, getRJson, patchRJson, postRJson, postRJson_)
import Sofa.Data.SmartSpec (BillingAccount, BillingAccountId(..), Buyer, ConfigValue, Contact, CrmAccountId(..), CrmQuoteId(..), LegalEntity, OrderForm, OrderId, OrderLineId, OrderNote, OrderNoteId, OrderObserver, OrderObserverId, OrderSectionId, ProductCatalog, Uri)

-- | Base URL to use for the ordering service.
foreign import orderingBaseUrl :: String

-- | Base URL to use for the Smart Spec repository.
foreign import smartSpecBaseUrl :: String

-- | Name of the Smart Spec product catalog file that should be loaded.
foreign import smartSpecProdCatalogFilename :: String

filesUrl :: String
filesUrl = orderingBaseUrl </> "v1alpha1" </> "files"

ordersUrl :: String
ordersUrl = orderingBaseUrl </> "v1alpha1" </> "orders"

crmQuoteOrdersUrl :: String
crmQuoteOrdersUrl = orderingBaseUrl </> "v1alpha1" </> "crm-quote-orders"

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

getOrders ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Maybe String ->
  m (Loadable { orders :: Array OrderForm, nextPageToken :: Maybe String })
getOrders nextPageToken = filterNextPageToken <$> getRJson url
  where
  url = ordersUrl <> "?pageSize=10" <> pageToken

  pageToken =
    fromMaybe "" do
      tok <- nextPageToken
      tokParam <- encodeURIComponent tok
      pure $ "&pageToken=" <> tokParam

  -- If the next page token is an empty string then we'll change it to a Nothing
  -- value.
  filterNextPageToken = case _ of
    Loaded r ->
      Loaded
        $ r
            { nextPageToken =
              case r.nextPageToken of
                Nothing -> Nothing
                Just "" -> Nothing
                t -> t
            }
    r -> r

getOrder :: forall m. MonadAff m => CredentialStore m => OrderId -> m (Loadable OrderForm)
getOrder orderId = getRJson url
  where
  url = ordersUrl </> show orderId

getOrderForQuote ::
  forall m.
  MonadAff m =>
  CredentialStore m => CrmQuoteId -> m (Loadable OrderForm)
getOrderForQuote (CrmQuoteId quoteId) = getRJson url
  where
  url = crmQuoteOrdersUrl </> quoteId

patchOrder :: forall m. MonadAff m => CredentialStore m => OrderId -> OrderForm -> m (Loadable OrderForm)
patchOrder orderId orderForm = patchRJson url orderForm
  where
  url = ordersUrl </> show orderId

postOrder :: forall m. MonadAff m => CredentialStore m => OrderForm -> m (Loadable OrderForm)
postOrder orderForm = postRJson ordersUrl orderForm

deleteOrder :: forall m. MonadAff m => CredentialStore m => OrderId -> m (Loadable Unit)
deleteOrder orderId = deleteR_ url
  where
  url = ordersUrl </> show orderId

deleteOrderSection ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderSectionId ->
  m (Loadable Unit)
deleteOrderSection orderId sectionId = deleteR_ url
  where
  url = ordersUrl </> show orderId </> "order-sections" </> show sectionId

deleteOrderLine ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  OrderId ->
  OrderSectionId ->
  OrderLineId ->
  m (Loadable Unit)
deleteOrderLine orderId sectionId lineId = deleteR_ url
  where
  url =
    ordersUrl
      </> show orderId
      </> "order-sections"
      </> show sectionId
      </> "order-lines"
      </> show lineId

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

type FileId
  = String

type FileBody
  = { file :: String
    , metadata ::
        { fileName :: String
        , "type" :: Maybe String
        }
    , orderLineId :: OrderLineId
    }

data FileStatus
  = FsPending
  | FsInProgress
  | FsSuccess
  | FsFailed

derive instance eqFileStatus :: Eq FileStatus

instance showFileStatus :: Show FileStatus where
  show = case _ of
    FsPending -> "PENDING"
    FsInProgress -> "IN_PROGRESS"
    FsSuccess -> "SUCCESS"
    FsFailed -> "FAILED"

instance decodeJsonFileStatus :: DecodeJson FileStatus where
  decodeJson json = do
    string <- decodeJson json
    case string of
      "PENDING" -> Right FsPending
      "IN_PROGRESS" -> Right FsInProgress
      "SUCCESS" -> Right FsSuccess
      "FAILED" -> Right FsFailed
      _ -> Left (TypeMismatch "FileStatus")

type FileRsp
  = { fileId :: String
    , fileName :: String
    , fileSize :: Int
    , status :: Maybe FileStatus
    , "type" :: String
    }

getFileContent ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  FileId ->
  m (Loadable String)
getFileContent fileId = map conv <$> getRJson url
  where
  url = filesUrl </> fileId </> "download"

  conv :: { file :: String } -> String
  conv { file } = file

getFileMetadata ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  FileId ->
  m (Loadable FileRsp)
getFileMetadata fileId = getRJson url
  where
  url = filesUrl </> fileId </> "metadata"

postFile ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  FileBody ->
  m (Loadable FileRsp)
postFile = postRJson url
  where
  url = filesUrl </> "upload"

deleteFile ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  FileId ->
  m (Loadable Unit)
deleteFile fileId = deleteR_ url
  where
  url = filesUrl </> fileId
