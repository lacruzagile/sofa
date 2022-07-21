-- | Contains various AJAX requests.
module Sofa.App.Requests
  ( FileStatus(..)
  , appendPathPiece
  , appendUrlParams
  , deleteFile
  , deleteOrder
  , deleteOrderLine
  , deleteOrderNote
  , deleteOrderObserver
  , deleteOrderSection
  , getAsset
  , getBillingAccount
  , getBillingAccounts
  , getBuyer
  , getBuyerContacts
  , getBuyers
  , getDataSourceEnum
  , getFileContent
  , getFileMetadata
  , getLegalEntities
  , getLegalEntity
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
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Alternative ((<|>))
import Data.Argonaut (JsonDecodeError(..), (.:), class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError)
import Data.Array as A
import Data.Either (Either(..))
import Data.HTTP.Method as HTTP
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object as FO
import JSURI (encodeURIComponent)
import Sofa.App.OrderForm.ConfirmFulfillModal (MarioPriority(..))
import Sofa.Data.Auth (class CredentialStore, getAuthorizationHeader)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.SmartSpec (AssetConfig, BillingAccount, BillingAccountId(..), Buyer, ConfigValue, Contact, CrmAccountId(..), CrmQuoteId(..), LegalEntity(..), OrderForm, OrderId, OrderLineId, OrderNote, OrderNoteId, OrderObserver, OrderObserverId, OrderSectionId, ProductCatalog, Uri)
import Web.URL.URLSearchParams (URLSearchParams)
import Web.URL.URLSearchParams as UrlParams

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

appendUrlParams :: String -> URLSearchParams -> String
appendUrlParams a params = case UrlParams.toString params of
  "" -> a
  paramsStr -> a <> "?" <> paramsStr

emptyUrlParams :: UrlParams.URLSearchParams
emptyUrlParams = UrlParams.fromString ""

infixr 5 appendPathPiece as </>

infixr 5 appendUrlParams as <?>

-- | Fetches buyers that match the given query string.
getBuyers ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  String -> m (Loadable (Array Buyer))
getBuyers query =
  map (map conv) $ getRJson
    $ buyersUrl
    <?> ( UrlParams.set "pageSize" "5"
          $ UrlParams.set "corporateName" query
          $ emptyUrlParams
      )
  where
  conv :: { buyers :: Array Buyer } -> Array Buyer
  conv { buyers } = buyers

-- | Fetches a specific buyer that match the given CRM account ID.
getBuyer ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  CrmAccountId -> m (Loadable Buyer)
getBuyer (CrmAccountId id) = getRJson $ buyersUrl </> idEncoded
  where
  idEncoded = fromMaybe "" $ encodeURIComponent id

-- | Fetches a specific asset that match the given order ID.
getAsset ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId -> m (Loadable (Array AssetConfig))
getAsset id = map conv <$> getRJson (ordersUrl </> idEncoded </> "assets")
  where
  idEncoded = fromMaybe "" $ encodeURIComponent (show id)

  conv :: { assets :: Array AssetConfig } -> Array AssetConfig
  conv { assets } = assets

getBillingAccounts ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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
  url = smartSpecBaseUrl </> "examples" </> "legalentities.json"

  conv :: { legalEntities :: Array LegalEntity } -> Array LegalEntity
  conv { legalEntities } = legalEntities

-- | Fetch the legal entity with the given registered name.
getLegalEntity :: forall m. MonadAff m => String -> m (Loadable LegalEntity)
getLegalEntity registeredName = do
  lLegalEntities <- getLegalEntities
  let
    isMatch (LegalEntity le) = le.registeredName == registeredName
  pure do
    legalEntities <- lLegalEntities
    case A.find isMatch legalEntities of
      Nothing -> Error $ "Legal entity not found: " <> registeredName
      Just legalEntity -> Loaded legalEntity

getOrders ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  Maybe String ->
  Maybe CrmAccountId ->
  m (Loadable { orders :: Array OrderForm, nextPageToken :: Maybe String })
getOrders nextPageToken crmAccountId = filterNextPageToken <$> getRJson url
  where
  url =
    ordersUrl
      <?> ( UrlParams.set "pageSize" "10"
            $ UrlParams.set "pageToken" (fromMaybe "" nextPageToken)
            $ setCrmAccountParam
            $ emptyUrlParams
        )

  setCrmAccountParam = case crmAccountId of
    Nothing -> identity
    Just (CrmAccountId id) -> UrlParams.set "crmAccountId" id

  -- If the next page token is an empty string then we'll change it to a Nothing
  -- value to indicate that there are no more pages.
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

getOrder ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId -> m (Loadable OrderForm)
getOrder orderId = getRJson url
  where
  url = ordersUrl </> show orderId

getOrderForQuote ::
  forall f m.
  MonadAff m =>
  CredentialStore f m => CrmQuoteId -> m (Loadable OrderForm)
getOrderForQuote (CrmQuoteId quoteId) = getRJson url
  where
  url = crmQuoteOrdersUrl </> quoteId

patchOrder ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId -> OrderForm -> m (Loadable OrderForm)
patchOrder orderId orderForm = patchRJson url orderForm
  where
  url = ordersUrl </> show orderId

postOrder ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderForm -> m (Loadable OrderForm)
postOrder orderForm = postRJson ordersUrl orderForm

deleteOrder ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId -> m (Loadable Unit)
deleteOrder orderId = deleteR_ url
  where
  url = ordersUrl </> show orderId

deleteOrderSection ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId ->
  OrderSectionId ->
  m (Loadable Unit)
deleteOrderSection orderId sectionId = deleteR_ url
  where
  url = ordersUrl </> show orderId </> "order-sections" </> show sectionId

deleteOrderLine ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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

postOrderFulfillment ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId -> Maybe MarioPriority -> Maybe String -> m (Loadable OrderForm)
postOrderFulfillment orderId mMarioPrio mMarioNote = postRJson_ url
  where
  url = ordersUrl </> show orderId <> ":fulfillment"
    <?> (setMarioPrioParam
            $ setMarioNoteParam
            $ emptyUrlParams
            )

  setMarioPrioParam = case mMarioPrio of
    Nothing -> identity
    Just p -> UrlParams.set "marioPriority" (prioValue p)

  setMarioNoteParam = case mMarioNote of
    Nothing -> identity
    Just p -> UrlParams.set "marioNote" p

  prioValue :: MarioPriority -> String
  prioValue = case _ of
    MarioPrioCritical -> "1"
    MarioPrioHigh -> "2"
    MarioPrioMedium -> "3"
    MarioPrioLow -> "4"

-- | Creates a new order note.
postOrderNote ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId ->
  OrderNote ->
  m (Loadable OrderNote)
postOrderNote orderId = postRJson url
  where
  url = ordersUrl </> show orderId </> "order-notes"

-- | Deletes an existing order note.
deleteOrderNote ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId ->
  OrderNoteId ->
  m (Loadable Unit)
deleteOrderNote orderId noteId = deleteR_ url
  where
  url = ordersUrl </> show orderId </> "order-notes" </> show noteId

-- | Updates an existing order.
patchOrderNote ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId ->
  OrderNoteId ->
  OrderNote ->
  m (Loadable OrderNote)
patchOrderNote orderId noteId = patchRJson url
  where
  url = ordersUrl </> show orderId </> "order-notes" </> show noteId

-- | Creates a new order observer.
postOrderObserver ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId ->
  OrderObserver ->
  m (Loadable OrderObserver)
postOrderObserver orderId = postRJson url
  where
  url = ordersUrl </> show orderId </> "order-observers"

-- | Deletes an existing order observer.
deleteOrderObserver ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  OrderId ->
  OrderObserverId ->
  m (Loadable Unit)
deleteOrderObserver orderId observerId = deleteR_ url
  where
  url = ordersUrl </> show orderId </> "order-observers" </> show observerId

-- | Updates an existing order.
patchOrderObserver ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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
  url = smartSpecBaseUrl </> "examples" </> smartSpecProdCatalogFilename

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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
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
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  FileId ->
  m (Loadable String)
getFileContent fileId = map conv <$> getRJson url
  where
  url = filesUrl </> fileId </> "download"

  conv :: { file :: String } -> String
  conv { file } = file

getFileMetadata ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  FileId ->
  m (Loadable FileRsp)
getFileMetadata fileId = getRJson url
  where
  url = filesUrl </> fileId </> "metadata"

postFile ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  FileBody ->
  m (Loadable FileRsp)
postFile = postRJson url
  where
  url = filesUrl </> "upload"

deleteFile ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  FileId ->
  m (Loadable Unit)
deleteFile fileId = deleteR_ url
  where
  url = filesUrl </> fileId

handleResponse ::
  forall result body r.
  (body -> Loadable result) ->
  (body -> String) ->
  Either AX.Error { body :: body, status :: StatusCode | r } ->
  Loadable result
handleResponse handleBody decodeError = case _ of
  Left err -> Error $ AX.printError err
  Right resp
    | statusOk resp.status -> handleBody resp.body
    | statusNotFound resp.status -> Error "Not found"
    | otherwise -> Error $ decodeError resp.body
  where
  statusOk (StatusCode n) = 200 <= n && n < 300

  statusNotFound (StatusCode n) = n == 404

handleEmptyResponse :: Either AX.Error (AX.Response Unit) -> Loadable Unit
handleEmptyResponse =
  handleResponse
    (\_ -> Loaded unit)
    (\_ -> "Generic error")

handleJsonResponse ::
  forall a.
  DecodeJson a =>
  Either AX.Error (AX.Response Json) -> Loadable a
handleJsonResponse = handleResponse decodeSuccessBody decodeErrorBody
  where
  decodeSuccessBody body = case decodeJson body of
    Left err -> Error $ printJsonDecodeError err
    Right value -> Loaded value

  -- Try to extract the error message. Note, if the message exists but is an
  -- empty string then we'll ignore it.
  decodeErrorBody body = case decodeJson body of
    Right ({ message } :: { message :: String })
      | not (S.null message) -> message
    _ -> "Generic error"

withAuthorizationHeader ::
  forall f m a.
  MonadAff m =>
  CredentialStore f m =>
  (RequestHeader -> Aff (Loadable a)) -> m (Loadable a)
withAuthorizationHeader fetcher = do
  creds <- getAuthorizationHeader
  case creds of
    Left err -> pure $ Error err
    Right authHdr -> liftAff $ fetcher authHdr

-- | Fetch JSON from an URL.
getJson :: forall a m. DecodeJson a => MonadAff m => String -> m (Loadable a)
getJson url =
  liftAff
    $ map handleJsonResponse
    $ AX.request
    $ AX.defaultRequest
        { url = url
        , method = Left HTTP.GET
        , timeout = Just (Milliseconds 10_000.0)
        , responseFormat = ResponseFormat.json
        }

-- | Fetch JSON from an URL.
getRJson ::
  forall a f m.
  DecodeJson a =>
  MonadAff m =>
  CredentialStore f m =>
  String -> m (Loadable a)
getRJson url =
  withAuthorizationHeader \authHdr ->
    map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.GET
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          }

-- | Submit JSON using a PATCH request and parse the response.
patchRJson ::
  forall a b f m.
  EncodeJson a =>
  DecodeJson b =>
  MonadAff m =>
  CredentialStore f m =>
  String ->
  a ->
  m (Loadable b)
patchRJson url body =
  withAuthorizationHeader \authHdr ->
    map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.PATCH
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          , content = Just $ RequestBody.json $ encodeJson body
          }

-- | Submit JSON using a POST request and parse the response.
postRJson ::
  forall a b f m.
  EncodeJson a =>
  DecodeJson b =>
  MonadAff m =>
  CredentialStore f m =>
  String ->
  a ->
  m (Loadable b)
postRJson url body =
  withAuthorizationHeader \authHdr ->
    map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.POST
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          , content = Just $ RequestBody.json $ encodeJson body
          }

-- | Submit an empty POST request and parse the response.
postRJson_ ::
  forall a f m.
  DecodeJson a =>
  MonadAff m =>
  CredentialStore f m =>
  String ->
  m (Loadable a)
postRJson_ url =
  withAuthorizationHeader \authHdr ->
    map handleJsonResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.POST
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          , responseFormat = ResponseFormat.json
          }

-- | Submit a DELETE request.
deleteR_ ::
  forall f m.
  MonadAff m =>
  CredentialStore f m =>
  String ->
  m (Loadable Unit)
deleteR_ url =
  withAuthorizationHeader \authHdr ->
    map handleEmptyResponse
      $ AX.request
      $ AX.defaultRequest
          { url = url
          , method = Left HTTP.DELETE
          , headers = [ authHdr ]
          , timeout = Just (Milliseconds 10_000.0)
          }
