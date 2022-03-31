module Sofa.App.OrderForm.Widget.FileAttachment (Slot, Output(..), proxy, component) where

import Prelude
import Data.Foldable (for_)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Sofa.App.Requests (deleteFile, getFileMetadata, postFile)
import Sofa.Component.FileUpload (readAsBase64)
import Sofa.Component.FileUpload as FileUpload
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Bytes as Bytes
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File (File)
import Web.File.File as File

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "widgetFileAttachment"
proxy = Proxy

type Slots
  = ( nectaryFileUpload :: FileUpload.Slot Unit )

type Input
  = { orderLineId :: SS.OrderLineId
    , value :: Maybe SS.ConfigValue
    , maxSize :: Maybe Number
    , mediaTypes :: Maybe (Array MediaType)
    }

type Output
  = Maybe SS.ConfigValue

type State
  = { orderLineId :: SS.OrderLineId
    , fileId :: Maybe String
    , maxSize :: Maybe Number
    , mediaTypes :: Maybe (Array MediaType)
    }

data Action
  = Initialize
  | UploadedFile (Maybe File)

component ::
  forall query m.
  MonadAff m => CredentialStore m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

initialState :: Input -> State
initialState input =
  { orderLineId: input.orderLineId
  , fileId:
      let
        fileTag = Just (SS.CvString "FILE_ATTACHMENT")
      in
        case input.value of
          Just (SS.CvObject v)
            | Map.lookup "type" v == fileTag -> case Map.lookup "fileId" v of
              Just (SS.CvString str) -> Just str
              _ -> Nothing
          _ -> Nothing
  , maxSize: input.maxSize
  , mediaTypes: input.mediaTypes
  }

render ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  State -> H.ComponentHTML Action Slots m
render st =
  HH.slot
    FileUpload.proxy
    unit
    FileUpload.component
    ( FileUpload.defaultInput
        { status = st.fileId
        , mediaTypes = st.mediaTypes
        }
    )
    UploadedFile

-- | Deletes the current file from the backend, if any such file exists.
resetFile ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  H.HalogenM State Action Slots Output m (Loadable String)
resetFile = do
  mFileId <- H.gets (_.fileId)
  case mFileId of
    Nothing -> pure Idle
    Just fileId -> do
      let
        mkIdle = case _ of
          Loaded _ -> Idle
          l -> unsafeCoerce l -- Safe since Loaded is handled.
      res <- mkIdle <$> H.lift (deleteFile fileId)
      when (Loadable.isIdle res) do
        H.modify_ _ { fileId = Nothing }
      pure res

handleAction ::
  forall m.
  MonadAff m =>
  CredentialStore m =>
  Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    mFileId <- H.gets (_.fileId)
    for_ mFileId \fileId -> do
      H.tell FileUpload.proxy unit (FileUpload.SetStatus Loading)
      metadata <- H.lift $ getFileMetadata fileId
      let
        status = case metadata of
          Loaded { fileName, fileSize } ->
            Loaded
              $ showFile fileName (Int.toNumber fileSize)
          l -> unsafeCoerce l -- Safe since Loaded has been handled.
      H.tell FileUpload.proxy unit (FileUpload.SetStatus status)
  -- If the user resets the upload form then we try to delete the existing file,
  -- if one exists.
  UploadedFile Nothing -> do
    result <- resetFile
    H.tell FileUpload.proxy unit (FileUpload.SetStatus result)
  -- If the user uploads a file then we
  --
  -- 1. validate the file and return with error message if invalid;
  --
  -- 2. try to delete the existing file, if one exists; and
  --
  -- 3. actually send the uploaded file to the backend.
  UploadedFile (Just file) -> do
    { orderLineId, maxSize } <- H.get
    case maxSize of
      Just n
        | File.size file > n ->
          H.tell FileUpload.proxy unit
            (FileUpload.SetStatus (Error $ "File must be smaller than " <> Bytes.showPretty n))
      _ -> do
        b64 <- H.liftAff $ readAsBase64 (File.toBlob file)
        H.liftEffect $ log $ "Content (first 50): " <> show (S.take 50 b64)
        result <-
          H.lift
            $ postFile
                { file: b64
                , metadata:
                    { fileName: File.name file
                    , "type": unwrap <$> File.type_ file
                    }
                , orderLineId: orderLineId
                }
        case result of
          Loaded { fileId } -> do
            H.modify_ _ { fileId = Just fileId }
            H.tell FileUpload.proxy unit
              ( FileUpload.SetStatus
                  $ Loaded
                  $ showFile (File.name file) (File.size file)
              )
            -- Let the parent component know about the new file.
            H.raise
              $ Just
              $ SS.CvObject
              $ Map.fromFoldable
                  [ Tuple "type" (SS.CvString "FILE_ATTACHMENT")
                  , Tuple "fileId" (SS.CvString fileId)
                  ]
          _ ->
            H.tell FileUpload.proxy unit
              $ FileUpload.SetStatus
              $ unsafeCoerce result -- Safe since we've handled the Loaded case.

showFile :: String -> Number -> String
showFile name size = name <> " (" <> Bytes.showPretty size <> ")"
