-- | A component providing Nectary style file uploads.
module Sofa.Component.FileUpload
  ( Slot
  , Output
  , Query(..)
  , proxy
  , component
  , defaultInput
  , readAsBase64
  ) where

import Prelude
import DOM.HTML.Indexed.InputAcceptType (InputAcceptType(..), InputAcceptTypeAtom(..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType)
import Data.String as S
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Sofa.Component.Icon as Icon
import Sofa.Css as Css
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Widgets as Widgets
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.File.Blob (Blob)
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader.Aff (readAsDataURL)
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DragEvent

type Slot id
  = H.Slot Query Output id

proxy :: Proxy "nectaryFileUpload"
proxy = Proxy

type Input
  = { status :: Maybe String
    -- ^ Initial status, nothing if idle, just a message otherwise.
    , wrapperClasses :: Array HH.ClassName
    , mediaTypes :: Maybe (Array MediaType) -- ^ The accepted media types, or all if nothing.
    }

defaultInput :: Input
defaultInput =
  { status: Nothing
  , wrapperClasses: []
  , mediaTypes: Nothing
  }

-- | The uploaded file. When the upload is reset then `Nothing` is raised.
type Output
  = Maybe File

type State
  = { status :: Loadable String
    , wrapperClasses :: Array HH.ClassName
    , accept :: Maybe InputAcceptType -- ^ The accepted media types, or all if nothing.
    }

data Action
  = UploadFile (Array File)
  | DropFile DragEvent
  | PreventDefault DragEvent
  | ResetStatus

data Query a
  = SetStatus (Loadable String) a

readAsBase64 :: Blob -> Aff String
readAsBase64 blob = do
  result <- readAsDataURL blob
  -- Extract the Base64 part of the data URI.
  pure $ S.drop 1 $ S.dropWhile (_ /= S.codePointFromChar ',') result

component ::
  forall m.
  MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }

initialState :: Input -> State
initialState input =
  { status: maybe Idle Loaded input.status
  , wrapperClasses: input.wrapperClasses
  , accept: (InputAcceptType <<< map AcceptMediaType) <$> input.mediaTypes
  }

render ::
  forall slots m.
  Monad m =>
  State -> H.ComponentHTML Action slots m
render state =
  HH.div
    [ HP.classes
        ( [ Css.c "w-[32rem]"
          , Css.c "py-10"
          , Css.c "flex"
          , Css.c "flex-col"
          , Css.c "space-y-6"
          , Css.c "bg-snow-500"
          , Css.c "border"
          , Css.c "border-stormy-300"
          , Css.c "rounded-sm"
          ]
            <> state.wrapperClasses
        )
    , HE.onDrag PreventDefault
    , HE.onDragStart PreventDefault
    , HE.onDragEnd PreventDefault
    , HE.onDragOver PreventDefault
    , HE.onDragEnter PreventDefault
    , HE.onDragLeave PreventDefault
    , HE.onDrop
        $ case state.status of
            Idle -> DropFile
            _ -> PreventDefault
    ]
    renderBody
  where
  renderText_ = renderText "text-stormy-300"

  renderText class_ text =
    HH.div
      [ HP.class_ (Css.c class_) ]
      [ HH.text text ]

  renderBodySkeleton icon text bottom =
    [ HH.div
        [ HP.classes
            [ Css.c "w-max"
            , Css.c "mx-auto"
            , Css.c "flex"
            , Css.c "items-center"
            , Css.c "gap-x-2"
            ]
        ]
        [ icon
        , text
        ]
    , bottom
    ]

  renderBodyUpload =
    renderBodySkeleton
      $ Icon.upload [ Icon.classes [ Css.c "h-4", Css.c "fill-stormy-300" ] ]

  renderBodySuccess =
    renderBodySkeleton
      $ Icon.done
          [ Icon.classes
              [ Css.c "h-8"
              , Css.c "fill-success-500"
              , Css.c "nectary-short-bounce"
              ]
          ]

  renderBody = case state.status of
    Loading ->
      renderBodyUpload (renderText_ "Attaching …")
        $ Widgets.spinner [ Css.c "w-8", Css.c "h-8", Css.c "mx-auto" ]
    Loaded msg ->
      renderBodySuccess (renderText_ msg)
        $ HH.button
            [ HP.classes
                [ Css.c "sofa-btn-destructive"
                , Css.c "h-8"
                , Css.c "mx-auto"
                ]
            , HE.onClick \_ -> ResetStatus
            ]
            [ HH.text "Remove" ]
    Error msg ->
      renderBodyUpload (renderText "text-raspberry-500" msg)
        $ HH.button
            [ HP.classes
                [ Css.c "sofa-btn-secondary"
                , Css.c "h-8"
                , Css.c "mx-auto"
                ]
            , HE.onClick \_ -> ResetStatus
            ]
            [ HH.text "Retry" ]
    Idle ->
      renderBodyUpload (renderText_ "Drop a file here to attach or …")
        $ let
            accept = maybe [] (\a -> [ HP.accept a ]) state.accept

            inputAttrs =
              [ HP.type_ HP.InputFile
              , HP.classes [ Css.c "sr-only" ]
              , HP.multiple false
              , HE.onFileUpload UploadFile
              ]
                <> accept
          in
            HH.label
              [ HP.classes
                  [ Css.c "sofa-btn-secondary"
                  , Css.c "h-8"
                  , Css.c "text-sm"
                  , Css.c "mx-auto"
                  ]
              ]
              [ HH.text "Browse"
              , HH.input inputAttrs
              ]

handleFile ::
  forall slots m.
  MonadAff m =>
  File -> H.HalogenM State Action slots Output m Unit
handleFile file = do
  H.modify_ _ { status = Loading }
  H.liftEffect $ log
    $ show
        { name: File.name file
        , mediaType: File.type_ file
        , size: File.size file
        }
  H.raise (Just file)

handleAction ::
  forall slots m.
  MonadAff m =>
  Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  UploadFile [ file ] -> handleFile file
  UploadFile _ -> H.modify_ _ { status = Error "Only one file can be attached." }
  PreventDefault event -> H.liftEffect $ Event.preventDefault (DragEvent.toEvent event)
  DropFile event -> do
    H.liftEffect $ Event.preventDefault (DragEvent.toEvent event)
    let
      dataTransfer = DragEvent.dataTransfer event
    case FileList.items <$> DataTransfer.files dataTransfer of
      Nothing -> H.liftEffect $ log "No file dropped"
      Just [ file ] -> handleFile file
      Just _ -> H.modify_ _ { status = Error "Only one file can be attached." }
  ResetStatus -> do
    H.modify_ _ { status = Loading }
    H.raise Nothing

handleQuery ::
  forall action slots output a m.
  MonadAff m =>
  Query a -> H.HalogenM State action slots output m (Maybe a)
handleQuery = case _ of
  SetStatus status next -> do
    H.modify_ _ { status = status }
    pure $ Just next
