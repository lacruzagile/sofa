module Sofa.App.OrderForm.SelectParticipant (Slot, Output(..), proxy, component) where

import Prelude
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Select as Sel
import Sofa.App.Requests (getParticipants)
import Sofa.Component.Spinner as Spinner
import Sofa.Component.Typeahead as Typeahead
import Sofa.Css as Css
import Sofa.Data.Auth (class CredentialStore)
import Sofa.Data.Loadable (Loadable(..))
import Sofa.Data.Loadable as Loadable
import Sofa.Data.SmartSpec as SS
import Sofa.HtmlUtils (focusElementByRef)
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLInputElement as HTMLInputElement

type Slot id
  = forall query. H.Slot query Output id

proxy :: Proxy "selectParticipant"
proxy = Proxy

type Output
  = Loadable SS.Participant

type State
  = ( selected :: Maybe SS.Participant -- ^ The chosen participant, if any.
    , selectedFull :: Loadable SS.Participant -- ^ The chosen participant in full, if any.
    , available :: Loadable (Array SS.Participant) -- ^ The available participant.
    )

data Action
  = Initialize

type Action'
  = Sel.Action Action

mkParticipants :: 
    (Loadable (Array (Tuple String SS.ConfigValue))) ->  
    Loadable (Array SS.Participant)
mkParticipants loadableParticipants = do
  participants <- case loadableParticipants of
    Loaded p -> Loaded (A.concatMap mapParticipant p)
    _ -> Loaded []
  pure participants
  

mapParticipant :: 
    Tuple String SS.ConfigValue ->  
    Array SS.Participant
mapParticipant (Tuple k v) = [SS.Participant {email: (show v), user: k}]

component ::
  forall query input f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component query input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render: const $ HH.slot selectLabel unit selectComponent unit identity
    , eval: H.mkEval H.defaultEval { handleAction = H.raise }
    }
  where
  selectLabel = Proxy :: Proxy "select"

selectComponent ::
  forall query input f m.
  MonadAff m =>
  CredentialStore f m =>
  H.Component (Sel.Query query ()) input Output m
selectComponent =
  Sel.component (const input)
    $ Sel.defaultSpec
        { initialize = Just Initialize
        , handleAction = handleAction
        , handleEvent = handleEvent
        , render = render
        }
  where
  input :: Sel.Input State
  input =
    { inputType: Sel.Text
    , debounceTime: Just (Milliseconds 500.0)
    , search: Nothing
    , getItemCount: maybe 0 A.length <<< Loadable.toMaybe <<< _.available
    , selected: Nothing
    , selectedFull: Idle
    , available: Idle
    }

  handleAction = case _ of
    Initialize -> focusElementByRef (H.RefLabel "select-input")

  handleEvent = case _ of
    Sel.Searched str ->
      when (S.length str >= 3)
        $ do
            H.modify_ _ { available = Loading }
            result <- H.lift $ getParticipants str
            participants <- pure $ mkParticipants result
            H.modify_ _ { available = participants }
            -- If the result is an error then we also propagate this to the
            -- parent.
            case result of
              Error msg -> H.raise $ Error msg
              _ -> pure unit
    Sel.Selected idx -> do
      st' <-
        H.modify \st ->
          st
            { search = ""
            , selected = (_ !! idx) =<< Loadable.toMaybe st.available
            , selectedFull = Loading
            , visibility = Sel.Off
            }
      -- Clear the input element.
      inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
      for_ inputElement
        $ maybe (pure unit) (H.liftEffect <<< HTMLInputElement.setValue "")
        <<< HTMLInputElement.fromHTMLElement
      -- Fetch the full representation of the selected participant, if possible.
      selectedFull <- case st'.selected of
        Just participant -> pure (Loaded participant)
        _ -> pure Idle
      H.modify_ \st -> st { selectedFull = selectedFull }
      -- Let the parent component know about the new selection.
      H.raise selectedFull
    _ -> pure unit

  render :: Sel.State State -> H.ComponentHTML _ () m
  render st = case st.selectedFull of
    Loading ->
      HH.div
        [ Css.classes
            [ "nectary-input"
            , "w-full"
            , "flex"
            , "items-center"
            , "space-x-3"
            ]
        ]
        [ HH.div [ Css.class_ "grow" ] [ HH.text "Loading participant …" ]
        , Spinner.render $ Spinner.defaults { classes = [ Css.c "my-4" ] }
        ]
    _ ->
      Typeahead.render
        $ (Typeahead.initRenderState st)
            { selected = map (\(SS.Participant { user }) -> user) st.selected
            , selectedIndex =
              do
                SS.Participant { user: selName } <- st.selected
                vals <- Loadable.toMaybe st.available
                A.findIndex (\(SS.Participant { user: name }) -> name == selName) vals
            , values =
              case st.available of
                Loaded available ->
                  let
                    renderItem (SS.Participant participant) =
                      HH.span_
                        [ HH.text participant.email
                        , HH.text " "
                        , HH.span
                            [ Css.class_ "text-gray-400" ]
                            [ HH.text participant.user]
                        ]
                  in
                    renderItem <$> available
                _ -> []
            , noSelectionText = "Type to search participant  …"
            , loading = Loadable.isLoading st.available
            }
