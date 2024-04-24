module App.App where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse_)

import Control.Monad.State (get, modify, modify_) as State

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Events as HE

import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

import Web.Event.Event as E
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import App.State (State, Context(..))
import App.State (init, suggestions) as AppState
import App.Utils (cn, extract)

import App.Action (Action(..))
import App.Component.Combo as ComboC
import App.Keys (Combo)
import App.Keys (Match, matches) as Keys
import App.Keys (get) as Combo
import App.Keys (Match(..)) as Match

import App.Component.OrgFileEditor as EditorC
import App.Component.OrgFilePreview as PreviewC


type Slots =
    ( "org-editor" :: EditorC.Slot Unit
    , "org-preview" :: PreviewC.Slot Unit
    )


component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> AppState.init
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }


render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    {- HH.p_
        -- [ HH.text $ "You clicked " <> show state.count <> " times" ]
        []
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    , HH.div_
        [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
        , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
        -- , HH.p_ [ HH.text state.chars ]
        ]
    , -}
    [ HP.class_ $ cn "ndorg" ]
    [ HH.div
        [ HP.class_ $ cn "ndorg-editor" ]
        [ HH.span [ HP.class_ $ cn "ndorg-hint" ]
        [ HH.text "Press a key..." ]
        , HH.slot_ EditorC._editor unit EditorC.component unit
        ]
    , HH.div
        [ HP.class_ $ cn "ndorg-preview" ]
        [ HH.span [ HP.class_ $ cn "ndorg-hint" ] [ HH.text "Press a key..." ]
        , HH.slot_ PreviewC._preview unit PreviewC.component unit
        ]
    , HH.div
        [ HP.class_ $ cn "ndorg-suggest" ]
        $ ComboC.render <$> AppState.suggestions state.context
    , case state.curKey of
        Just kevt ->
          HH.div [ HP.class_ $ cn "ndorg-curkey" ] [ ComboC.key kevt.key ]
        Nothing -> HH.text ""
    ]


handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> pure unit -- H.modify_ \st -> st { count = st.count + 1 }
  None -> pure unit
  Initialize -> do
    document <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListener
        KET.keydown
        (HTMLDocument.toEventTarget document)
        (map (HandleKeyDown sid) <<< KE.fromEvent)
    H.subscribe' \sid ->
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKeyUp sid) <<< KE.fromEvent)
  HandleKeyDown _ ev -> do
    State.modify_ $ _ { curKey = extract ev }
  HandleKeyUp _ ev -> do
    state <- State.modify $ _ { curKey = Nothing }
    traverse_  (Debug.spy "next" >>> handleAction) $ nextActionByKeypress (AppState.suggestions state.context) ev
  ClearCombo ->
    State.modify_ $ _ { context = TopLevel }
  AddHeading -> do
    State.modify_ $ _ { context = InSection }
    pure unit
  AddProperty -> do
    State.modify_ $ _ { context = InSection }
    pure unit
  WaitForCombos combo nextCombos ->
    State.modify_ $ _ { context = InCombo combo nextCombos }
    {-
    | KE.shiftKey ev -> do
        H.liftEffect $ E.preventDefault $ KE.toEvent ev
        let char = KE.key ev
        when (String.length char == 1) do
          H.modify_ \st -> st { chars = st.chars <> char }

    | KE.key ev == "Enter" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.modify_ _ { chars = "" }
        H.unsubscribe sid

    | otherwise ->
        pure unit
    -}


nextActionByKeypress :: Array (Combo Action) -> KE.KeyboardEvent -> Array Action
nextActionByKeypress curCombos wkevt =
  case extract wkevt of
    Just kevt ->
      case Debug.spy "matches" $ Keys.matches curCombos kevt of
        Match.Exact combo -> [ Combo.get combo ]
        Match.Wait combo nextCombos -> [ WaitForCombos combo nextCombos ]
        Match.None -> [ ClearCombo ]
    _ -> []