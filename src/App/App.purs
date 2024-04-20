module App.App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String

import Control.Monad.State (get) as State

import Effect (Effect)
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

import App.State (State)
import App.State (init, suggestions) as State
import App.Utils (cn, extract)

import App.Component.Combo as Combo
import App.Keys (Combo)
import App.Keys (Match, matches) as Keys

data Action
  = Initialize
  | None
  | Increment
  | HandleKey H.SubscriptionId KE.KeyboardEvent
  | AddHeading
  | AddProperty


component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> State.init
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
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
        [ HH.span [ HP.class_ $ cn "ndorg-hint" ] [ HH.text "Press a key..." ] ]
    , HH.div
        [ HP.class_ $ cn "ndorg-preview" ]
        [ HH.span [ HP.class_ $ cn "ndorg-hint" ] [ HH.text "Press a key..." ] ]
    , HH.div
        [ HP.class_ $ cn "ndorg-suggest" ]
        $ Combo.render <$> State.suggestions state.context
    ]

handleAction :: forall cs o m. MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Increment -> pure unit -- H.modify_ \st -> st { count = st.count + 1 }
  None -> pure unit
  Initialize -> do
    document <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListener
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey _ ev -> do
    state <- State.get
    case nextActionByKeypress (State.suggestions state.context) ev of
        None -> pure unit
        nextAction -> handleAction nextAction
  AddHeading ->
    pure unit
  AddProperty ->
    pure unit
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


nextActionByKeypress :: Array Combo -> KE.KeyboardEvent -> Action
nextActionByKeypress curCombos wkevt =
  case extract wkevt of
    Just kevt ->
      case Keys.matches curCombos kevt of
        _ -> None
    _ -> None