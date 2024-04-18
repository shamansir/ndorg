module App.App where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

import App.State (State)
import App.State (init) as State

import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Action
  = Initialize
  | None
  | Increment
  | HandleKey H.SubscriptionId KE.KeyboardEvent

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
  HH.div_
    [ HH.p_
        -- [ HH.text $ "You clicked " <> show state.count <> " times" ]
        []
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "Click me" ]
    , HH.div_
        [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
        , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
        , HH.p_ [ HH.text state.chars ]
        ]
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
  HandleKey _ ev ->
    case findNextAction ev of
        None -> pure unit
        nextAction -> handleAction nextAction
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


findNextAction :: KE.KeyboardEvent -> Action
findNextAction = const None