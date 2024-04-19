module Example.Driver.IO.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component unit body

  _ <- liftEffect $ HS.subscribe io.messages \(Toggled newState) -> do
    liftEffect $ log $ "Button was internally toggled to: " <> show newState
    pure Nothing

  state0 <- io.query $ H.mkRequest IsOn
  liftEffect $ log $ "The button state is currently: " <> show state0

  void $ io.query $ H.mkTell (SetState true)

  state1 <- io.query $ H.mkRequest IsOn
  liftEffect $ log $ "The button state is now: " <> show state1

-- Child component implementation

type Slot = H.Slot Query Message

data Query a
  = IsOn (Boolean -> a)
  | SetState Boolean a

data Message = Toggled Boolean

data Action = Toggle

type State = { enabled :: Boolean }

component :: forall i m. H.Component Query i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Toggle
      ]
      [ HH.text label ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    newState <- H.modify \st -> st { enabled = not st.enabled }
    H.raise (Toggled newState.enabled)

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  IsOn k -> do
    enabled <- H.gets _.enabled
    pure (Just (k enabled))
  SetState enabled a -> do
    H.modify_ (_ { enabled = enabled })
    pure (Just a)
