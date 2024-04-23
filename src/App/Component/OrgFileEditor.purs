module App.Component.OrgFileEditor where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Construct (empty, sec, emptyDoc) as Org

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


import App.Component.OrgFileEditor.Section as SectionC


type State =
    { file :: OrgFile
    , clicked :: Int
    }


type Slots =
    ( section :: SectionC.Slot Int -- TODO: Distinct by Path
    )


data Action
    = Action
    | HandleSection Int SectionC.Output


component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
      -- The only internal event this component can handle are actions as
      -- defined in the `ParentAction` type.
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  _section = SectionC._section
  section = SectionC.component
  sampleSec = Org.sec 0 [] Org.emptyDoc

  initialState :: input -> State
  initialState _ = { file: Org.empty, clicked : 0 }

  -- We render three buttons, handling their output messages with the `HandleButton`
  -- action. When our state changes this render function will run again, each time
  -- sending new input (which contains a new label for the child button component
  -- to use.)
  render :: State -> H.ComponentHTML Action Slots m
  render { clicked } = do
    HH.div_
      [ -- We render our first button with the slot id 0
        HH.slot _section 0 section { section : sampleSec } $ HandleSection 0
        -- We render our second button with the slot id 1
      , HH.slot _section 1 section { section : sampleSec } $ HandleSection 1
        -- We render our third button with the slot id 2
      , HH.slot _section 2 section { section : sampleSec } $ HandleSection 2
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Action -> pure unit
    -- We handle one action, `HandleButton`, which itself handles the output messages
    -- of our button component.
    HandleSection _ output -> case output of
      -- There is only one output message, `Clicked`.
      SectionC.Output -> do
        -- When the `Clicked` message arises we will increment our clicked count
        -- in state, then send a query to the first button to tell it to be `true`,
        -- then send a query to all the child components requesting their current
        -- enabled state, which we log to the console.
        H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell _section 0 (SectionC.SetEditing true)
        on <- H.requestAll _section SectionC.GetEditing
        liftEffect $ Console.log $ show on