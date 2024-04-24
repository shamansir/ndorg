module App.Component.OrgFileEditor where

import Prelude

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console

import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Construct (empty, sec, emptyDoc) as Org

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


import App.Component.OrgFileEditor.Section as SectionC


_editor = Proxy :: _ "org-editor"


type Slot = H.Slot Query Output


data Query a = Query a


type Output = Unit


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


component :: forall input m. MonadEffect m => H.Component Query input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  _section = SectionC._section
  section = SectionC.component
  sampleSec = Org.sec 0 [] Org.emptyDoc

  initialState :: input -> State
  initialState _ = { file: Org.empty, clicked : 0 }

  render :: State -> H.ComponentHTML Action Slots m
  render {} = do
    HH.div_
      [ HH.slot _section 0 section { section : sampleSec } $ HandleSection 0
      , HH.slot _section 1 section { section : sampleSec } $ HandleSection 1
      , HH.slot _section 2 section { section : sampleSec } $ HandleSection 2
      , HH.text "foo"
      ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Action -> pure unit
    HandleSection _ output -> case output of
      SectionC.Output -> do
        H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell _section 0 (SectionC.SetEditing true)
        on <- H.requestAll _section SectionC.GetEditing
        liftEffect $ Console.log $ show on