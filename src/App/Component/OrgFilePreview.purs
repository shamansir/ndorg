module App.Component.OrgFilePreview where

import Prelude

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console

import Data.Text.Format.Org.Types (OrgFile, Section)
import Data.Text.Format.Org.Construct (empty, sec, emptyDoc) as Org

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


_preview = Proxy :: _ "org-preview"


type Slot = H.Slot Query Output


data Query a = Query a


type Output = Unit


type State =
    { file :: OrgFile
    }


data Action
    = Action


component :: forall input m. MonadEffect m => H.Component Query input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> State
  initialState _ = { file: Org.empty }

  sampleSec = Org.sec 0 [] Org.emptyDoc

  render :: State -> H.ComponentHTML Action () m
  render { } = do
    HH.div_
      [ section sampleSec
      , section sampleSec
      , section sampleSec
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Action -> pure unit


section :: forall m. Section -> H.ComponentHTML Action () m
section _ = HH.div [] [ HH.text "section" ]