module App.Component.OrgFileEditor where

import Prelude

import Type.Proxy (Proxy(..))

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console

import Data.Text.Format.Org.Types (OrgFile(..))
import Data.Text.Format.Org.Construct (empty, sec, emptyDoc) as Org

import Org.Test.Test03b as Test

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import App.Component.OrgFileEditor.DocAndSection as DocC -- only Doc component is used here
import App.Component.OrgFileEditor.Keywords as KeywordsC


_editor = Proxy :: _ "org-editor"


type Slot = H.Slot Query Output


data Query a = Query a


type Output = Unit


type State =
    { file :: OrgFile
    , clicked :: Int
    }


type Slots =
    ( keywords :: KeywordsC.Slot Int
    , doc :: DocC.DocSlot Int
    )


data Action
    = Action
    | HandleMeta KeywordsC.Output
    | HandleDoc DocC.DocOutput


component :: forall input m. MonadEffect m => H.Component Query input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  initialState :: input -> State
  initialState _ = { file: Test.test, clicked : 0 }

  render :: State -> H.ComponentHTML Action Slots m
  render { file } = do
    HH.div_
      $ case file of
        OrgFile { meta, doc } ->
          [ HH.slot KeywordsC._keywords 0 KeywordsC.component { keywords : meta } HandleMeta
          , HH.slot DocC._doc 1 DocC.docComponent { doc } HandleDoc
          ]
      {-
      [ HH.slot _section 0 section { section : sampleSec } $ HandleSection 0
      , HH.slot _section 1 section { section : sampleSec } $ HandleSection 1
      , HH.slot _section 2 section { section : sampleSec } $ HandleSection 2
      , HH.text "foo"
      ]
       -}

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Action -> pure unit
    HandleMeta output -> case output of
      KeywordsC.Output -> do
        H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell KeywordsC._keywords 0 (KeywordsC.SetEditing true)
        on <- H.requestAll KeywordsC._keywords KeywordsC.GetEditing
        liftEffect $ Console.log $ show on
    HandleDoc output -> case output of
      DocC.DocOutput -> do
        H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell DocC._doc 0 (DocC.DocSetEditing true)
        on <- H.requestAll DocC._doc DocC.DocGetEditing
        liftEffect $ Console.log $ show on