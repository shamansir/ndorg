module App.Component.OrgFileEditor where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))

import Control.Monad.State as State

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console

import Data.Text.Format.Org.Types (OrgFile(..))
import Data.Text.Format.Org.Construct (empty, sec, emptyDoc) as Org

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import App.Component.OrgFileEditor.DocAndSection as DocC -- only Doc component is used here
import App.Component.OrgFileEditor.Keywords as KeywordsC


_editor = Proxy :: _ "org-editor"


type Slot = H.Slot Query Output


data Query a = Query a


type Input =
    { file :: OrgFile
    }


type Output = Unit


type State =
    { file :: OrgFile
    }


type Slots =
    ( keywords :: KeywordsC.Slot Unit
    , doc :: DocC.DocSlot Unit
    )


data Action
    = Action
    | HandleMeta KeywordsC.Output
    | HandleDoc DocC.DocOutput
    | Receive Input


component :: forall m. MonadEffect m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval : H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

  initialState :: Input -> State
  initialState { file } = { file }

  render :: State -> H.ComponentHTML Action Slots m
  render { file } = do
    HH.div_
      $ case file of
        OrgFile { meta, doc } ->
          [ HH.slot KeywordsC._keywords unit KeywordsC.component { keywords : meta } HandleMeta
          , HH.slot DocC._doc unit DocC.docComponent { doc } HandleDoc
          ]

  handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
  handleAction = case _ of
    Action -> pure unit
    HandleMeta output -> case output of
      -- KeywordsC.Output -> do
      _ -> do
        -- H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell KeywordsC._keywords unit (KeywordsC.KwSetEditing true)
        on <- H.requestAll KeywordsC._keywords KeywordsC.KwGetEditing
        liftEffect $ Console.log $ show on
    HandleDoc output -> case output of
      DocC.DocOutput -> do
        -- H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell DocC._doc unit (DocC.DocSetEditing true)
        on <- H.requestAll DocC._doc DocC.DocGetEditing
        liftEffect $ Console.log $ show on
    Receive { file } ->
      State.modify_ $ _ { file = file }
