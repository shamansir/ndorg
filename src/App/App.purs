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

import App.Action (Action(..), ExampleId(..))
import App.Component.Combo as ComboC
import App.Keys (Combo)
import App.Keys (Match, matches) as Keys
import App.Keys (get) as Combo
import App.Keys (Match(..)) as Match

import App.Component.OrgFileEditor as EditorC
import App.Component.OrgFilePreview as PreviewC

import Org.Test.Test01 as Ex01
import Org.Test.Test02a as Ex02a
import Org.Test.Test02b as Ex02b
import Org.Test.Test03a as Ex03a
import Org.Test.Test03b as Ex03b
import Org.Test.Test03c as Ex03c
import Org.Test.Test03d as Ex03d
import Org.Test.Test03e as Ex03e
import Org.Test.Test04a as Ex04a
import Org.Test.Test04b as Ex04b
import Org.Test.Test04c as Ex04c
import Org.Test.Test04d as Ex04d
import Org.Test.Test04e as Ex04e
import Org.Test.Test04f as Ex04f
import Org.Test.Test04g as Ex04g
import Org.Test.Test04h as Ex04h
import Org.Test.Test04i as Ex04i
import Org.Test.Test18 as Ex18


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
    [ HH.div [ HP.class_ $ cn "ndorg-path" ] [ HH.text $ show state.focus ]
    , HH.div
        [ HP.class_ $ cn "ndorg-editor" ]
        [ HH.span [ HP.class_ $ cn "ndorg-hint" ]
        [ HH.text "Press a key..." ]
        , HH.slot_ EditorC._editor unit EditorC.component { file : Debug.spy "file-ed" state.file }
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
  LoadExample n ->
    State.modify_ $ _ { file = example n, context = TopLevel }
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


example = case _ of
  Ex01 -> Ex01.test
  Ex02a -> Ex02a.test
  Ex02b -> Ex02b.test
  Ex03a -> Ex03a.test
  Ex03b -> Ex03b.test
  Ex03c -> Ex03c.test
  Ex03d -> Ex03d.test
  Ex03e -> Ex03e.test
  Ex04a -> Ex04a.test
  Ex04b -> Ex04b.test
  Ex04c -> Ex04c.test
  Ex04d -> Ex04d.test
  Ex04e -> Ex04e.test
  Ex04f -> Ex04f.test
  Ex04g -> Ex04g.test
  Ex04h -> Ex04h.test
  Ex04i -> Ex04i.test
  Ex18 -> Ex18.test
  _ -> Ex01.test


nextActionByKeypress :: Array (Combo Action) -> KE.KeyboardEvent -> Array Action
nextActionByKeypress curCombos wkevt =
  case Debug.spy "kevt" $ extract wkevt of
    Just kevt ->
      case Debug.spy "matches" $ Keys.matches curCombos kevt of
        Match.Exact combo -> [ Combo.get combo ]
        Match.Wait combo nextCombos -> [ WaitForCombos combo nextCombos ]
        Match.None -> [ ClearCombo ]
    _ -> []