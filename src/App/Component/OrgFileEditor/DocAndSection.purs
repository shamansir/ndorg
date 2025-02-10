module App.Component.OrgFileEditor.DocAndSection where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Array ((:))

import Data.FunctorWithIndex (mapWithIndex)

import Data.Text.Format.Org.Types (Section(..), OrgDoc(..), Drawer(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import App.Component.OrgFileEditor.Block as BlockC
import App.Component.OrgFileEditor.Words as WordsC
import App.Component.OrgFileEditor.Keywords as KeywordsC
import App.Component.OrgFileEditor.Planning as PlanningC


type SecSlot = H.Slot SecQuery SecOutput


_section = Proxy :: _ "section"


type SecInput =
    { section :: Section
    }


data SecOutput =
    SecOutput


data SecQuery a
  = SecGetEditing (Boolean -> a)
  | SecSetEditing Boolean a


type SecState =
    { section :: Section
    , editing :: Boolean
    }


data SecAction
    = SecAction
    | SecReceive SecInput
    | HandleDoc DocOutput
    | HandleWords Int WordsC.Output


type SecSlots =
    ( doc :: DocSlot Int
    , words :: WordsC.Slot Int
    )


secComponent :: forall m. H.Component SecQuery SecInput SecOutput m
secComponent = H.mkComponent
    { initialState
    , render
      -- This component can handle internal actions, handle queries sent by a
      -- parent component, and update when it receives new input.
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< SecReceive
        }
    }
  where
  initialState :: SecInput -> SecState
  initialState { section } = { section, editing: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: SecState -> H.ComponentHTML SecAction SecSlots m
  render { section, editing } =
    HH.div
      [ HE.onClick \_ -> SecAction ]
      [ HH.text $ "Section: " <> " (" <> (if editing then "on" else "off") <> ")"
      , renderSectionHeader section
      , case section of
        Section { doc } -> HH.slot _doc 0 docComponent { doc } HandleDoc
      -- HH.slot KeywordsC._keywords 0 KeywordsC.component { keywords : meta } HandleMeta
      ]

  handleAction
    :: SecAction
    -> H.HalogenM SecState SecAction SecSlots SecOutput m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    SecReceive input ->
      H.modify_ _ { section = input.section }

    HandleDoc _ -> pure unit
    HandleWords _ _ -> pure unit
    -- When the button is clicked we update our `enabled` field in state, and
    -- we notify our parent component that the `Clicked` event happened.
    SecAction -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise SecOutput

  handleQuery
    :: forall a
     . SecQuery a
    -> H.HalogenM SecState SecAction SecSlots SecOutput m (Maybe a)
  handleQuery = case _ of
    -- When we receive a the tell-style `SetEnabled` query with a boolean, we
    -- set that value in state.
    SecSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)

    -- When we receive a the request-style `GetEnabled` query, which requires
    -- a boolean result, we get a boolean from our state and reply with it.
    SecGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))


renderSectionHeader :: forall m. Section -> H.ComponentHTML SecAction SecSlots m
renderSectionHeader =
  case _ of
    Section section ->
      HH.div []
        [ HH.text $ show section.level
        , HH.slot WordsC._words 0 WordsC.component { words : section.heading } $ HandleWords 0
        , case section.todo of
          Just todo -> HH.text "todo"
          Nothing -> HH.text "-"
        , case section.priority of
          Just priority -> HH.text "priority"
          Nothing -> HH.text "-"
        , case section.cookie of
          Just cookie -> HH.text "cookie"
          Nothing -> HH.text "-"
        , case section.check of
          Just check -> HH.text "check"
          Nothing -> HH.text "-"
        , case section.tags of
          [] -> HH.text "-"
          tags -> HH.span [] $ HH.text <$> tags
        , case section.drawers of
          [] -> HH.text "-"
          drawers -> HH.div [] $ renderDrawer <$> drawers
        , if PlanningC.hasPlanning section.planning
          then PlanningC.renderPlanning section.planning
          else HH.text "-"
        , KeywordsC.renderKeywords section.props
        , if section.comment then HH.text "comment" else HH.text "-"
        ]


renderDrawer :: forall action slots m. Drawer -> H.ComponentHTML action slots m
renderDrawer = case _ of
  Drawer { name, content } -> HH.span [] [ HH.text name, WordsC.renderWordsNEA content ]


_doc = Proxy :: _ "doc"


type DocSlot = H.Slot DocQuery DocOutput


type DocInput =
    { doc :: OrgDoc
    }


data DocOutput =
    DocOutput


data DocQuery a
  = DocGetEditing (Boolean -> a)
  | DocSetEditing Boolean a


type DocState =
    { doc :: OrgDoc
    , editing :: Boolean
    }


type DocSlots =
    ( block :: BlockC.Slot Int
    , section :: SecSlot Int
    )


data DocAction
    = DocAction
    | DocReceive DocInput
    | HandleSection Int SecOutput
    | HandleBlock Int BlockC.Output


docComponent :: forall m. H.Component DocQuery DocInput DocOutput m
docComponent = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< DocReceive
        }
    }
  where
  initialState :: DocInput -> DocState
  initialState { doc } = { doc, editing: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: DocState -> H.ComponentHTML DocAction DocSlots m
  render { doc, editing } =
    HH.div
      [ HE.onClick \_ -> DocAction ]
      $ (HH.text $ "Doc: " <> " (" <> (if editing then "on" else "off") <> ")")
      : case doc of
        OrgDoc { zeroth, sections } ->
          [ HH.div [] $ mapWithIndex (\idx block -> HH.slot BlockC._block idx BlockC.component { block } $ HandleBlock idx) zeroth
          , HH.div [] $ mapWithIndex (\idx section -> HH.slot _section idx secComponent { section } $ HandleSection idx) sections
          ]


  handleAction
    :: DocAction
    -> H.HalogenM DocState DocAction DocSlots DocOutput m Unit
  handleAction = case _ of
    DocReceive input ->
      H.modify_ _ { doc = input.doc }

    DocAction -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise DocOutput

    HandleSection _ _ ->
      pure unit

    HandleBlock _ _ ->
      pure unit

  handleQuery
    :: forall a
     . DocQuery a
    -> H.HalogenM DocState DocAction DocSlots DocOutput m (Maybe a)
  handleQuery = case _ of
    DocSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)
    DocGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))
