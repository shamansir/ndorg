module App.Component.OrgFileEditor.DocAndSection where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Array ((:), replicate)
import Data.String (joinWith, fromCodePointArray, codePointFromChar) as String

import Data.FunctorWithIndex (mapWithIndex)

import Data.Text.Format.Org.Types (Section(..), OrgDoc(..), Drawer(..), Todo(..), Priority(..), Cookie(..), Check(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import App.Utils (cn, cn_editing)
import App.Utils (none) as HH

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
    ( doc :: DocSlot Unit
    , words :: WordsC.Slot Unit
    )


secComponent :: forall m. H.Component SecQuery SecInput SecOutput m
secComponent = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< SecReceive
        }
    }
  where
  initialState :: SecInput -> SecState
  initialState { section } = { section, editing: false }

  render :: SecState -> H.ComponentHTML SecAction SecSlots m
  render { section, editing } =
    HH.div
      [ HE.onClick \_ -> SecAction
      , HP.classes [ cn_editing editing, cn "ndorg-section" ]
      ]
      [ renderSectionHeader section
      , case section of
        Section { doc } -> HH.slot _doc unit docComponent { doc } HandleDoc
      ]

  handleAction
    :: SecAction
    -> H.HalogenM SecState SecAction SecSlots SecOutput m Unit
  handleAction = case _ of
    SecReceive input ->
      H.modify_ _ { section = input.section }

    HandleDoc _ -> pure unit
    HandleWords _ _ -> pure unit
    SecAction -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise SecOutput

  handleQuery
    :: forall a
     . SecQuery a
    -> H.HalogenM SecState SecAction SecSlots SecOutput m (Maybe a)
  handleQuery = case _ of
    SecSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)

    SecGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))


renderSectionHeader :: forall m. Section -> H.ComponentHTML SecAction SecSlots m
renderSectionHeader =
  case _ of
    Section section ->
      HH.div [ HP.class_ $ cn "ndorg-section" ]
        [ HH.span
          [ HP.classes [ cn "ndorg-heading", cn $ "ndorg-heading-" <> show section.level, cn $ if section.comment then "ndorg-comment" else "ndorg-none" ]
          ]
          [ HH.span [ HP.class_ $ cn "ndorg-heading-marker" ] [ HH.text $ headingMarker section.level ]  -- FIXME: use css:before for marker
          , case section.todo of
            Just todo -> case todo of
              Todo -> HH.span [ HP.class_ $ cn "ndorg-todo" ] [ HH.text "TODO" ]
              Doing -> HH.span [ HP.class_ $ cn "ndorg-doing" ] [ HH.text "DOING" ]
              Done -> HH.span [ HP.class_ $ cn "ndorg-done" ] [ HH.text "DONE" ]
              Now -> HH.span [ HP.class_ $ cn "ndorg-now" ] [ HH.text "NOW" ]
              CustomKW kw -> HH.span [ HP.class_ $ cn "ndorg-custom" ] [ HH.text kw ]
            Nothing -> HH.none
          , case section.priority of
            Just (Alpha 'A') -> HH.span [ HP.class_ $ cn "ndorg-priority-top" ] [ HH.text "[A]" ]
            Just (Alpha 'B') -> HH.span [ HP.class_ $ cn "ndorg-priority-middle" ] [ HH.text "[B]" ]
            Just (Alpha 'C') -> HH.span [ HP.class_ $ cn "ndorg-priority-low" ] [ HH.text "[C]" ]
            Just (Alpha char) -> HH.span [ HP.class_ $ cn "ndorg-priority-uncertain" ] [ HH.text $ String.fromCodePointArray [ String.codePointFromChar char ] ]
            Just (Num 1) -> HH.span [ HP.class_ $ cn "ndorg-priority-top" ] [ HH.text "[#1]" ]
            Just (Num 2) -> HH.span [ HP.class_ $ cn "ndorg-priority-middle" ] [ HH.text "[#2]" ]
            Just (Num 3) -> HH.span [ HP.class_ $ cn "ndorg-priority-low" ] [ HH.text "[#3]" ]
            Just (Num n) -> HH.span [ HP.class_ $ cn "ndorg-priority-uncertain" ] [ HH.text $ "#" <> show n ]
            Nothing ->  HH.none
          , case section.cookie of
            Just Split -> HH.span [ HP.class_ $ cn "ndorg-cookie" ] [ HH.text "[/]" ]
            Just Percent -> HH.span [ HP.class_ $ cn "ndorg-cookie" ] [ HH.text "[%]" ]
            Just Pie -> HH.span [ HP.class_ $ cn "ndorg-cookie" ] [ HH.text "[/]" ]
            Nothing ->  HH.none
          , case section.check of
            Just Check -> HH.span [ HP.class_ $ cn "ndorg-check" ] [ HH.text "[X]" ]
            Just Uncheck -> HH.span [ HP.class_ $ cn "ndorg-uncheck" ] [ HH.text "[ ]" ]
            Just Halfcheck -> HH.span [ HP.class_ $ cn "ndorg-halfcheck" ] [ HH.text "[-]" ]
            Nothing ->  HH.none
          , HH.text " "
          , HH.slot WordsC._words unit WordsC.component { words : section.heading, kind : WordsC.Inline } $ HandleWords 0
          ]
        , case section.tags of
          [] ->  HH.none
          tags -> HH.span [ HP.class_ $ cn "ndorg-tags" ] $ renderTag <$> tags
        , case section.drawers of
          [] ->  HH.none
          drawers -> HH.div [ HP.class_ $ cn "ndorg-drawer" ] $ renderDrawer <$> drawers
        , if PlanningC.hasPlanning section.planning
          then PlanningC.renderPlanning section.planning
          else  HH.none
        , KeywordsC.renderKeywords section.props
        ]
  where
    renderTag tag = HH.span [ HP.class_ $ cn "ndorg-tag" ] [ HH.text tag ]
    headingMarker level = String.joinWith "" $ replicate level "*"


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

  render :: DocState -> H.ComponentHTML DocAction DocSlots m
  render { doc, editing } =
    HH.div
      [ HE.onClick \_ -> DocAction
      , HP.classes [ cn_editing editing, cn "ndorg-doc" ]
      ]
      $ case doc of
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
