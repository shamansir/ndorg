module App.Component.OrgFileEditor.Block where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array (singleton) as Array
import Data.Array.NonEmpty (toArray) as NEA
import Data.String (joinWith) as String

import Data.Text.Format.Org.Types (Block(..), BlockKind(..), Language(..), Check(..), Counter(..), Drawer(..))
import Data.Text.Format.Org.Types (ListItems(..), ListType(..), Item(..)) as OrgList

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import App.Utils (cn, cn_editing)
import App.Utils (none) as HH
import App.Component.OrgFileEditor.Words as WordsC


type Slot = H.Slot Query Output


_block = Proxy :: _ "block"


type Input =
    { block :: Block
    }


data Output =
    Output


data Query a
  = BlockGetEditing (Boolean -> a)
  | BlockSetEditing Boolean a


type State =
    { block :: Block
    , editing :: Boolean
    }


data Action
    = Action
    | Receive Input


component :: forall m. H.Component Query Input Output m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input -> State
  initialState { block } = { block, editing: false }

  render :: State -> H.ComponentHTML Action () m
  render { block, editing } =
    HH.div
      [ HE.onClick \_ -> Action
      , HP.classes [ cn_editing editing, cn "ndorg-block" ]
      ]
      [ renderBlock block
      ]

  handleAction
    :: Action
    -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Receive input ->
      H.modify_ _ { block = input.block }

    Action -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise Output

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of

    BlockSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)

    BlockGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))


renderBlock :: forall action slots m. Block -> H.ComponentHTML action slots m
renderBlock =
    case _ of
      Of Quote words ->
        [ blockDiv_ "quote" $ blockStartEnd "QUOTE" $ WordsC.renderWordsNEA words ]
      Of Example words ->
        [ blockDiv_ "example" $ blockStartEnd "EXAMPLE" $ WordsC.renderWordsNEA words ]
      Of Center words ->
        [ blockDiv_ "center" $ blockStartEnd "CENTER" $ WordsC.renderWordsNEA words ]
      Of Verse words ->
        [ blockDiv_ "verse" $ blockStartEnd "VERSE" $ WordsC.renderWordsNEA words ]
      Of Export words ->
        [ blockDiv_ "export" $ blockStartEnd "EXPORT" $ WordsC.renderWordsNEA words ]
      Of Comment words ->
        [ blockDiv_ "comment" $ blockStartEnd "COMMENT" $ WordsC.renderWordsNEA words ]
      Of (Code mbLang) words ->
        let
          startMark = "SRC" <> case mbLang of
            Just (Language lang) -> " " <> lang
            Nothing -> ""
          endMark = "SRC"
        in [ blockDiv_ "code" $ blockStartEnd_ startMark endMark $ WordsC.renderWordsNEA words ]
      Of (Custom text params) words ->
        let
          startMark = text <> " " <> String.joinWith " " params
          endMark = text
        in [ blockDiv_ "custom" $ blockStartEnd_ startMark endMark $ WordsC.renderWordsNEA words ]
      IsDrawer drawer -> [ renderDrawer drawer ]
      Footnote name words -> [ HH.text "footnote", WordsC.renderWordsNEA words ]
      List items -> [ renderList items ]
      Table header rows -> [ HH.text "table" ]
      Paragraph words ->
        [ blockDiv "para" $ WordsC.renderWordsNEA words ]
      WithKeyword kw block -> [ HH.text "kw", renderBlock block ]
      HRule -> [ HH.hr [] ]
      LComment text -> [ HH.text "lcomment" ]
      FixedWidth words -> [ HH.div [ HP.class_ $ cn "ndorg-block-fixed-width" ] [ WordsC.renderWordsNEA words ] ]
      JoinB blockA blockB -> [ renderBlock blockA, renderBlock blockB ]
    >>> HH.div [ HP.class_ $ cn "ndorg-block" ]
    where
      blockStartEnd bname = blockStartEnd_ bname bname
      blockStartEnd_ bstart bend inner =
        [ HH.span [ HP.class_ $ cn "ndorg-block-start-end" ] [ HH.text $ "#+BEGIN_" <> bstart ]
        , HH.span [ HP.class_ $ cn "ndorg-block-content" ] [ inner ]
        , HH.span [ HP.class_ $ cn "ndorg-block-start-end" ] [ HH.text $ "#+END_" <> bend ]
        ]
      blockDiv_ class_ = HH.div [ HP.class_ $ cn $ "ndorg-block-" <> class_ ]
      blockDiv class_ inner = blockDiv_ class_ [ inner ]


renderDrawer :: forall action slots m. Drawer -> H.ComponentHTML action slots m
renderDrawer =
  case _ of
    Drawer { name, content } ->
      HH.div
        [ HP.class_ $ cn "ndorg-drawer" ]
        [ HH.div [ HP.class_ $ cn "ndorg-drawer-name" ] [ HH.text name ]
        , HH.div [ HP.class_ $ cn "ndorg-drawer-content" ]
            [ WordsC.renderWordsNEA content ]
        ]


renderList :: forall action slots m. OrgList.ListItems -> H.ComponentHTML action slots m
renderList =
  case _ of
    OrgList.ListItems ltype items ->
      HH.div
        [ HP.classes [ cn $ "ndorg-list", cn $ "ndorg-list-" <> listTypeClass ltype ]
        ]
        $ NEA.toArray $ renderItem <$> items
  where
    renderItem = case _ of
      OrgList.Item def words mbSubitems ->
        HH.div
          [ HP.class_ $ cn "ndorg-list-item" ]
          $ case def.check of
            Just Check     -> HH.span [ HP.class_ $ cn "ndorg-check" ]   [ HH.text "[X]" ]
            Just Uncheck   -> HH.span [ HP.class_ $ cn "ndorg-uncheck" ] [ HH.text "[ ]" ]
            Just Halfcheck -> HH.span [ HP.class_ $ cn "ndorg-uncheck" ] [ HH.text "[-]" ]
            Nothing -> HH.none
          : case def.counter of
            Just (Counter n) -> HH.span [ HP.class_ $ cn "ndorg-counter" ] [ HH.text $ "(" <> show n <> ")" ]
            Nothing -> HH.none
          : HH.span [ HP.class_ $ cn "ndorg-list-item-content" ]
            [ WordsC.renderWordsNEA words ]
          : case def.tag of
            Just tag -> HH.span [ HP.class_ $ cn "ndorg-tag" ] [ HH.text tag ]
            Nothing -> HH.none
          : case def.drawers of
            [] -> HH.none
            drawers ->
              HH.div
                [ HP.class_ $ cn "ndorg-list-item-drawers" ]
                $ renderDrawer <$> drawers
          : case mbSubitems of
            Nothing -> []
            Just subItems ->
              [ HH.div
                [ HP.class_ $ cn "ndorg-list-item-continue" ]
                [ renderList subItems ]
              ]

    listTypeClass = case _ of
      OrgList.Bulleted -> "bulleted"
      OrgList.Numbered -> "numbered"
      OrgList.NumberedFrom _ -> "numbered"
      OrgList.Hyphened -> "hyphened"
      OrgList.Alphed -> "alphed"
      OrgList.Plussed -> "plussed"