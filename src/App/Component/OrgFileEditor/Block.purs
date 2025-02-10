module App.Component.OrgFileEditor.Block where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Array (singleton) as Array

import Data.Text.Format.Org.Types (Block(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import App.Utils (cn, cn_editing)
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


renderBlock :: forall action m. Block -> H.ComponentHTML action () m
renderBlock =
    case _ of
      Of kind words -> [ HH.text "of", WordsC.renderWordsNEA words ]
      IsDrawer drawer -> [ HH.text "drawer" ]
      Footnote name words -> [ HH.text "footnote", WordsC.renderWordsNEA words ]
      List items -> [ HH.text "list" ]
      Table header rows -> [ HH.text "table" ]
      Paragraph words ->
        [ HH.div
          [ HP.class_ $ cn "ndorg-block-para" ]
          [ WordsC.renderWordsNEA words ]
        ]
      WithKeyword kw block -> [ HH.text "kw", renderBlock block ]
      HRule -> [ HH.text "hr" ]
      LComment text -> [ HH.text "lcomment" ]
      FixedWidth words -> [ HH.text "fixedw", WordsC.renderWordsNEA words ]
      JoinB blockA blockB -> [ renderBlock blockA, renderBlock blockB ]
    >>> HH.div [ HP.class_ $ cn "ndorg-block" ]