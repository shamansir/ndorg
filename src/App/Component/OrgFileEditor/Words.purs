module App.Component.OrgFileEditor.Words where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEA

import Data.Text.Format.Org.Types (Words(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import App.Utils (cn, cn_editing)


type Slot = H.Slot Query Output


_words = Proxy :: _ "words"


data Kind
  = Inline
  | AsBlock


type Input =
    { words :: NonEmptyArray Words
    , kind :: Kind
    }


data Output =
    Output


data Query a
  = WordsGetEditing (Boolean -> a)
  | WordsSetEditing Boolean a


type State =
    { words :: NonEmptyArray Words
    , kind :: Kind
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
  initialState { words, kind } = { words, kind, editing: false }

  render :: State -> H.ComponentHTML Action () m
  render { kind, words, editing } =
    HH.div
      [ HE.onClick \_ -> Action
      , HP.classes
          $ case kind of
            Inline -> [ cn_editing editing, cn "ndorg-words", cn "ndorg-words-inline" ]
            AsBlock -> [ cn_editing editing, cn "ndorg-words" ]
      ]
      [ renderWordsNEA words
      ]


  handleAction
    :: Action
    -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Receive input ->
      H.modify_ _ { words = input.words }
    Action -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise Output

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    WordsSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)
    WordsGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))


renderWordsNEA :: forall action slots m. NonEmptyArray Words -> H.ComponentHTML action slots m
renderWordsNEA = HH.span [] <<< NEA.toArray <<< map renderWords


renderWords :: forall action slots m. Words -> H.ComponentHTML action slots m
renderWords = case _ of
  Marked _ str -> HH.text $ "marked" <> str
  Link trg mbstr -> HH.text "link"
  Image imageSrc -> HH.text "image"
  Punct cp -> HH.text "codepoint"
  Plain plain -> HH.text plain
  Markup str -> HH.text str
  DateTime { start, end } -> HH.text "datettime"
  ClockW clock -> HH.text "clock"
  DiaryW diary -> HH.text "diary"
  FootnoteRef { label, def } -> HH.text $ "fn-" <> label
  Break -> HH.br_
  JoinW wordsA wordsB -> HH.span [] [ renderWords wordsA, renderWords wordsB ]