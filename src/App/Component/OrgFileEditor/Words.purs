module App.Component.OrgFileEditor.Words where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEA

import Data.Text.Format.Org.Types (Words(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


type Slot = H.Slot Query Output


_words = Proxy :: _ "words"


type Input =
    { words :: NonEmptyArray Words
    }


data Output =
    Output


data Query a
  = GetEditing (Boolean -> a)
  | SetEditing Boolean a


type State =
    { words :: NonEmptyArray Words
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
  initialState { words } = { words, editing: false }

  render :: State -> H.ComponentHTML Action () m
  render { words, editing } =
    HH.div
      [ HE.onClick \_ -> Action ]
      [ HH.text $ "Words: " <> "(" <> (if editing then "on" else "off") <> ")"
      , renderWordsNEA words
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
    SetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)
    GetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))


renderWordsNEA :: forall action slots m. NonEmptyArray Words -> H.ComponentHTML action slots m
renderWordsNEA = HH.div [] <<< NEA.toArray <<< map renderWords


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
  Break -> HH.text "<br>"
  JoinW wordsA wordsB -> HH.span [] [ renderWords wordsA, renderWords wordsB ]