module App.Component.OrgFileEditor.Words where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Array ((:))
import Data.Array (singleton) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEA

import Data.Text.Format.Org.Types (Words(..))
import Data.Text.Format.Org.Types (MarkupKey(..)) as MK
import Data.Text.Format.Org.Types (InlineKey(..)) as IK
import Data.Text.Format.Org.Types (LinkTarget(..)) as LT
import Data.Text.Format.Org.Types (ImageSource(..)) as IS

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
  Marked how str -> renderMarkedTag how str
  Link trg mbstr ->
    qmkspan "link"
      $ HH.a
        [ HP.href $ targetToLink trg ]
        [ HH.text $ case mbstr of
          Just text -> text
          Nothing -> targetToLink trg
        ]
  Image imageSrc ->
    qmkspan "image"
      $ HH.img [ HP.src $ imageSrcToLink imageSrc ]
  Punct cp -> HH.text "codepoint"
  Plain plain -> HH.text plain
  Markup str -> HH.text str
  DateTime { start, end } -> HH.text "datettime"
  ClockW clock -> HH.text "clock"
  DiaryW diary -> HH.text "diary"
  FootnoteRef { label, def } -> HH.text $ "fn-" <> label
  Break -> HH.br_
  JoinW wordsA wordsB -> HH.span [] [ renderWords wordsA, renderWords wordsB ]
  where
    targetToLink = case _ of
      LT.Remote src -> src
      LT.Local src -> src
      LT.Heading src -> "#" <> src
    imageSrcToLink = case _ of
      IS.RemoteSrc src -> src
      IS.LocalSrc src -> src
    qmkspan class_ = HH.span [ HP.class_ $ cn $ "ndorg-markup-" <> class_ ] <<< Array.singleton
    {-
    wrapTag how =
      case how of
        MK.Bold         -> HH.b []    <<< Array.singleton
        MK.Italic       -> HH.i []    <<< Array.singleton
        MK.Underline    -> HH.u []    <<< Array.singleton
        MK.Strike       -> HH.del []  <<< Array.singleton
        MK.Verbatim     -> HH.code [] <<< Array.singleton
        MK.Highlight    -> HH.mark [] <<< Array.singleton
        MK.InlineCode   -> HH.code [] <<< Array.singleton
        MK.Inline MK.IComment -> HH.code [] <<< Array.singleton
        MK.Inline MK.IHtml -> HH.code [] <<< Array.singleton
        MK.Error        -> qmkspan "error" <<< Array.singleton
        MK.And mkA mkB  -> \inner -> wrapTag mkA $ wrapTag mkB inner
    -}
    wrapClassed how =
      case how of
        MK.Bold -> qmkspan "bold"
        MK.Italic -> qmkspan "italic"
        MK.Underline -> qmkspan "underline"
        MK.Strike -> qmkspan "strikethrough"
        MK.Verbatim -> qmkspan "verbatim"
        MK.Highlight -> qmkspan "highlight"
        MK.InlineCode -> qmkspan "inline-code"
        MK.Inline IK.IComment -> qmkspan "inline-comment"
        MK.Inline IK.IHtml -> qmkspan "inline-html"
        MK.Error -> qmkspan "error"
        MK.And mkA mkB -> \inner -> wrapClassed mkA $ wrapClassed mkB inner
    renderMarkedTag how str = wrapClassed how $ HH.text str
        -- _ -> HH.text $ "marked" <> str