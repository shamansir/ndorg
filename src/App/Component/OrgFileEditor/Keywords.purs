module App.Component.OrgFileEditor.Keywords where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))

import Data.Text.Format.Org.Keywords (Keywords(..), Keyword(..))
-- import Data.Text.Format.Org.Types (Keywords)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import App.Utils (cn, cn_editing)
import App.Utils (none) as HH


type Slot = H.Slot Query Output


_keywords = Proxy :: _ "keywords"


type Input =
    { keywords :: Keywords String
    }


data Output =
    Output


data Query a
  = GetEditing (Boolean -> a)
  | SetEditing Boolean a


type State =
    { keywords :: Keywords String
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
  initialState { keywords } = { keywords, editing: false }

  render :: State -> H.ComponentHTML Action () m
  render { keywords, editing } =
    HH.div
      [ HE.onClick \_ -> Action
      , HP.classes [ cn_editing editing, cn "ndorg-keywords" ]
      ]
      [ renderKeywords keywords
      ]

  handleAction
    :: Action
    -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Receive input ->
      H.modify_ _ { keywords = input.keywords }

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


renderKeywords :: forall action slots m. Keywords String -> H.ComponentHTML action slots m
renderKeywords = case _ of
  Keywords keywords -> HH.div [] $ renderKeyword <$> keywords


renderKeyword :: forall action slots m. Keyword String -> H.ComponentHTML action slots m
renderKeyword = case _ of
  Keyword { name, value, default } ->
    HH.div []
      [ HH.span [] [ HH.text name ]
      , HH.span [] [ case value of
        Just val -> HH.text val
        Nothing ->  HH.none ]
      , HH.span [] [ case default of
        Just def -> HH.text $ def
        Nothing -> HH.none ]
      ]