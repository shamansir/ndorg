module App.Component.OrgFileEditor.Keywords where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))

import Data.Text.Format.Org.Keywords (Keywords(..), Keyword(..))
-- import Data.Text.Format.Org.Types (Keywords)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


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
      -- This component can handle internal actions, handle queries sent by a
      -- parent component, and update when it receives new input.
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input -> State
  initialState { keywords } = { keywords, editing: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: State -> H.ComponentHTML Action () m
  render { keywords, editing } =
    HH.div
      [ HE.onClick \_ -> Action ]
      [ HH.text $ "Keywords: " <> " (" <> (if editing then "on" else "off") <> ")"
      , renderKeywords keywords
      ]

  handleAction
    :: Action
    -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    Receive input ->
      H.modify_ _ { keywords = input.keywords }

    -- When the button is clicked we update our `enabled` field in state, and
    -- we notify our parent component that the `Clicked` event happened.
    Action -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise Output

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    -- When we receive a the tell-style `SetEnabled` query with a boolean, we
    -- set that value in state.
    SetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)

    -- When we receive a the request-style `GetEnabled` query, which requires
    -- a boolean result, we get a boolean from our state and reply with it.
    GetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))


renderKeywords :: forall m. Keywords String -> H.ComponentHTML Action () m
renderKeywords = case _ of
  Keywords keywords -> HH.div [] $ renderKeyword <$> keywords


renderKeyword :: forall m. Keyword String -> H.ComponentHTML Action () m
renderKeyword = case _ of
  Keyword { name, value, default } ->
    HH.div []
      [ HH.span [] [ HH.text name ]
      , HH.span [] [ HH.text $ case value of
        Just val -> val
        Nothing -> "-" ]
      , HH.span [] [ HH.text $ case default of
        Just def -> def
        Nothing -> "-" ]
      ]