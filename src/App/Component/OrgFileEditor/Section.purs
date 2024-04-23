module App.Component.OrgFileEditor.Section where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))

import Data.Text.Format.Org.Types (Section)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


type Slot = H.Slot Query Output


_section = Proxy :: _ "section"


type Input =
    { section :: Section
    }


data Output =
    Output


data Query a
  = GetEditing (Boolean -> a)
  | SetEditing Boolean a


type State =
    { section :: Section
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
  initialState { section } = { section, editing: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: State -> H.ComponentHTML Action () m
  render { section, editing } =
    HH.h1
      [ HE.onClick \_ -> Action ]
      [ HH.text $ {- label <> -} " (" <> (if editing then "on" else "off") <> ")" ]

  handleAction
    :: Action
    -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    Receive input ->
      H.modify_ _ { section = input.section }

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
