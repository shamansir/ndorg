module App.Component.OrgFileEditor.Planning where

import Prelude

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..), isJust)

import Data.Text.Format.Org.Types (Planning(..), OrgDateTime(..))
-- import Data.Text.Format.Org.Types (Keywords)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


type Slot = H.Slot Query Output


_planning = Proxy :: _ "planning"


type Input =
    { planning :: Planning
    }


data Output =
    Output


data Query a
  = GetEditing (Boolean -> a)
  | SetEditing Boolean a


type State =
    { planning :: Planning
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
  initialState { planning } = { planning, editing: false }

  render :: State -> H.ComponentHTML Action () m
  render { planning, editing } =
    HH.div
      [ HE.onClick \_ -> Action ]
      [ HH.text $ "Keywords: " <> " (" <> (if editing then "on" else "off") <> ")"
      , renderPlanning planning
      ]

  handleAction
    :: Action
    -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Receive input ->
      H.modify_ _ { planning = input.planning }

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


renderPlanning :: forall action slots m. Planning -> H.ComponentHTML action slots m
renderPlanning = case _ of
  Planning { closed, deadline, scheduled, timestamp } ->
    HH.div []
        [ case closed of
            Just closedTime -> renderDateTime closedTime
            Nothing -> HH.text ""
        , case deadline of
            Just deadlineTime -> renderDateTime deadlineTime
            Nothing -> HH.text ""
        , case scheduled of
            Just scheduledTime -> renderDateTime scheduledTime
            Nothing -> HH.text ""
        , case timestamp of
            Just timestampTime -> renderDateTime timestampTime
            Nothing -> HH.text ""
        ]


renderDateTime :: forall action slots m. OrgDateTime -> H.ComponentHTML action slots m
renderDateTime = case _ of
  _ ->
    HH.div []
      [ HH.text "datettime"
      ]


hasPlanning :: Planning -> Boolean -- FIXME: move to the `org` package
hasPlanning = case _ of
    Planning { closed, deadline, scheduled, timestamp } ->
        isJust closed || isJust deadline || isJust scheduled || isJust timestamp