module App.Component.OrgFileEditor.Planning where

import Prelude

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..), isJust)
import Data.Date (day, month, year, weekday) as Date
import Data.Time (hour, minute) as Time
-- import Data.Date.Component
import Data.Enum (fromEnum)
import Data.String (take) as String

import Data.Text.Format.Org.Types (Planning(..), OrgDateTime(..), OrgTimeRange(..), Repeater(..), RepeaterMode(..), Interval(..), Delay(..), DelayMode(..))
-- import Data.Text.Format.Org.Types (Keywords)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import App.Utils (cn, cn_editing)
import App.Utils (none) as HH


type Slot = H.Slot Query Output


_planning = Proxy :: _ "planning"


type Input =
    { planning :: Planning
    }


data Output =
    Output


data Query a
  = PlanningGetEditing (Boolean -> a)
  | PlanningSetEditing Boolean a


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
      [ HE.onClick \_ -> Action
      , HP.classes [ cn_editing editing, cn "ndorg-planning" ]
      ]
      [ renderPlanning planning
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
    PlanningSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)

    PlanningGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))


renderPlanning :: forall action slots m. Planning -> H.ComponentHTML action slots m
renderPlanning = case _ of
  Planning { closed, deadline, scheduled, timestamp } ->
    HH.div [ HP.class_ $ cn "ndorg-planning" ]
        $ case closed of
            Just closedTime -> [ qitem "closed" closedTime ]
            Nothing -> []
        <> case deadline of
            Just deadlineTime -> [ qitem "deadline" deadlineTime ]
            Nothing -> []
        <> case scheduled of
            Just scheduledTime -> [ qitem "scheduled" scheduledTime ]
            Nothing -> []
        <> case timestamp of
            Just timestampTime -> [ qitem "timestamp" timestampTime ]
            Nothing -> []
    where
      qitem label datetime = HH.div [ HP.class_ $ cn "ndorg-planning-item" ] [ qlabel label, renderDateTime datetime ]
      qlabel label = HH.span [ HP.class_ $ cn "ndorg-planning-label" ] [ HH.text label ]


renderDateTime :: forall action slots m. OrgDateTime -> H.ComponentHTML action slots m
renderDateTime = case _ of
  OrgDateTime def ->
    HH.span [ HP.class_ $ cn $ if def.active then "ndorg-datetime" else "ndorg-datetime-inactive" ]
      $
      [ HH.text $ show $ fromEnum $ Date.year def.date
      , qsep "-"
      , HH.text $ leadingZero $ fromEnum $ Date.month def.date
      , qsep "-"
      , HH.text $ leadingZero $ fromEnum $ Date.day def.date
      , HH.text " "
      , HH.text $ String.take 3 $ show $ Date.weekday def.date
      ]

      <> case def.time of
        Just (OrgTimeRange range) ->
          [ HH.text " "
          , HH.text $ leadingZero $ fromEnum $ Time.hour range.start
          , qsep ":"
          , HH.text $ leadingZero $ fromEnum $ Time.minute range.start
          ]
          <> case range.end of
            Just endTime ->
              [ qsep "--"
              , HH.text $ leadingZero $ fromEnum $ Time.hour endTime
              , qsep ":"
              , HH.text $ leadingZero $ fromEnum $ Time.minute endTime
              ]
            Nothing -> []
        Nothing -> []

       <> case def.repeat of
        Just (Repeater rep) ->
          [ HH.text " "
          , case rep.mode of
            Single -> qsep "+"
            FromToday ->  qsep "++"
            Jump ->  qsep ".+"
          , HH.text $ valueAndInterval rep.value rep.interval
          ]
          <> case rep.with of
            Just with ->
              [ HH.text " ", HH.text $ valueAndInterval with.value with.interval ]
            Nothing -> []
        Nothing -> []

       <> case def.delay of
        Just (Delay del) ->
          [ HH.text " "
          , case del.mode of
            One -> qsep "-"
            All ->  qsep "--"
          , HH.text $ valueAndInterval del.value del.interval
          ]
        Nothing -> []

  where
    valueAndInterval val int = show val <> case int of
      Hour -> "h"
      Day -> "d"
      Week -> "w"
      Month -> "m"
      Year -> "y"
    qsep sep = HH.span [ HP.class_ $ cn "ndorg-time-sep" ] [ HH.text sep ]
    leadingZero n | n < 10 = "0" <> show n
    leadingZero n | otherwise = show n


hasPlanning :: Planning -> Boolean -- FIXME: move to the `org` package
hasPlanning = case _ of
    Planning { closed, deadline, scheduled, timestamp } ->
        isJust closed || isJust deadline || isJust scheduled || isJust timestamp