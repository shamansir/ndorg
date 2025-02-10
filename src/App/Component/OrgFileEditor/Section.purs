module App.Component.OrgFileEditor.Section where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))

import Data.FunctorWithIndex (mapWithIndex)

import Data.Text.Format.Org.Types (Section(..), OrgDoc(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import App.Component.OrgFileEditor.Block as BlockC


type SecSlot = H.Slot SecQuery SecOutput


_section = Proxy :: _ "section"


type SecInput =
    { section :: Section
    }


data SecOutput =
    SecOutput


data SecQuery a
  = SecGetEditing (Boolean -> a)
  | SecSetEditing Boolean a


type SecState =
    { section :: Section
    , editing :: Boolean
    }


data SecAction
    = SecAction
    | SecReceive SecInput
    | HandleDoc DocOutput


type SecSlots =
    ( doc :: DocSlot Int
    )


secComponent :: forall m. H.Component SecQuery SecInput SecOutput m
secComponent = H.mkComponent
    { initialState
    , render
      -- This component can handle internal actions, handle queries sent by a
      -- parent component, and update when it receives new input.
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< SecReceive
        }
    }
  where
  initialState :: SecInput -> SecState
  initialState { section } = { section, editing: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: SecState -> H.ComponentHTML SecAction SecSlots m
  render { section, editing } =
    HH.h1
      [ HE.onClick \_ -> SecAction ]
      [ HH.text $ "Section: " <> " (" <> (if editing then "on" else "off") <> ")"
      , case section of
        Section { doc } -> HH.slot _doc 0 docComponent { doc } HandleDoc
      -- HH.slot KeywordsC._keywords 0 KeywordsC.component { keywords : meta } HandleMeta
      ]

  handleAction
    :: SecAction
    -> H.HalogenM SecState SecAction SecSlots SecOutput m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    SecReceive input ->
      H.modify_ _ { section = input.section }

    -- When the button is clicked we update our `enabled` field in state, and
    -- we notify our parent component that the `Clicked` event happened.
    HandleDoc _ -> pure unit
    SecAction -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise SecOutput

  handleQuery
    :: forall a
     . SecQuery a
    -> H.HalogenM SecState SecAction SecSlots SecOutput m (Maybe a)
  handleQuery = case _ of
    -- When we receive a the tell-style `SetEnabled` query with a boolean, we
    -- set that value in state.
    SecSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)

    -- When we receive a the request-style `GetEnabled` query, which requires
    -- a boolean result, we get a boolean from our state and reply with it.
    SecGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))



_doc = Proxy :: _ "doc"


type DocSlot = H.Slot DocQuery DocOutput


type DocInput =
    { doc :: OrgDoc
    }


data DocOutput =
    DocOutput


data DocQuery a
  = DocGetEditing (Boolean -> a)
  | DocSetEditing Boolean a


type DocState =
    { doc :: OrgDoc
    , editing :: Boolean
    }


type DocSlots =
    ( blocks :: BlockC.Slot Int
    , section :: SecSlot Int
    )


data DocAction
    = DocAction
    | DocReceive DocInput
    | HandleSection Int SecOutput


docComponent :: forall m. H.Component DocQuery DocInput DocOutput m
docComponent = H.mkComponent
    { initialState
    , render
      -- This component can handle internal actions, handle queries sent by a
      -- parent component, and update when it receives new input.
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< DocReceive
        }
    }
  where
  initialState :: DocInput -> DocState
  initialState { doc } = { doc, editing: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: DocState -> H.ComponentHTML DocAction DocSlots m
  render { doc, editing } =
    HH.h1
      [ HE.onClick \_ -> DocAction ]
      (
      [ HH.text $ "Doc: " <> " (" <> (if editing then "on" else "off") <> ")"
      ]
      <> case doc of
        OrgDoc { zeroth, sections } ->
          mapWithIndex (\idx section -> HH.slot _section idx secComponent { section } $ HandleSection idx) sections
          -- HH.slot KeywordsC._keywords 0 KeywordsC.component { keywords : meta } HandleMeta
      )

  handleAction
    :: DocAction
    -> H.HalogenM DocState DocAction DocSlots DocOutput m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    DocReceive input ->
      H.modify_ _ { doc = input.doc }

    -- When the button is clicked we update our `enabled` field in state, and
    -- we notify our parent component that the `Clicked` event happened.
    DocAction -> do
      H.modify_ \state -> state { editing = not state.editing }
      H.raise DocOutput

    HandleSection _ _ ->
      pure unit

  handleQuery
    :: forall a
     . DocQuery a
    -> H.HalogenM DocState DocAction DocSlots DocOutput m (Maybe a)
  handleQuery = case _ of
    -- When we receive a the tell-style `SetEnabled` query with a boolean, we
    -- set that value in state.
    DocSetEditing value next -> do
      H.modify_ _ { editing = value }
      pure (Just next)

    -- When we receive a the request-style `GetEnabled` query, which requires
    -- a boolean result, we get a boolean from our state and reply with it.
    DocGetEditing reply -> do
      editing <- H.gets _.editing
      pure (Just (reply editing))
