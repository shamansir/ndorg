module App.State where

import Prelude

import Data.Array.NonEmpty ((:))
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..))


import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Path (Path(..))
import Data.Text.Format.Org.Path (root) as Path
import Data.Text.Format.Org.Construct (empty) as Org

import App.Action (Action)
import App.Action (Action(..), topLevel, headingSelected) as Action
import App.Keys (Combo(..), Key(..), KeyEvent, Modifier(..))
import App.Keys (shift, opt, cmd) as Mod
import App.Keys (char, code, num, mod, seq, kcode) as Combo


data Context
    = TopLevel
    | InSection
    | InBlock
    -- TODO | Planning
    | InCombo (Combo Action) (Array (Combo Action))
    | EditingWords


-- TODO: undo history


type State =
    { context :: Context
    , focus :: Path Int
    , file :: OrgFile
    , curKey :: Maybe KeyEvent
    }


init :: State
init =
    { context : TopLevel
    , focus : Path.root
    , file : Org.empty
    , curKey : Nothing
    }


suggestions :: Context -> Array (Combo Action)
suggestions = case _ of
    TopLevel -> Action.topLevel
    InSection -> Action.headingSelected
    InCombo _ combos -> combos
    _ -> []