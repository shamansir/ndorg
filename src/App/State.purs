module App.State where

import Prelude

import Data.Array.NonEmpty ((:))
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..))


import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Path (Path(..))
import Data.Text.Format.Org.Construct (empty) as Org

import App.Action (Action(..))
import App.Keys (Combo(..), Key(..), KeyEvent, Modifier(..))
import App.Keys (shift, opt, cmd) as Mod
import App.Keys (char, code, mod, seq, kcode) as Combo


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
    , focus : Root
    , file : Org.empty
    , curKey : Nothing
    }


suggestions :: Context -> Array (Combo Action)
suggestions = case _ of
    TopLevel -> topLevel
    InSection -> headingSelected
    InCombo _ combos -> combos
    _ -> []



-- TODO: group combos


topLevel :: Array (Combo Action)
topLevel =
    [ Combo.seq (Combo.char 'a' "add..." None)
        [ Combo.char 'v' "property" None
        , Combo.char 'p' "paragraph" None
        , Combo.char 'h' "heading" AddHeading
        , Combo.char 't' "task" None
        , Combo.char 'i' "item" None
        ]
    , Combo.char 'g' "agenda" None
    , Combo.char 't' "set title" None
    ]


headingSelected :: Array (Combo Action)
headingSelected =
    [ Combo.char 'e' "edit heading" None
    , Combo.code 45 "->" "level increase" None
    , Combo.code 45 "<-" "level decrease" None -- If not at the top
    , Combo.code 43 "" "move down" None
    , Combo.code 41 "" "move up" None
    , Combo.mod Mod.shift (Combo.kcode 45 "->") "cycle todo level fwd" None
    , Combo.mod Mod.shift (Combo.kcode 42 "<-") "cycle todo level back" None
    , Combo.mod Mod.shift (Combo.kcode 45 "") "priority up" None
    , Combo.mod Mod.shift (Combo.kcode 42 "") "priority down" None
    , Combo.char 's' "schedule" None
    , Combo.char 'd' "deadline" None
    , Combo.char 'r' "repeat" None
    , Combo.char 't' "timestamp" None
    -- TODO: sequence to edit todo manually
    -- TODO: sequence to edit priority manually
    ]