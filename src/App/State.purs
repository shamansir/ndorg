module App.State where

import Prelude

import Data.Array.NonEmpty ((:))
import Data.Array.NonEmpty as NE


import Data.Text.Format.Org.Types (OrgFile)
import Data.Text.Format.Org.Construct (empty) as Org

import App.Keys (Combo(..), Key(..), Modifier(..))
import App.Keys (shift, opt, cmd) as Mod
import App.Keys (char, code, mod, seq, kcode) as Combo


data Context
    = TopLevel
    | InSection
    | InBlock
    -- TODO | Planning
    | InCombo Combo
    | EditingWords


data PathItem =
    Top


data Path
    = Nowhere
    | Block Int
    | Section Int
    | Property Int
    | Word Int
    | Deeper Path Path


data Focus = Focus Path


-- TODO: undo history


type State =
    { context :: Context
    , focus :: Focus
    , file :: OrgFile
    }


init :: State
init =
    { context : TopLevel
    , focus : Focus Nowhere
    , file : Org.empty
    }


suggestions :: Context -> Array Combo
suggestions = case _ of
    TopLevel -> topLevel
    InSection -> headingSelected
    _ -> []



-- TODO: group combos


topLevel :: Array Combo
topLevel =
    [ Combo.seq (Combo.char 'a' "add...")
        [ Combo.char 'v' "property"
        , Combo.char 'p' "paragraph"
        , Combo.char 'h' "heading"
        , Combo.char 't' "task"
        , Combo.char 'i' "item"
        ]
    , Combo.char 'g' "agenda"
    ]


headingSelected :: Array Combo
headingSelected =
    [ Combo.char 'e' "edit heading"
    , Combo.code 45 "->" "level deep"
    , Combo.code 45 "<-" "level higher" -- If not at the top
    , Combo.code 43 "" "move down"
    , Combo.code 41 "" "move up"
    , Combo.mod Mod.shift (Combo.kcode 45 "->") "cycle todo level fwd"
    , Combo.mod Mod.shift (Combo.kcode 42 "<-") "cycle todo level back"
    , Combo.mod Mod.shift (Combo.kcode 45 "") "priority up"
    , Combo.mod Mod.shift (Combo.kcode 42 "") "priority down"
    , Combo.char 's' "schedule"
    , Combo.char 'd' "deadline"
    , Combo.char 'r' "repeat"
    , Combo.char 't' "timestamp"
    -- TODO: sequence to edit todo manually
    -- TODO: sequence to edit priority manually
    ]