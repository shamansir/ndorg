module App.Action where

import Prelude


import Halogen as H
import Web.UIEvent.KeyboardEvent as KE

import App.Keys (Combo(..), Key(..), KeyEvent, Modifier(..))
import App.Keys (shift, opt, cmd) as Mod
import App.Keys (char, code, num, mod, seq, kcode) as Combo


data Action
  = Initialize
  | None
  | Increment
  | HandleKeyDown H.SubscriptionId KE.KeyboardEvent
  | HandleKeyUp H.SubscriptionId KE.KeyboardEvent
  | WaitForCombos (Combo Action) (Array (Combo Action))
  | ClearCombo
  | AddHeading
  | AddProperty
  | LoadExample ExampleId


data ExampleId
    = Ex01
    | Ex02a
    | Ex02b
    | Ex03a
    | Ex03b
    | Ex03c
    | Ex03d
    | Ex03e
    | Ex04a
    | Ex04b
    | Ex04c
    | Ex04d
    | Ex04e
    | Ex04f
    | Ex04g
    | Ex04h
    | Ex04i
    | Ex18


-- TODO: group combos


topLevel :: Array (Combo Action)
topLevel =
    [ Combo.seq (Combo.char 'a' "add..." None)
        [ Combo.char 'v' "property" None
        , Combo.char 'p' "paragraph" None
        , Combo.char 'h' "heading" AddHeading
        , Combo.char 't' "task" None
        , Combo.char 'i' "item" None
        , Combo.char 'x' "back" ClearCombo
        ]
    , Combo.char 'g' "agenda" None
    , Combo.char 't' "set title" None
    , Combo.seq (Combo.char 'f' "select example" None)
        [ Combo.num 1 (explain Ex01) $ LoadExample Ex01
        , Combo.seq (Combo.num 2 "group 2: meta" None)
          $ exampleToChCombo <$> group2
        , Combo.seq (Combo.num 3 "group 3 : headings+" None)
          $ exampleToChCombo <$> group3
        , Combo.seq (Combo.num 4 "group 4 : formatting" None)
          $ exampleToChCombo <$> group4
        ]
    ]
    where
      exampleToChCombo ex = Combo.char (charInGroup ex) (explain ex) $ LoadExample ex
      group2 = [ Ex02a, Ex02b ]
      group3 = [ Ex03a, Ex03b, Ex03c, Ex03d, Ex03e ]
      group4 = [ Ex04a, Ex04b, Ex04c, Ex04d, Ex04e, Ex04f, Ex04g, Ex04h, Ex04i ]


charInGroup :: ExampleId -> Char
charInGroup =
  case _ of
      Ex01 -> 'a'
      Ex02a -> 'a'
      Ex02b -> 'b'
      Ex03a -> 'a'
      Ex03b -> 'b'
      Ex03c -> 'c'
      Ex03d -> 'd'
      Ex03e -> 'e'
      Ex04a -> 'a'
      Ex04b -> 'b'
      Ex04c -> 'c'
      Ex04d -> 'd'
      Ex04e -> 'e'
      Ex04f -> 'f'
      Ex04g -> 'g'
      Ex04h -> 'h'
      Ex04i -> 'i'
      Ex18 -> 'a'


explain :: ExampleId -> String
explain = case _ of
    Ex01 -> "empty"
    Ex02a -> "meta"
    Ex02b -> "meta-special"
    Ex03a -> "headings"
    Ex03b -> "headings w/content"
    Ex03c -> "headings w/planning"
    Ex03d -> "headings w/tags"
    Ex03e -> "basic structure"
    Ex04a -> "formatting headings"
    Ex04b -> "formatting blocks"
    Ex04c -> "formatting lists"
    Ex04d -> "formatting tables"
    Ex04e -> "formatting footnotes"
    Ex04f -> "formatting comments"
    Ex04g -> "formatting dates"
    Ex04h -> "formatting props and keywrds"
    Ex04i -> "formatting drawers"
    Ex18 -> "properties & drawers"



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