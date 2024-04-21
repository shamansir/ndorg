module App.Keys where

import Prelude

import Data.Set (Set)
import Data.Set (isEmpty, member) as Set
import Data.Maybe (Maybe(..))
import Data.Array (foldr) as Array
import Data.Tuple.Nested ((/\), type (/\))


import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE


data Modifier
    = Shift
    | Command
    | Control
    | Option


derive instance Eq Modifier
derive instance Ord Modifier


data Key
    = Alpha Char
    | Num Int
    | Special Int String -- for special keys like arrows, string is the representation, int is code


derive instance Eq Key


data Combo
    = Single Key String
    | WithModifier Modifier Key String
    | Sequence Combo (Array Combo)


seq :: Combo -> Array Combo -> Combo
seq = Sequence


shift :: Modifier
shift = Shift


cmd :: Modifier
cmd = Command


opt :: Modifier
opt = Option


char :: Char -> String -> Combo
char = Single <<< Alpha


kcode :: Int -> String -> Key
kcode = Special


code :: Int -> String -> String -> Combo
code n = Single <<< Special n


mod :: Modifier -> Key -> String -> Combo
mod = WithModifier


data Match
    = None
    | Exact Combo
    | Wait Combo (Array Combo)


type KeyEvent =
    { key :: Key
    , modifiers :: Set Modifier
    }


matches :: Array Combo -> KeyEvent -> Match
matches combos kevt = Array.foldr checkCombo None combos
    where
        checkCombo :: Combo -> Match -> Match
        checkCombo combo None =
            case combo of
                Single key _ ->
                    if Set.isEmpty kevt.modifiers && key == kevt.key then Exact combo
                    else None
                WithModifier mod key _ ->
                    if Set.member mod kevt.modifiers then
                        if key == kevt.key then Exact combo else None
                    else None
                Sequence current next ->
                    case checkCombo current None of
                        None -> None
                        Exact _ -> Wait current next
                        Wait _ _ -> None -- FIXME: shouldn't be the case?
        checkCombo _ prev = prev
