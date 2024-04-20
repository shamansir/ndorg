module App.Keys where

import Prelude


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))


import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE


data Modifier
    = Shift
    | Command
    | Option


data Key
    = Alpha Char
    | Num Int
    | Special Int String


data Combo
    = Single Key String
    | WithModifier Modifier Combo
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


code :: Int -> String -> String -> Combo
code n = Single <<< Special n


mod :: Modifier -> Combo -> Combo
mod = WithModifier


data Match
    = None
    | Exact Combo
    | Wait Combo


type KeyEvent =
    { key :: Key
    , modifier :: Maybe Modifier
    }


matches :: Array Combo -> KeyEvent -> Match
matches _ _ = None
