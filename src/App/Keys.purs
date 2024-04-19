module App.Keys where

import Prelude


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
    = Combo (NonEmptyArray Key) String
    | ModCombo Modifier Combo
    | Sequence Combo Combo


seq :: Combo -> Combo -> Combo
seq = Sequence


shift :: Modifier
shift = Shift


cmd :: Modifier
cmd = Command


opt :: Modifier
opt = Option


char :: Char -> String -> Combo
char c = Combo $ NE.singleton $ Alpha c


code :: Int -> String -> String -> Combo
code n symbol = Combo $ NE.singleton $ Special n symbol


mod :: Modifier -> Combo -> Combo
mod = ModCombo