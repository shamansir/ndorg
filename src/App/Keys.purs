module App.Keys where

import Prelude

import Debug as Debug

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


instance Eq Key where
    eq (Alpha ch) (Num n) =
        case charOf n of
            Just nch -> ch == nch
            Nothing -> false
    eq (Num n) (Alpha ch) =
        case charOf n of
            Just nch -> ch == nch
            Nothing -> false
    eq (Alpha chA) (Alpha chB) = chA == chB
    eq (Num nA) (Num nB) = nA == nB
    eq (Special nA strA) (Special nB strB) = nA == nB && strA == strB
    eq _ _ = false


charOf :: Int -> Maybe Char
charOf = case _ of
    0 -> Just '0'
    1 -> Just '1'
    2 -> Just '2'
    3 -> Just '3'
    4 -> Just '4'
    5 -> Just '5'
    6 -> Just '6'
    7 -> Just '7'
    8 -> Just '8'
    9 -> Just '9'
    _ -> Nothing


data Combo a
    = Single Key String a
    | WithModifier Modifier Key String a
    | Sequence (Combo a) (Array (Combo a))


get :: forall a. Combo a -> a
get = case _ of
    Single _ _ a -> a
    WithModifier _ _ _ a -> a
    Sequence combo _ -> get combo


seq :: forall a. Combo a -> Array (Combo a) -> Combo a
seq = Sequence


shift :: Modifier
shift = Shift


cmd :: Modifier
cmd = Command


opt :: Modifier
opt = Option


char :: forall a. Char -> String -> a -> Combo a
char = Single <<< Alpha


num :: forall a. Int -> String -> a -> Combo a
num = Single <<< Num


kcode :: Int -> String -> Key
kcode = Special


code :: forall a. Int -> String -> String -> a -> Combo a
code n = Single <<< Special n


mod :: forall a. Modifier -> Key -> String -> a -> Combo a
mod = WithModifier


data Match a
    = None
    | Exact (Combo a)
    | Wait (Combo a) (Array (Combo a))


type KeyEvent =
    { key :: Key
    , modifiers :: Set Modifier
    }


matches :: forall a. Array (Combo a) -> KeyEvent -> (Match a)
matches combos kevt = Array.foldr checkCombo None combos
    where
        checkCombo :: Combo a -> Match a -> Match a
        checkCombo combo None =
            case Debug.spy "check" combo of
                Single key _ _ ->
                    if Set.isEmpty kevt.modifiers && key == kevt.key then Exact combo
                    else None
                WithModifier mod key _ _ ->
                    if Set.member mod kevt.modifiers then
                        if key == kevt.key then Exact combo else None
                    else None
                Sequence current next ->
                    case Debug.spy "seq" $ checkCombo current None of
                        None -> None
                        Exact _ -> Wait current next
                        Wait _ _ -> None -- FIXME: shouldn't be the case?
        checkCombo _ prev = prev
