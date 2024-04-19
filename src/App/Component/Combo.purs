module App.Component.Combo where

import Prelude

import App.Keys (Key(..), Combo(..))

import Halogen.HTML as HH


data State
    = FirstKey
    | WaitingSecondKey


render :: forall w i. Combo -> HH.HTML w i
render =
    case _ of
        Combo keys def -> HH.span [ ] [ HH.text def ]
        ModCombo mod combo -> HH.span [ ] [ HH.text "mod" ]
        Sequence comboA comboB -> HH.span [] [ HH.text "seq" ]


key :: forall w i. Key -> HH.HTML w i
key =
    case _ of
        Alpha char -> HH.span_ [ HH.text "c" ]
        Num num -> HH.span_ [ HH.text $ show num ]
        Special n str -> HH.span_ [ HH.text $ show n ]