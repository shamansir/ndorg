module App.Component.Combo where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Traversable (traverse)
import Data.String as String
import Data.String.CodeUnits (singleton) as String

import App.Keys (Key(..), Modifier(..), Combo(..))
import App.Utils (cn)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


data State
    = FirstKey
    | WaitingSecondKey


render :: forall w i. Combo -> HH.HTML w i
render c =
    HH.div
        [ HP.class_ $ cn "combo" ]
        $ case c of
            Single ckey def ->
                [ key ckey
                , HH.div
                    [ HP.class_ $ cn "def" ]
                    [ HH.text def ]
                ]
            WithModifier mod combo ->
                [ modifier mod
                , render combo
                ]
            Sequence root _ ->
                [ render root
                , HH.div
                    [ HP.class_ $ cn "def" ]
                    [ HH.text $ "..." ]
                ]


key :: forall w i. Key -> HH.HTML w i
key k =
    HH.div
        [ HP.class_ $ cn "key" ]
        $ case k of
            Alpha char -> [ HH.span_ [ HH.text $ String.singleton char ] ]
            Num num -> [ HH.span_ [ HH.text $ show num ] ]
            Special n str ->
                [ HH.span_ [ HH.text $ show n ]
                , HH.span_ [ HH.text str ]
                ]


modifier :: forall w i. Modifier -> HH.HTML w i
modifier m =
    HH.div
        [ HP.class_ $ cn "mod" ]
        $ case m of
            Shift -> [ HH.span_ [ HH.text "Shift" ] ]
            Option -> [ HH.span_ [ HH.text "Option" ] ]
            Command -> [ HH.span_ [ HH.text "Command" ] ]