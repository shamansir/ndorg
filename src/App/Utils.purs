module App.Utils where

import Debug as Debug

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (wrap) as NT
import Data.Set as Set
import Data.Int as Int
import Data.String as String
import Data.String.CodeUnits as CU
import Data.CodePoint.Unicode as CP
import Data.Tuple.Nested ((/\), type (/\))

import Web.HTML.Common (ClassName)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Halogen.HTML.Core (HTML)
import Halogen.HTML as HH

import App.Keys (Key(..), Modifier(..), KeyEvent)


cn :: String -> ClassName
cn = NT.wrap


cn_editing :: Boolean -> ClassName
cn_editing v = if v then cn "ndorg-editing" else cn "ndorg-not-editing"


none :: forall w i. HTML w i
none = HH.text ""


extract :: KE.KeyboardEvent -> Maybe KeyEvent
extract kevt =
    let

        maybeKey =
            let
                keyStr = KE.key kevt
                mbChar = CU.toChar keyStr
                mbCp = mbChar <#> String.codePointFromChar
            in
                mbCp >>= \cp ->
                    if CP.isAlpha cp then
                        mbChar <#> Alpha
                    else
                        let mbNum = Int.fromString keyStr <#> Num
                        in
                            if isJust mbNum then mbNum else Just $ Special 0 keyStr

        modifiers =
            Set.empty
            # \ms -> if KE.shiftKey kevt then ms # Set.insert Shift else ms
            # \ms -> if KE.altKey kevt then ms # Set.insert Option else ms
            # \ms -> if KE.ctrlKey kevt then ms # Set.insert Control else ms
            # \ms -> if KE.metaKey kevt then ms # Set.insert Command else ms

    in
        maybeKey <#>
            \key ->
                { key
                , modifiers
                }