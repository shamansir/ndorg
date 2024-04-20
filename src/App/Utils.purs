module App.Utils where

import Prelude


import Data.Newtype (wrap) as NT

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Web.HTML.Common (ClassName)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import App.Keys (Key(..), Modifier(..), KeyEvent)


cn :: String -> ClassName
cn = NT.wrap


extract :: KE.KeyboardEvent -> Maybe KeyEvent
extract ke = Nothing